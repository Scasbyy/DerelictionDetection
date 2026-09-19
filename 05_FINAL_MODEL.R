# =============================================================================
#  FINAL VACANCY MODEL AND CLASSIFICATION  (v2)
#  Shea Casby, MSc Geoinformatics, University College Cork
#
#  Firth model -> validation -> city scoring -> quota-constrained tiers with a
#  CONDITIONAL boarded-up priority rule. Prints every number for the results
#  chapter and exports the ArcGIS join table.
#
#  Requires: dplyr, readr, readxl, logistf, pROC        Run time: ~5 min
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(readxl); library(logistf); library(pROC)
})

INPUT_FILE        <- "C:/Users/sheac/Documents/College/Dissertation/August/GeminiResults2.csv"
GROUND_TRUTH_FILE <- "C:/Users/sheac/Documents/College/Dissertation/Claude/GROUND_TRUTH_MASTER.csv"
METADATA_FILE     <- "C:/Users/sheac/Documents/College/Dissertation/August/P2_Gemini_rank.xls"
OUTPUT_FILE       <- "C:/Users/sheac/Documents/College/Dissertation/Claude/ArcGIS_Vacancy_Join_boost.csv"

# --- ANALYTICAL CHOICES ------------------------------------------------------
TARGET_GEO    <- 0.01705   # GeoDirectory Cork City Q3 2022
TARGET_CEN    <- 0.05555   # Census 2022, 5,061 of 91,107 dwellings
PRIORITY_IND  <- "boarded_up"   # the override fires on this indicator only
MAX_OCC       <- 2         # priority withdrawn at this many occupancy indicators
DROP_FOCONNOR <- TRUE      # is_from_foconnor p = 0.977

hdr <- function(x) cat("\n\n=========== ", x, " ===========\n")

# =============================================================================
# 1. LOAD AND PREPARE
# =============================================================================
city_data <- read_csv(INPUT_FILE, show_col_types = FALSE)
gt_data   <- read_csv(GROUND_TRUTH_FILE, show_col_types = FALSE)
bldg_meta <- read_excel(METADATA_FILE)

if ("FUNC_ID" %in% names(city_data)) city_data <- city_data %>% select(-FUNC_ID)
id_col <- intersect(c("Bldg_GUID","GUID","GEO_ID","GeoID"), names(bldg_meta))[1]
if (!is.na(id_col) && id_col != "Bldg_GUID")
  names(bldg_meta)[names(bldg_meta) == id_col] <- "Bldg_GUID"
city_data <- city_data %>%
  left_join(bldg_meta %>% select(Bldg_GUID, FUNC_ID) %>%
              distinct(Bldg_GUID, .keep_all = TRUE), by = "Bldg_GUID")

defence_5g   <- c("boarded_up","infilled_apertures")
struct_5g    <- c("smashed_glass","roof_damage","internal_debris",
                  "structural_vegetation","exposed_wiring")
neglect_cols <- c("facade_decay","overgrown_grounds","threshold_debris",
                  "graffiti","obsolete_signage")
interv_cols  <- c("fixed_grilles","external_padlocks","notices","for_sale_sign")
occ_cols     <- c("active_vehicles","domestic_items","waste_management",
                  "maintained_curtilage")
ALL_COLS <- unique(c(defence_5g, struct_5g, neglect_cols, interv_cols, occ_cols))

for (col in ALL_COLS) {
  if (col %in% names(city_data))
    city_data[[col]] <- as.numeric(ifelse(is.na(city_data[[col]]), 0, city_data[[col]]))
  ai <- paste0("AI_", col)
  if (ai %in% names(gt_data))
    gt_data[[ai]] <- as.numeric(ifelse(is.na(gt_data[[ai]]), 0, gt_data[[ai]]))
}
city_data$has_target_building <- as.integer(
  toupper(as.character(city_data$has_target_building)) %in% c("TRUE","T","1","YES","Y"))

rs <- function(d, cols) rowSums(as.data.frame(d[, cols, drop = FALSE]))

gt_data <- gt_data %>% filter(!is.na(GROUND_TRUTH))
gt_data$is_from_handpicked <- as.integer(gt_data$sample_source == "handpicked_v1")
gt_data$is_from_foconnor   <- as.integer(gt_data$sample_source == "FOConnor_vacant")
gt_data$Idx_Defence5 <- rs(gt_data, paste0("AI_", defence_5g))
gt_data$Idx_Struct5  <- rs(gt_data, paste0("AI_", struct_5g))
gt_data$Idx_Neglect  <- rs(gt_data, paste0("AI_", neglect_cols))
gt_data$Idx_Interv   <- rs(gt_data, paste0("AI_", interv_cols))
gt_data$Idx_Occ      <- rs(gt_data, paste0("AI_", occ_cols))
gt_data$Priority     <- as.integer(gt_data[[paste0("AI_", PRIORITY_IND)]] > 0 &
                                     gt_data$Idx_Occ < MAX_OCC)

# =============================================================================
# 2. MODEL
# =============================================================================
hdr("2. MODEL")
CTRL <- if (DROP_FOCONNOR) "is_from_handpicked" else "is_from_handpicked + is_from_foconnor"
f5 <- as.formula(paste("GROUND_TRUTH ~ Idx_Defence5 + Idx_Struct5 + Idx_Neglect +",
                       "Idx_Interv + Idx_Occ +", CTRL))
m5 <- logistf(f5, data = gt_data)
r5 <- roc(gt_data$GROUND_TRUTH, m5$predict, quiet = TRUE)
c5 <- coords(r5, "best", ret = c("threshold","specificity","sensitivity"), best.method = "youden")

cat(sprintf("n = %d (vacant %d, occupied %d)\n", nrow(gt_data),
            sum(gt_data$GROUND_TRUTH == 1), sum(gt_data$GROUND_TRUTH == 0)))
print(data.frame(term = names(coef(m5)), coef = round(coef(m5), 3),
                 lower = round(m5$ci.lower, 3), upper = round(m5$ci.upper, 3),
                 p = round(m5$prob, 4), row.names = NULL))
cat(sprintf("AUC %.4f | Youden %.1f%% | Sens %.1f%% | Spec %.1f%%\n",
            auc(r5), c5$threshold[1]*100, c5$sensitivity[1]*100, c5$specificity[1]*100))

# Five-fold stratified cross-validation
set.seed(42)
i0 <- sample(which(gt_data$GROUND_TRUTH == 0)); i1 <- sample(which(gt_data$GROUND_TRUTH == 1))
folds <- lapply(1:5, function(k) c(i0[seq(k, length(i0), 5)], i1[seq(k, length(i1), 5)]))
oof <- rep(NA_real_, nrow(gt_data))
for (f in folds) {
  mtr <- logistf(f5, data = gt_data[-f, ])
  oof[f] <- as.vector(1/(1 + exp(-(model.matrix(f5, gt_data[f, ]) %*% coef(mtr)))))
}
rcv <- roc(gt_data$GROUND_TRUTH, oof, quiet = TRUE)
cat(sprintf("Cross-validated AUC %.4f (optimism %.4f)\n", auc(rcv), auc(r5) - auc(rcv)))

# =============================================================================
# 3. EVIDENCE FOR THE PRIORITY RULE
# =============================================================================
hdr("3. EVIDENCE FOR THE PRIORITY RULE")
b <- gt_data[[paste0("AI_", PRIORITY_IND)]] > 0
cat(sprintf("%s against ground truth:\n", PRIORITY_IND))
print(table(boarded = b, truth = gt_data$GROUND_TRUTH))
cat(sprintf("Precision of boarded -> vacant: %.1f%% (%d of %d)\n",
            mean(gt_data$GROUND_TRUTH[b])*100, sum(gt_data$GROUND_TRUTH[b]), sum(b)))
cat("\nBoarded buildings split by occupancy load:\n")
print(gt_data %>% filter(b) %>%
        group_by(occ_band = ifelse(Idx_Occ < MAX_OCC,
                                   paste0("< ", MAX_OCC, " (priority)"),
                                   paste0(">= ", MAX_OCC, " (no priority)"))) %>%
        summarise(n = n(), pct_vacant = round(mean(GROUND_TRUTH)*100, 1), .groups = "drop"))
cat("\nNOTE: training sample is enriched with vacant cases. Report as an upper bound.\n")

# =============================================================================
# 4. CITY SCORING. Linear_Score is never modified.
# =============================================================================
hdr("4. CITY SCORING")
W <- coef(m5)
city_data <- city_data %>% mutate(
  Index_Defence       = rs(city_data, defence_5g),
  Index_Structural    = rs(city_data, struct_5g),
  Index_Neglect       = rs(city_data, neglect_cols),
  Index_Interventions = rs(city_data, interv_cols),
  Index_Occupancy     = rs(city_data, occ_cols))
city_data$Linear_Score <-
  city_data$Index_Defence * W["Idx_Defence5"] + city_data$Index_Structural * W["Idx_Struct5"] +
  city_data$Index_Neglect * W["Idx_Neglect"]  + city_data$Index_Interventions * W["Idx_Interv"] +
  city_data$Index_Occupancy * W["Idx_Occ"]

city_data$Priority <- as.integer(city_data[[PRIORITY_IND]] > 0 &
                                   city_data$Index_Occupancy < MAX_OCC)
city_data$is_outbuilding <- !is.na(city_data$FUNC_ID) &
  (tolower(trimws(city_data$FUNC_ID)) == "outbuilding" | trimws(city_data$FUNC_ID) == "325")

ev <- city_data %>% filter(has_target_building == 1)
cat(sprintf("Buildings %d | evaluable %d | occluded %d (%.2f%%) | outbuildings %d\n",
            nrow(city_data), nrow(ev), nrow(city_data) - nrow(ev),
            100*(1 - nrow(ev)/nrow(city_data)), sum(city_data$is_outbuilding, na.rm = TRUE)))
cat(sprintf("Boarded (evaluable) %d | of those, priority class %d | withdrawn %d\n",
            sum(ev[[PRIORITY_IND]] > 0), sum(ev$Priority == 1),
            sum(ev[[PRIORITY_IND]] > 0 & ev$Priority == 0)))
cat("\nBoarded buildings by number of occupancy indicators:\n")
print(ev %>% filter(.data[[PRIORITY_IND]] > 0) %>% count(Index_Occupancy))

# Calibrated probabilities, reported but not used to cut tiers
anchor <- function(s, t) uniroot(function(c0) mean(1/(1+exp(-(c0+s)))) - t,
                                 c(-40, 40), extendInt = "yes")$root
c_geo <- anchor(ev$Linear_Score, TARGET_GEO); c_cen <- anchor(ev$Linear_Score, TARGET_CEN)
city_data$Prob_Geo <- 1/(1 + exp(-(c_geo + city_data$Linear_Score)))
city_data$Prob_Cen <- 1/(1 + exp(-(c_cen + city_data$Linear_Score)))
cat(sprintf("\nCalibrated intercepts: Geo %.4f | Census %.4f\n", c_geo, c_cen))
cat(sprintf("Count above p=0.5: %d and %d, versus target counts %d and %d.\n",
            sum(city_data$Prob_Geo[city_data$has_target_building==1] >= .5),
            sum(city_data$Prob_Cen[city_data$has_target_building==1] >= .5),
            round(TARGET_GEO*nrow(ev)), round(TARGET_CEN*nrow(ev))))
cat("Tiers are therefore set by quota, not by a probability cutoff.\n")

# =============================================================================
# 5. TIER ASSIGNMENT
# =============================================================================
hdr("5. TIER ASSIGNMENT")

assign_tiers <- function(df, n1, n2cum) {
  n <- nrow(df); tier <- rep(3L, n)
  ord <- order(-df$Linear_Score)
  p <- which(df$Priority == 1); p <- p[order(-df$Linear_Score[p])]
  take <- head(p, min(length(p), n1)); tier[take] <- 1L
  slots <- n1 - length(take)
  if (slots > 0) tier[head(ord[tier[ord] != 1L], slots)] <- 1L
  n2 <- max(0, n2cum - n1)
  if (n2 > 0) tier[head(ord[tier[ord] != 1L], n2)] <- 2L
  tier
}

run <- function(df, label) {
  e <- df %>% filter(has_target_building == 1)
  n1 <- round(TARGET_GEO * nrow(e)); n2c <- round(TARGET_CEN * nrow(e))
  t <- assign_tiers(e, n1, n2c)
  bo <- e[[PRIORITY_IND]] > 0
  cat(sprintf("\n--- %s ---\n", label))
  cat(sprintf("Evaluable %d | quota T1 %d, T1+2 %d | delivered T1 %d, T2 %d\n",
              nrow(e), n1, n2c, sum(t == 1), sum(t == 2)))
  cat(sprintf("Tier 1: %d by priority + %d by score\n",
              sum(e$Priority == 1 & t == 1), sum(e$Priority == 0 & t == 1)))
  cat(sprintf("All priority buildings in Tier 1: %s\n",
              ifelse(sum(e$Priority == 1 & t == 1) == sum(e$Priority == 1), "YES", "NO")))
  cat(sprintf("Boarded with >= %d occupancy signs: %d -> T1 %d, T2 %d, T3 %d\n", MAX_OCC,
              sum(bo & e$Priority == 0), sum(bo & e$Priority == 0 & t == 1),
              sum(bo & e$Priority == 0 & t == 2), sum(bo & e$Priority == 0 & t == 3)))
  cat(sprintf("Lowest score admitted: %.2f by priority, %.2f by score\n",
              min(e$Linear_Score[e$Priority == 1 & t == 1]),
              min(e$Linear_Score[e$Priority == 0 & t == 1])))
  e %>% mutate(Tier = t) %>% select(Bldg_GUID, Tier)
}
tiers_all   <- run(city_data, "ALL BUILDINGS")
tiers_noout <- run(city_data %>% filter(!is_outbuilding), "EXCLUDING OUTBUILDINGS")

# =============================================================================
# 6. WHAT THE RULE CHANGED
# =============================================================================
hdr("6. EFFECT OF THE PRIORITY RULE")
n1 <- round(TARGET_GEO*nrow(ev)); n2c <- round(TARGET_CEN*nrow(ev))
o <- order(-ev$Linear_Score)
score_only <- rep(3L, nrow(ev)); score_only[head(o, n1)] <- 1L
score_only[head(o[score_only[o] != 1L], n2c - n1)] <- 2L
with_rule <- assign_tiers(ev, n1, n2c)
bo <- ev[[PRIORITY_IND]] > 0
cat(sprintf("Tier 1 by score alone   : %d, containing %d boarded\n",
            sum(score_only == 1), sum(score_only == 1 & bo)))
cat(sprintf("Tier 1 with priority    : %d, containing %d boarded\n",
            sum(with_rule == 1), sum(with_rule == 1 & bo)))
cat(sprintf("Boarded promoted        : %d\n", sum(with_rule == 1 & score_only != 1)))
cat(sprintf("Non-boarded displaced   : %d (scores %.2f to %.2f)\n",
            sum(score_only == 1 & with_rule != 1),
            min(ev$Linear_Score[score_only == 1 & with_rule != 1]),
            max(ev$Linear_Score[score_only == 1 & with_rule != 1])))

hdr("7. RANKED SHORTLIST")
rk <- ev %>% arrange(desc(Linear_Score))
for (n in c(100, 200, 500, n1)) {
  h <- head(rk, n)
  cat(sprintf("  Top %4d: min score %5.2f | %3d boarded | %3d with occupancy signs\n",
              n, min(h$Linear_Score), sum(h[[PRIORITY_IND]] > 0), sum(h$Index_Occupancy > 0)))
}

# =============================================================================
# 8. EXPORT
# =============================================================================
# hdr("8. EXPORT")
# LBL <- c("1 - Long-Term Vacant","2 - Transitional Vacant","3 - Occupied / Unflagged")
# out <- city_data %>%
#   left_join(tiers_all   %>% rename(TA = Tier), by = "Bldg_GUID") %>%
#   left_join(tiers_noout %>% rename(TN = Tier), by = "Bldg_GUID") %>%
#   mutate(
#     Vacancy_Tier_All = ifelse(has_target_building == 0,
#                               "4 - Unevaluable / Occluded", LBL[TA]),
#     Vacancy_Tier_NoOut = case_when(
#       is_outbuilding ~ "Excluded - Outbuilding",
#       has_target_building == 0 ~ "4 - Unevaluable / Occluded",
#       TRUE ~ LBL[TN]),
#     Priority_Admission = ifelse(Priority == 1 & has_target_building == 1, 1L, 0L),
#     City_Rank = rank(-Linear_Score, ties.method = "min")) %>%
#   select(Bldg_GUID, FUNC_ID, Linear_Score, City_Rank, Prob_Geo, Prob_Cen,
#          Priority_Admission, Vacancy_Tier_All, Vacancy_Tier_NoOut,
#          Index_Defence, Index_Structural, Index_Neglect, Index_Interventions,
#          Index_Occupancy)
# write_csv(out, OUTPUT_FILE)
# print(out %>% count(Vacancy_Tier_NoOut))
# cat(sprintf("\nExported %d rows.\n", nrow(out)))
