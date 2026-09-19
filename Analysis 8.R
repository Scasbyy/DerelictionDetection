###############################################################################
#  VACANCY COMPARISON ANALYSIS  --  Cork City
#  Shea Casby, MSc Geoinformatics, UCC.   Version 8.
#
#  PURPOSE OF THIS VERSION
#  Version 6 fixed the bivariate LISA. Version 7 changes nothing about the
#  analysis logic except where noted below, but makes every figure that the
#  results chapter needs an EXPLICIT, TAGGED, PRINTED output. Nothing is
#  computed outside this script.
#
#  Every reportable quantity is emitted twice:
#    1. to the console (and to Analysis8_Console_Log.txt via sink)
#    2. to a CSV in OUT/tables/ named after its table tag
#  Each block is announced with a banner of the form
#       ### [T3.2] Building-level agreement statistics ###
#  where T3.2 is the table number in the results document.
#
#  CHANGES FROM VERSION 6
#  - Section 3   : Is_Residential NA -> 0 retained. NA means "not on the
#                  residential footprint layer", i.e. genuinely not residential.
#                  Message reworded from a warning to a statement of intent.
#  - Section 3.5 : "Weighted 2:1" label corrected to "Youden's J (1:1)".
#                  Confusion matrix, sens/spec/PPV/NPV, ci.auc and a bootstrap
#                  optimism correction for threshold selection added.
#                  Optional k-fold refit CV if the indicator columns are named
#                  in GRP_COLS below.
#  - Section 10  : univariate LISA switched to localmoran_perm() using the same
#                  "Pr(folded) Sim" column as the fixed bivariate, so the two
#                  are on the same inferential footing. Analytic p-values are
#                  still printed alongside for comparison.
#  - Section 13  : the GeoDirectory GWR does NOT control for
#                  Observation_Coverage_Rate_Res. That variable is the share of
#                  RESIDENTIAL footprints observed, and GeoDirectory covers
#                  commercial delivery points too, so it is not a valid control
#                  for that model. Percent_New_Imagery is ALSO dropped from the
#                  GeoDirectory GWR: it is highest in the city centre, which is
#                  also where vacancy concentrates, so it was proxying location
#                  rather than imagery vintage and looked spuriously important.
#                  The GeoDirectory GWR (M2, M4) is now univariate.
#  - Section 15  : NEW. The Youden-threshold classification is pushed through
#                  the whole small-area pipeline (rates, Moran, LISA, GWR) as a
#                  robustness check that the spatial findings do not depend on
#                  the prevalence anchor.
#  - Section 16  : NEW. Coverage-exclusion robustness for the bivariate LISA.
#  - Section 17  : NEW. Field validation sample sizes and a seeded sample draw.
###############################################################################

## ============================== 0. CONFIG ============================== ##
GDB      <- "C:/Users/sheac/Documents/ArcGIS/Projects/StreetViewTesting/StreetViewTesting.gdb"
OUT      <- "C:/Users/sheac/Documents/College/Dissertation/Analysis8"
CSV      <- "C:/Users/sheac/Documents/College/Dissertation/P2_GeminiResults_rank3_point_short_res.csv"
GPKG     <- file.path(OUT, "Vacancy_Compare.gpkg")
GT_FILE  <- "C:/Users/sheac/Documents/College/Dissertation/Claude/GROUND_TRUTH_MASTER.csv"
GLOSSARY <- "C:/Users/sheac/Downloads/Glossary_Saps_2022_REVISED_21102024.xlsx"

L_SA      <- "Cork_SAPs_Analysis"
L_CITY    <- "CorkCity_Boundary"
L_BLDG    <- "P2_GeminiResults_rank3"
L_GD22    <- "GeoDirectoryQ32022_CorkCity"
L_GD23    <- "GeoDirectoryQ42023_CorkCity"
L_RES_ALL <- "P2_buildings2_residentialfinal"

CUTOFF   <- as.Date("2023-05-01")
MIN_EVAL <- 20
NSIM     <- 9999
ITM      <- 2157
USE_POLYGONS <- TRUE
MATCH_TOL    <- 8
BV_PCOL  <- "Pr(folded) Sim"   # permutation p-value column, see Section 10
N_BOOT   <- 2000               # bootstrap reps for threshold optimism, Section 3.5
CV_K     <- 5                  # folds for the optional refit CV, Section 3.5

# OPTIONAL. If you name the five Firth index columns here, Section 3.5 will
# refit the model inside cross-validation folds and report a properly
# out-of-sample AUC. Leave as NULL to skip that and report only the bootstrap
# threshold-optimism correction. Set to the actual column names in CSV, e.g.
# GRP_COLS <- c("Index_Defence","Index_Structural","Index_Neglect",
#               "Index_Interventions","Index_Occupancy")
GRP_COLS <- c("Index_Defence","Index_Structural","Index_Neglect",
              "Index_Interventions","Index_Occupancy")

# The 20 per-photo visual indicator flags (0/1) that Linear_Score is built
# from. Used in Section 6.3 to profile the 361 GeoDirectory-only "true
# misses" (Section 3.2, item P5): are they failing to trigger the model's
# negative predictors (maintained_curtilage, domestic_items), or are they
# simply not showing the positive ones?
IND_COLS <- c("boarded_up","fixed_grilles","obsolete_signage","threshold_debris",
              "facade_decay","graffiti","notices","smashed_glass","for_sale_sign",
              "roof_damage","external_padlocks","exposed_wiring","structural_vegetation",
              "infilled_apertures","internal_debris","overgrown_grounds","domestic_items",
              "waste_management","active_vehicles","maintained_curtilage")

set.seed(20260905)
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUT, "tables"), showWarnings = FALSE, recursive = TRUE)
if (!dir.exists(OUT)) stop("Could not create output folder: ", OUT)

## ---- logging + table emitter -------------------------------------------- ##
LOGFILE <- file.path(OUT, "Analysis8_Console_Log.txt")
if (file.exists(LOGFILE)) file.remove(LOGFILE)
sink(LOGFILE, split = TRUE)          # everything goes to console AND file
# If anything fails mid-run, close the sink so the error is visible on the console.
options(error = function() { while (sink.number() > 0) sink(); cat("\n*** RUN FAILED. See above. Partial log:", LOGFILE, "***\n") })
cat("ANALYSIS 8 RUN:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Output folder:", normalizePath(OUT, winslash = "/"), "\n")
cat("GeoPackage  :", GPKG, "\n\n")

BANNER <- function(tag, title) {
  cat("\n\n", strrep("=", 78), "\n", sep = "")
  cat("### [", tag, "] ", title, " ###\n", sep = "")
  cat(strrep("=", 78), "\n", sep = "")
}
# emit(): print a data.frame and write it to tables/<tag>.csv
emit <- function(tag, title, df, digits = 4) {
  BANNER(tag, title)
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  df[] <- lapply(df, function(cc) if (is.matrix(cc)) as.vector(cc) else cc)
  print(df, row.names = FALSE, digits = digits)
  fn <- file.path(OUT, "tables", paste0(gsub("[^A-Za-z0-9]", "_", tag), ".csv"))
  write.csv(df, fn, row.names = FALSE)
  cat("\n-> written:", basename(fn), "\n")
  invisible(df)
}
# kv(): emit a simple key/value table from a named list
kv <- function(tag, title, lst) {
  emit(tag, title, data.frame(Quantity = names(lst),
                              Value = unname(sapply(lst, function(z) as.character(z))),
                              stringsAsFactors = FALSE))
}
pct <- function(x, d = 3) sprintf(paste0("%.", d, "f%%"), 100 * x)
fm  <- function(x, d = 4) formatC(x, format = "f", digits = d)

## ============================= 1. PACKAGES ============================= ##
need <- c("sf","dplyr","spdep","lmtest","pROC","GWmodel","readxl")
new  <- setdiff(need, rownames(installed.packages()))
if (length(new)) install.packages(new)
invisible(lapply(need, library, character.only = TRUE))
kv("T0.1", "Software versions",
   list(R = paste(R.version$major, R.version$minor, sep="."),
        sf = as.character(packageVersion("sf")),
        spdep = as.character(packageVersion("spdep")),
        GWmodel = as.character(packageVersion("GWmodel")),
        pROC = as.character(packageVersion("pROC")),
        seed = 20260905, NSIM = NSIM, MIN_EVAL = MIN_EVAL,
        BV_PCOL = BV_PCOL))

## ===================== 2. SMALL AREAS AND THE CENSUS ==================== ##
sa <- st_read(GDB, layer = L_SA, quiet = TRUE) |> st_transform(ITM)
tot <- sum(sa$T6_8_T, na.rm = TRUE)

cen_defs <- data.frame(
  Definition = c("Other vacant only (T6_8_OVD) [USED]",
                 "Vacant + unoccupied holiday homes (OVD + UHH)",
                 "Everything not occupied (T6_8_T - T6_8_O)"),
  Vacant = c(sum(sa$T6_8_OVD, na.rm=TRUE),
             sum(sa$T6_8_OVD + sa$T6_8_UHH, na.rm=TRUE),
             tot - sum(sa$T6_8_O, na.rm=TRUE)),
  Total_Dwellings = tot, stringsAsFactors = FALSE)
cen_defs$Rate_Pct <- round(100 * cen_defs$Vacant / cen_defs$Total_Dwellings, 3)
emit("T2.2a", "Census 2022 vacancy definitions (city-wide)", cen_defs)

CEN_VAC <- sa$T6_8_OVD
sa$Census_Vacant <- CEN_VAC
sa$Census_Total  <- sa$T6_8_T
sa$Small_Area_ID <- as.character(sa$SA_GUID_2022)

city <- st_read(GDB, layer = L_CITY, quiet = TRUE) |> st_transform(ITM)
inside <- st_within(st_point_on_surface(st_geometry(sa)), st_union(city), sparse = FALSE)[,1]
n_sa_all <- nrow(sa)
sa <- sa[inside, c("Small_Area_ID","Census_Vacant","Census_Total")]
kv("T0.2", "Small Area counts",
   list(Small_Areas_loaded = n_sa_all, Small_Areas_inside_city_boundary = nrow(sa)))

## ==================== 3. BUILDINGS AND THEIR DATES ===================== ##
b <- read.csv(CSV, stringsAsFactors = FALSE)
keep_cols <- c("GUID","Linear_Score","City_Rank","Priority_Admission",
               "Vacancy_Tier_All","Vacancy_Tier_NoOut","has_target_building",
               "Capture_Date_Converted","Shape_Area2","Lat","Lon","Is_Residential")
if (!is.null(GRP_COLS)) keep_cols <- c(keep_cols, GRP_COLS)
if (!is.null(IND_COLS)) keep_cols <- c(keep_cols, IND_COLS)
missing_cols <- setdiff(keep_cols, names(b))
if (length(missing_cols)) {
  cat("\nNOTE: these requested columns are not in the CSV and were dropped:\n  ",
      paste(missing_cols, collapse = ", "), "\n")
  if (!is.null(GRP_COLS) && any(GRP_COLS %in% missing_cols)) {
    cat("  GRP_COLS is therefore being ignored; the refit CV in 3.5 will be skipped.\n")
    GRP_COLS <- NULL
  }
  if (!is.null(IND_COLS) && any(IND_COLS %in% missing_cols)) {
    cat("  IND_COLS is therefore being trimmed; the indicator profile in 6.3 will use\n")
    cat("  only the flags that were actually found.\n")
    IND_COLS <- intersect(IND_COLS, names(b))
    if (length(IND_COLS) == 0) IND_COLS <- NULL
  }
  keep_cols <- setdiff(keep_cols, missing_cols)
}
b <- b[, keep_cols]

b$Is_Residential <- as.integer(b$Is_Residential)
n_res_na <- sum(is.na(b$Is_Residential))
# NA on this field means the building did not match the residential footprint
# layer, i.e. it is not residential. Coding NA to 0 is the intended semantics,
# not a fallback: the field is TRUE (1) / FALSE (0 or NA).
b$Is_Residential[is.na(b$Is_Residential)] <- 0L

b$Tier_Number            <- substr(as.character(b$Vacancy_Tier_NoOut), 1, 1)
b$Is_Evaluable           <- as.integer(b$Tier_Number %in% c("1","2","3"))
b$Is_Tier1_Vacant        <- as.integer(b$Tier_Number == "1")
b$Is_Tier1_or_2_Vacant   <- as.integer(b$Tier_Number %in% c("1","2"))
b$StreetView_Capture_Date    <- as.Date(substr(b$Capture_Date_Converted, 1, 10))
b$StreetView_Capture_Vintage <- ifelse(b$StreetView_Capture_Date < CUTOFF, "Q3_2022", "Q4_2023")

n_eval <- sum(b$Is_Evaluable)
tier_tab <- as.data.frame(table(b$Vacancy_Tier_NoOut), stringsAsFactors = FALSE)
names(tier_tab) <- c("Vacancy_Tier_NoOut","n")
tier_tab$Pct_of_all_buildings <- round(100 * tier_tab$n / nrow(b), 3)
tier_tab$Pct_of_evaluable <- ifelse(substr(tier_tab$Vacancy_Tier_NoOut,1,1) %in% c("1","2","3"),
                                    round(100 * tier_tab$n / n_eval, 3), NA)
emit("T2.1a", "Tier composition of all buildings", tier_tab)

funnel <- data.frame(
  Stage = c("Buildings loaded","Residential flag = 1","Residential flag = 0 (incl. NA recode)",
            "  of which NA in source, recoded to 0",
            "Excluded as outbuilding","Unevaluable / occluded (Tier 4)",
            "EVALUABLE (Tiers 1 to 3)","  evaluable and residential",
            "  evaluable and non-residential",
            "Tier 1 (long-term vacant)","Tier 2 (transitional vacant)",
            "Tier 1 + Tier 2","Tier 3 (occupied / unflagged)"),
  Count = c(nrow(b), sum(b$Is_Residential==1), sum(b$Is_Residential==0), n_res_na,
            sum(b$Tier_Number=="E" | grepl("^Excluded", b$Vacancy_Tier_NoOut)),
            sum(b$Tier_Number=="4"),
            n_eval, sum(b$Is_Evaluable==1 & b$Is_Residential==1),
            sum(b$Is_Evaluable==1 & b$Is_Residential==0),
            sum(b$Is_Tier1_Vacant), sum(b$Tier_Number=="2"),
            sum(b$Is_Tier1_or_2_Vacant), sum(b$Tier_Number=="3")),
  stringsAsFactors = FALSE)
funnel$Pct_of_all       <- round(100 * funnel$Count / nrow(b), 3)
funnel$Pct_of_evaluable <- round(100 * funnel$Count / n_eval, 3)
emit("T2.1", "The building funnel", funnel)

## ================= 3.5 GROUND TRUTH OPTIMAL THRESHOLD ================= ##
BANNER("T2.3", "Ground truth validation of the visual dereliction score")

if (!file.exists(GT_FILE)) {
  cat("ERROR: Ground Truth file not found at:\n  ", GT_FILE, "\n")
  cat("T2.3 CANNOT BE PRODUCED. Fix GT_FILE and rerun.\n")
  b$AI_Vacant_By_Threshold <- NA_integer_
} else {
  gt_raw <- read.csv(GT_FILE, stringsAsFactors = FALSE)
  if ("Bldg_GUID" %in% names(gt_raw)) names(gt_raw)[names(gt_raw)=="Bldg_GUID"] <- "GUID"
  if ("GEO_ID"    %in% names(gt_raw)) names(gt_raw)[names(gt_raw)=="GEO_ID"]    <- "GUID"
  gt_clean <- gt_raw[!is.na(gt_raw$GROUND_TRUTH), c("GUID","GROUND_TRUTH")]

  gt_cols <- c("GUID","Linear_Score", if (!is.null(GRP_COLS)) GRP_COLS)
  gt_eval <- merge(b[!is.na(b$Linear_Score), gt_cols], gt_clean, by = "GUID")

  if (nrow(gt_eval) == 0) {
    cat("ERROR: no matching GUIDs between ground truth and building data.\n")
    b$AI_Vacant_By_Threshold <- NA_integer_
  } else {
    truth <- gt_eval$GROUND_TRUTH
    roc_score <- pROC::roc(truth, gt_eval$Linear_Score, quiet = TRUE)
    auc_score <- as.numeric(pROC::auc(roc_score))
    auc_ci    <- as.numeric(pROC::ci.auc(roc_score, method = "delong"))

    all_thresh <- pROC::coords(roc_score, "all",
                   ret = c("threshold","sensitivity","specificity","accuracy"),
                   transpose = FALSE)
    # Youden's J. Weight is 1:1 on sensitivity and specificity.
    all_thresh$youden_J <- all_thresh$sensitivity + all_thresh$specificity - 1
    best_thresh <- all_thresh[which.max(all_thresh$youden_J), ]
    optimal_cutoff <- best_thresh$threshold[1]

    b$AI_Vacant_By_Threshold <- as.integer(b$Linear_Score >= optimal_cutoff)
    pred <- as.integer(gt_eval$Linear_Score >= optimal_cutoff)

    # ---- confusion matrix at the chosen cutoff --------------------------- #
    TP <- sum(pred==1 & truth==1); FP <- sum(pred==1 & truth==0)
    FN <- sum(pred==0 & truth==1); TN <- sum(pred==0 & truth==0)
    cm <- data.frame(
      Predicted = c("Predicted vacant","Predicted not vacant","Column total"),
      Truth_vacant     = c(TP, FN, TP+FN),
      Truth_not_vacant = c(FP, TN, FP+TN),
      Row_total        = c(TP+FP, FN+TN, TP+FP+FN+TN), stringsAsFactors = FALSE)
    emit("T2.3a", "Confusion matrix at the Youden cutoff", cm)

    sens <- TP/(TP+FN); spec <- TN/(TN+FP)
    ppv  <- TP/(TP+FP); npv  <- TN/(TN+FN)
    kv("T2.3b", "Classification performance at the Youden cutoff", list(
      Ground_truth_records_matched = nrow(gt_eval),
      Truth_vacant  = sum(truth==1),
      Truth_not_vacant = sum(truth==0),
      Base_rate_vacant = pct(mean(truth==1)),
      AUC = fm(auc_score),
      AUC_95CI_DeLong = paste0(fm(auc_ci[1])," to ",fm(auc_ci[3])),
      Optimal_cutoff_YoudenJ = fm(optimal_cutoff),
      Youden_J_at_cutoff = fm(best_thresh$youden_J[1]),
      Sensitivity = pct(sens), Specificity = pct(spec),
      PPV_precision = pct(ppv), NPV = pct(npv),
      Accuracy = pct((TP+TN)/nrow(gt_eval)),
      Balanced_accuracy = pct((sens+spec)/2),
      GT_predicted_vacant = sum(pred),
      City_predicted_vacant = sum(b$AI_Vacant_By_Threshold[b$Is_Evaluable==1], na.rm=TRUE),
      City_evaluable = n_eval,
      City_predicted_rate = pct(sum(b$AI_Vacant_By_Threshold[b$Is_Evaluable==1], na.rm=TRUE)/n_eval)))

    # ---- bootstrap optimism of THRESHOLD SELECTION ------------------------ #
    # The score itself is fixed here, so this corrects only the optimism from
    # choosing the cutoff on the same data it is evaluated on. It does NOT
    # correct optimism in the Firth coefficients: for that, set GRP_COLS.
    cat("\n--- bootstrap optimism of threshold selection (", N_BOOT, " reps) ---\n", sep="")
    set.seed(20260905)
    opt_J <- numeric(N_BOOT); opt_sens <- numeric(N_BOOT); opt_spec <- numeric(N_BOOT)
    for (i in seq_len(N_BOOT)) {
      idx <- sample(nrow(gt_eval), replace = TRUE)
      tb <- truth[idx]; sb <- gt_eval$Linear_Score[idx]
      if (length(unique(tb)) < 2) { opt_J[i] <- NA; next }
      rb <- pROC::roc(tb, sb, quiet = TRUE)
      cb <- pROC::coords(rb, "all", ret = c("threshold","sensitivity","specificity"),
                         transpose = FALSE)
      cb$J <- cb$sensitivity + cb$specificity - 1
      cut_b <- cb$threshold[which.max(cb$J)][1]
      J_boot <- max(cb$J, na.rm = TRUE)                      # apparent, in bootstrap
      pj <- as.integer(gt_eval$Linear_Score >= cut_b)        # applied to original
      s1 <- sum(pj==1 & truth==1)/sum(truth==1)
      s0 <- sum(pj==0 & truth==0)/sum(truth==0)
      opt_J[i] <- J_boot - (s1 + s0 - 1)
      opt_sens[i] <- s1; opt_spec[i] <- s0
    }
    optimism <- mean(opt_J, na.rm = TRUE)
    kv("T2.3c", "Bootstrap optimism correction for threshold selection", list(
      Bootstrap_reps = N_BOOT,
      Apparent_Youden_J = fm(best_thresh$youden_J[1]),
      Mean_optimism = fm(optimism),
      Optimism_corrected_J = fm(best_thresh$youden_J[1] - optimism),
      Mean_sensitivity_of_bootstrap_cutoffs_on_full_data = pct(mean(opt_sens, na.rm=TRUE)),
      Mean_specificity_of_bootstrap_cutoffs_on_full_data = pct(mean(opt_spec, na.rm=TRUE)),
      NOTE = "Corrects cutoff selection only. Score coefficients are still in-sample unless GRP_COLS is set."))

    # ---- optional: refit the model inside CV folds ------------------------ #
    if (!is.null(GRP_COLS) && requireNamespace("logistf", quietly = TRUE)) {
      library(logistf)
      cat("\n--- ", CV_K, "-fold cross-validated AUC with Firth refit per fold ---\n", sep="")
      set.seed(20260905)
      folds <- sample(rep(seq_len(CV_K), length.out = nrow(gt_eval)))
      oof <- rep(NA_real_, nrow(gt_eval))
      fml <- as.formula(paste("GROUND_TRUTH ~", paste(GRP_COLS, collapse = " + ")))
      for (k in seq_len(CV_K)) {
        tr <- gt_eval[folds != k, ]; te <- gt_eval[folds == k, ]
        fit <- logistf::logistf(fml, data = tr)
        cf <- coef(fit)
        X <- cbind(1, as.matrix(te[, GRP_COLS]))
        oof[folds == k] <- as.numeric(X %*% cf)
      }
      roc_cv <- pROC::roc(truth, oof, quiet = TRUE)
      auc_cv <- as.numeric(pROC::auc(roc_cv))
      kv("T2.3d", "Cross-validated AUC with model refit inside folds", list(
        Folds = CV_K, Apparent_AUC = fm(auc_score),
        CrossValidated_AUC = fm(auc_cv),
        Optimism = fm(auc_score - auc_cv),
        Predictors = paste(GRP_COLS, collapse = ", ")))
    } else {
      BANNER("T2.3d", "Cross-validated AUC with model refit inside folds")
      cat("SKIPPED. Set GRP_COLS in Section 0 to the five Firth index column\n")
      cat("names in the source CSV, and install 'logistf', to produce this.\n")
      cat("Without it, the AUC in T2.3b remains an APPARENT (in-sample) figure.\n")
      write.csv(data.frame(Status="SKIPPED", Reason="GRP_COLS is NULL or logistf missing"),
                file.path(OUT,"tables","T2_3d.csv"), row.names = FALSE)
    }
  }
}

bpt <- st_as_sf(b, coords = c("Lon","Lat"), crs = 4326) |> st_transform(ITM)

## ======================= 4. THE TWO GEODIRECTORY SNAPSHOTS ============== ##
read_gd <- function(layer, tag) {
  g <- st_read(GDB, layer = layer, quiet = TRUE) |> st_transform(ITM)
  geom <- attr(g, "sf_column"); names(g) <- toupper(names(g)); st_geometry(g) <- toupper(geom)
  yn <- function(x) as.integer(toupper(trimws(as.character(x))) %in% c("Y","YES","1","TRUE"))
  g$gd_vac <- yn(g$VACANT); g$gd_der <- yn(g$DERELICT)
  g$gd_any <- as.integer(g$gd_vac == 1 | g$gd_der == 1)
  g
}
gd22 <- read_gd(L_GD22, "GeoDirectory Q3 2022")
gd23 <- read_gd(L_GD23, "GeoDirectory Q4 2023")

gd_city <- data.frame(
  Snapshot = c("GeoDirectory Q3 2022","GeoDirectory Q4 2023"),
  Layer = c(L_GD22, L_GD23),
  Records = c(nrow(gd22), nrow(gd23)),
  Vacant_flag = c(sum(gd22$gd_vac), sum(gd23$gd_vac)),
  Derelict_flag = c(sum(gd22$gd_der), sum(gd23$gd_der)),
  Vacant_or_derelict = c(sum(gd22$gd_any), sum(gd23$gd_any)),
  stringsAsFactors = FALSE)
gd_city$Rate_Pct <- round(100 * gd_city$Vacant_or_derelict / gd_city$Records, 3)
emit("T2.2b", "GeoDirectory city-wide counts (exact integers)", gd_city)

## ====================== 5. THE COVERAGE CHECK ========================== ##
ev <- b[b$Is_Evaluable == 1, ]
vt <- ev |> group_by(StreetView_Capture_Vintage) |>
  summarise(Evaluable_buildings = n(),
            Tier1_Pct  = 100*mean(Is_Tier1_Vacant),
            Tier12_Pct = 100*mean(Is_Tier1_or_2_Vacant),
            Youden_Pct = 100*mean(AI_Vacant_By_Threshold, na.rm = TRUE),
            Mean_Linear_Score = mean(Linear_Score),
            Mean_Area_m2 = mean(Shape_Area2, na.rm = TRUE), .groups = "drop")
emit("T2.5a", "Imagery vintage: AI measures (GeoDirectory row added in T2.5b)", vt)

## ================= 6. MATCHING BUILDINGS TO GEODIRECTORY =============== ##
join_gd <- function(target, gd, prefix) {
  j <- st_join(gd[, c("gd_vac","gd_der","gd_any")], target["GUID"], join = st_within, left = FALSE)
  out <- j |> st_drop_geometry() |> group_by(GUID) |>
    summarise(n = n(), vac = as.integer(any(gd_vac == 1)), der = as.integer(any(gd_der == 1)),
              any = as.integer(any(gd_any == 1)), .groups = "drop")
  names(out)[-1] <- paste0(prefix, "_", names(out)[-1]); out
}
if (USE_POLYGONS) {
  poly <- st_read(GDB, query = paste0("SELECT GUID, Shape FROM ", L_BLDG), quiet = TRUE) |> st_transform(ITM)
  poly <- poly[poly$GUID %in% b$GUID, ]
  m22 <- join_gd(poly, gd22, "g22"); m23 <- join_gd(poly, gd23, "g23")
} else {
  nn <- function(gd, prefix) {
    i <- st_nearest_feature(bpt, gd)
    d <- as.numeric(st_distance(bpt, gd[i,], by_element = TRUE)); ok <- d <= MATCH_TOL
    out <- data.frame(GUID = bpt$GUID, n = as.integer(ok),
                      vac = ifelse(ok, gd$gd_vac[i], NA_integer_),
                      der = ifelse(ok, gd$gd_der[i], NA_integer_),
                      any = ifelse(ok, gd$gd_any[i], NA_integer_))
    names(out)[-1] <- paste0(prefix, "_", names(out)[-1]); out
  }
  m22 <- nn(gd22, "g22"); m23 <- nn(gd23, "g23")
}
b <- b |> left_join(m22, by = "GUID") |> left_join(m23, by = "GUID")
for (c_ in c("g22_any","g23_any","g22_vac","g23_vac","g22_der","g23_der")) b[[c_]][is.na(b[[c_]])] <- 0L
b$g22_n[is.na(b$g22_n)] <- 0L; b$g23_n[is.na(b$g23_n)] <- 0L

b$GeoDir_Q3_2022_Vacant <- b$g22_any
b$GeoDir_Q4_2023_Vacant <- b$g23_any
b$GeoDir_Time_Matched_Vacant <- ifelse(b$StreetView_Capture_Vintage == "Q3_2022",
                                       b$GeoDir_Q3_2022_Vacant, b$GeoDir_Q4_2023_Vacant)
b$GeoDir_Persistent_Vacant <- as.integer(b$GeoDir_Q3_2022_Vacant == 1 & b$GeoDir_Q4_2023_Vacant == 1)
b$GeoDir_Newly_Vacant      <- as.integer(b$GeoDir_Q3_2022_Vacant == 0 & b$GeoDir_Q4_2023_Vacant == 1)
b$GeoDir_Record_Exists     <- as.integer(b$g22_n > 0 | b$g23_n > 0)
b$Validation_Agreement <- ifelse(b$Is_Evaluable == 0, NA_character_,
  ifelse(b$Is_Tier1_Vacant == 1 & b$GeoDir_Time_Matched_Vacant == 1, "Both",
  ifelse(b$Is_Tier1_Vacant == 1 & b$GeoDir_Time_Matched_Vacant == 0, "Visible only",
  ifelse(b$Is_Tier1_Vacant == 0 & b$GeoDir_Time_Matched_Vacant == 1, "Recorded only", "Neither"))))
ev <- b[b$Is_Evaluable == 1, ]

## ---- T2.5b: the vintage table with the independent GeoDirectory row ----- ##
vt2 <- ev |> group_by(StreetView_Capture_Vintage) |>
  summarise(GeoDir_Vacancy_Rate_Pct = 100*mean(GeoDir_Time_Matched_Vacant), .groups="drop")
vtab <- merge(vt, vt2, by = "StreetView_Capture_Vintage")
r22 <- vtab[vtab$StreetView_Capture_Vintage=="Q3_2022", ]
r23 <- vtab[vtab$StreetView_Capture_Vintage=="Q4_2023", ]
ratios <- data.frame(
  Measure = c("Evaluable buildings","Tier 1 rate","Tier 1+2 rate","Youden rate",
              "Mean Linear_Score","Mean area m2","GeoDirectory vacancy rate"),
  Q3_2022 = c(r22$Evaluable_buildings, r22$Tier1_Pct, r22$Tier12_Pct, r22$Youden_Pct,
              r22$Mean_Linear_Score, r22$Mean_Area_m2, r22$GeoDir_Vacancy_Rate_Pct),
  Q4_2023 = c(r23$Evaluable_buildings, r23$Tier1_Pct, r23$Tier12_Pct, r23$Youden_Pct,
              r23$Mean_Linear_Score, r23$Mean_Area_m2, r23$GeoDir_Vacancy_Rate_Pct),
  stringsAsFactors = FALSE)
ratios$Ratio_2023_over_2022 <- round(ratios$Q4_2023 / ratios$Q3_2022, 3)
ratios$Ratio_2023_over_2022[ratios$Measure %in% c("Evaluable buildings","Mean Linear_Score")] <- NA
emit("T2.5", "Imagery vintage confound, with the independent GeoDirectory benchmark", ratios)
cat("\nREAD THIS ROW: if the GeoDirectory ratio is close to the AI ratios, the vintage gap is\n")
cat("mostly a real difference between the areas covered, not an imagery artefact.\n")

## ---- T2.6: residential vs non-residential ------------------------------ ##
res_tab <- ev |> group_by(Is_Residential) |>
  summarise(Evaluable = n(),
            Tier1_n = sum(Is_Tier1_Vacant), Tier1_Pct = 100*mean(Is_Tier1_Vacant),
            Tier12_n = sum(Is_Tier1_or_2_Vacant), Tier12_Pct = 100*mean(Is_Tier1_or_2_Vacant),
            Youden_n = sum(AI_Vacant_By_Threshold, na.rm=TRUE),
            Youden_Pct = 100*mean(AI_Vacant_By_Threshold, na.rm=TRUE),
            Mean_Linear_Score = mean(Linear_Score),
            GeoDir_Rate_Pct = 100*mean(GeoDir_Time_Matched_Vacant), .groups="drop")
res_tab$Is_Residential <- ifelse(res_tab$Is_Residential==1,"Residential (1)","Non-residential (0/NA)")
emit("T2.6", "Residential versus non-residential vacancy rates", res_tab)
ct_res <- table(ev$Is_Residential, ev$Is_Tier1_Vacant)
cs_res <- chisq.test(ct_res)
kv("T2.6b", "Test of the residential / non-residential difference (Tier 1)", list(
  Chi_squared = fm(as.numeric(cs_res$statistic),3), df = cs_res$parameter,
  p_value = format.pval(cs_res$p.value, digits=4),
  Risk_ratio_nonres_over_res = fm(
    (ct_res[1,2]/sum(ct_res[1,])) / (ct_res[2,2]/sum(ct_res[2,])), 3),
  NOTE = "Non-residential here means the building did not match the residential footprint layer."))

## ============ 6.2 BUILDING-LEVEL AGREEMENT: THE 2x2 TABLES ============= ##
# Generic 2x2 reporter. ai and gd are 0/1 integer vectors of equal length.
agree2x2 <- function(ai, gd, label, tag_tab, tag_stat) {
  # as.numeric() is essential: with N ~ 53,000 the cross-products in the
  # expected-agreement and phi formulas exceed R's 32-bit integer limit and
  # silently return NA. Doubles avoid that.
  A <- as.numeric(sum(ai==1 & gd==1)); B <- as.numeric(sum(ai==1 & gd==0))
  C <- as.numeric(sum(ai==0 & gd==1)); D <- as.numeric(sum(ai==0 & gd==0)); N <- A+B+C+D
  tab <- data.frame(
    Row = c(paste0(label," = yes"), paste0(label," = no"), "Column total"),
    GeoDirectory_vacant     = as.integer(c(A, C, A+C)),
    GeoDirectory_not_vacant = as.integer(c(B, D, B+D)),
    Row_total               = as.integer(c(A+B, C+D, N)), stringsAsFactors = FALSE)
  if (!is.na(tag_tab)) emit(tag_tab, paste0("2x2: ", label, " versus time-matched GeoDirectory"), tab)

  po <- (A+D)/N
  pe <- ((A+B)*(A+C) + (C+D)*(B+D)) / N^2
  kappa <- (po-pe)/(1-pe); pabak <- 2*po - 1
  jac <- A/(A+B+C)
  phi <- (A*D - B*C)/sqrt((A+B)*(C+D)*(A+C)*(B+D))
  orv <- if (B>0 && C>0) (A*D)/(B*C) else NA
  mc  <- stats::mcnemar.test(matrix(c(A,C,B,D), nrow=2), correct = TRUE)
  bt  <- stats::binom.test(B, B+C, 0.5)
  stats_list <- list(
    Both = A, AI_only = B, GeoDirectory_only = C, Neither = D, N = N,
    Observed_agreement = pct(po), Expected_agreement = pct(pe),
    Cohens_kappa = fm(kappa), PABAK = fm(pabak),
    Jaccard = paste0(fm(jac)," (",A,"/",A+B+C,")"),
    Phi = fm(phi), Odds_ratio = fm(orv,2),
    Recall_of_GeoDirectory = pct(A/(A+C)),
    Precision_vs_GeoDirectory = pct(A/(A+B)),
    Specificity = pct(D/(D+B)),
    McNemar_chisq_cc = fm(as.numeric(mc$statistic),3),
    McNemar_p = format.pval(mc$p.value, digits=4),
    Exact_binomial_p = format.pval(bt$p.value, digits=4))
  if (!is.na(tag_stat)) kv(tag_stat, paste0("Agreement statistics: ", label), stats_list)
  data.frame(Definition=label, Both=as.integer(A), AI_only=as.integer(B),
             GD_only=as.integer(C), Neither=as.integer(D),
             Kappa=round(kappa,4), PABAK=round(pabak,4), Jaccard=round(jac,4),
             Recall_of_GD_Pct=round(100*A/(A+C),2), Precision_Pct=round(100*A/(A+B),2),
             McNemar_p=mc$p.value, stringsAsFactors = FALSE)
}
r1 <- agree2x2(ev$Is_Tier1_Vacant,       ev$GeoDir_Time_Matched_Vacant, "AI Tier 1",         "T3.1","T3.2")
r2 <- agree2x2(ev$Is_Tier1_or_2_Vacant,  ev$GeoDir_Time_Matched_Vacant, "AI Tier 1+2",       NA, NA)
r3 <- agree2x2(ev$AI_Vacant_By_Threshold,ev$GeoDir_Time_Matched_Vacant, "AI Youden cutoff",  NA, NA)
emit("T3.3", "Agreement under three AI definitions", rbind(r1,r2,r3))

va <- as.data.frame(table(ev$Validation_Agreement), stringsAsFactors = FALSE)
names(va) <- c("Validation_Agreement","n"); va$Pct <- round(100*va$n/nrow(ev),3)
emit("T3.1b", "Validation_Agreement counts (the pie chart)", va)

## ---- T3.4: where did the GeoDirectory-only buildings go? --------------- ##
gdonly <- ev[ev$Is_Tier1_Vacant == 0 & ev$GeoDir_Time_Matched_Vacant == 1, ]
nm <- gdonly |> group_by(Vacancy_Tier_NoOut) |>
  summarise(n = n(), Mean_Linear_Score = mean(Linear_Score),
            Median_Linear_Score = median(Linear_Score),
            Max_Linear_Score = max(Linear_Score), .groups="drop")
nm$Pct_of_GD_only <- round(100*nm$n/nrow(gdonly), 2)
emit("T3.4", "Where the GeoDirectory-only buildings sit in the AI ranking", nm)
kv("T3.4b", "Near-miss summary", list(
  GeoDirectory_flagged_evaluable = sum(ev$GeoDir_Time_Matched_Vacant),
  Caught_at_Tier1 = sum(ev$Is_Tier1_Vacant==1 & ev$GeoDir_Time_Matched_Vacant==1),
  Missed_by_Tier1 = nrow(gdonly),
  Of_those_in_Tier2 = sum(substr(gdonly$Vacancy_Tier_NoOut,1,1)=="2"),
  Of_those_in_Tier2_Pct = pct(mean(substr(gdonly$Vacancy_Tier_NoOut,1,1)=="2")),
  Of_those_in_Tier3 = sum(substr(gdonly$Vacancy_Tier_NoOut,1,1)=="3"),
  Reached_by_Tier1_plus_2 = sum(ev$Is_Tier1_or_2_Vacant==1 & ev$GeoDir_Time_Matched_Vacant==1),
  Reached_by_Tier1_plus_2_Pct = pct(sum(ev$Is_Tier1_or_2_Vacant==1 & ev$GeoDir_Time_Matched_Vacant==1)/
                                    sum(ev$GeoDir_Time_Matched_Vacant))))

## ---- T3.4c: indicator profile of the 361 true misses (plan item P5) ---- ##
# gdonly buildings that landed in Tier 3 are the "true misses": GeoDirectory
# records them as vacant, the model was confident they were fine. This asks
# which of the 20 visual flags are over- or under-firing on them, relative to
# the whole evaluable population, as a check on the two explanations in the
# plan (over-detected occupancy signs vs. genuinely non-derelict-looking
# vacancy).
if (is.null(IND_COLS)) {
  BANNER("T3.4c", "Indicator profile of the true misses")
  cat("SKIPPED. IND_COLS is NULL (the 20 visual flag columns were not found\n")
  cat("in the source CSV). This is plan item P5.\n")
  write.csv(data.frame(Status="SKIPPED", Reason="IND_COLS is NULL"),
            file.path(OUT,"tables","T3_4c.csv"), row.names = FALSE)
} else {
  true_miss <- gdonly[substr(gdonly$Vacancy_Tier_NoOut,1,1)=="3", ]
  ind_rate <- function(df, col) 100 * mean(df[[col]], na.rm = TRUE)
  ind_prof <- do.call(rbind, lapply(IND_COLS, function(cc) data.frame(
    Indicator = cc,
    Rate_True_Misses_Pct   = round(ind_rate(true_miss, cc), 2),
    Rate_All_Evaluable_Pct = round(ind_rate(ev, cc), 2),
    Rate_Tier3_Pct         = round(ind_rate(ev[ev$Tier_Number=="3", ], cc), 2),
    stringsAsFactors = FALSE)))
  ind_prof$Ratio_vs_Evaluable <- round(ind_prof$Rate_True_Misses_Pct / ind_prof$Rate_All_Evaluable_Pct, 2)
  ind_prof$Direction <- ifelse(ind_prof$Ratio_vs_Evaluable > 1.1, "Over-represented",
                         ifelse(ind_prof$Ratio_vs_Evaluable < 0.9, "Under-represented", "Typical"))
  ind_prof <- ind_prof[order(-ind_prof$Ratio_vs_Evaluable), ]
  emit("T3.4c", paste0("Indicator profile of the ", nrow(true_miss),
       " true misses versus the evaluable population"), ind_prof)
  cat("\nThese are DESCRIPTIVE rates, not model contributions: a flag can be common in the\n")
  cat("true misses and still not be what suppressed their score, because Linear_Score is a\n")
  cat("fitted combination, not a simple sum of flags. Read this as 'what these buildings look\n")
  cat("like', not 'why the model missed them'. maintained_curtilage and domestic_items are the\n")
  cat("two flags flagged a priori in the plan as previously over-detected negative predictors.\n")
}

## ---- T3.5: persistent versus newly vacant ------------------------------ ##
gdflag <- ev[ev$GeoDir_Time_Matched_Vacant == 1, ]
pn <- data.frame(
  GeoDirectory_status = c("Persistent (vacant 2022 AND 2023)","Newly vacant (2023 only)",
                          "Non-persistent (all others flagged)"),
  n = c(sum(ev$GeoDir_Persistent_Vacant==1), sum(ev$GeoDir_Newly_Vacant==1),
        sum(gdflag$GeoDir_Persistent_Vacant==0)),
  Tier1_n = c(sum(ev$GeoDir_Persistent_Vacant==1 & ev$Is_Tier1_Vacant==1),
              sum(ev$GeoDir_Newly_Vacant==1 & ev$Is_Tier1_Vacant==1),
              sum(gdflag$GeoDir_Persistent_Vacant==0 & gdflag$Is_Tier1_Vacant==1)),
  Tier12_n = c(sum(ev$GeoDir_Persistent_Vacant==1 & ev$Is_Tier1_or_2_Vacant==1),
               sum(ev$GeoDir_Newly_Vacant==1 & ev$Is_Tier1_or_2_Vacant==1),
               sum(gdflag$GeoDir_Persistent_Vacant==0 & gdflag$Is_Tier1_or_2_Vacant==1)),
  Mean_Linear_Score = c(mean(ev$Linear_Score[ev$GeoDir_Persistent_Vacant==1]),
                        mean(ev$Linear_Score[ev$GeoDir_Newly_Vacant==1]),
                        mean(gdflag$Linear_Score[gdflag$GeoDir_Persistent_Vacant==0])),
  stringsAsFactors = FALSE)
pn$Tier1_Pct  <- round(100*pn$Tier1_n/pn$n, 2)
pn$Tier12_Pct <- round(100*pn$Tier12_n/pn$n, 2)
emit("T3.5", "Detection of persistent versus newly vacant buildings", pn)
ct_pn <- table(gdflag$GeoDir_Persistent_Vacant, gdflag$Is_Tier1_Vacant)
cs_pn <- chisq.test(ct_pn)
kv("T3.5b", "Test: does the model detect persistent vacancy better? (within GeoDirectory-flagged only)", list(
  Persistent_n = sum(gdflag$GeoDir_Persistent_Vacant==1),
  Persistent_Tier1_Pct = pct(mean(gdflag$Is_Tier1_Vacant[gdflag$GeoDir_Persistent_Vacant==1])),
  NonPersistent_n = sum(gdflag$GeoDir_Persistent_Vacant==0),
  NonPersistent_Tier1_Pct = pct(mean(gdflag$Is_Tier1_Vacant[gdflag$GeoDir_Persistent_Vacant==0])),
  Chi_squared = fm(as.numeric(cs_pn$statistic),3), df = cs_pn$parameter,
  p_value = format.pval(cs_pn$p.value, digits=4),
  Mean_score_persistent = fm(mean(gdflag$Linear_Score[gdflag$GeoDir_Persistent_Vacant==1]),3),
  Mean_score_nonpersistent = fm(mean(gdflag$Linear_Score[gdflag$GeoDir_Persistent_Vacant==0]),3),
  Wilcoxon_p = format.pval(wilcox.test(Linear_Score ~ GeoDir_Persistent_Vacant, data=gdflag)$p.value, digits=4)))

## ---- T2.4: score distribution by tier ---------------------------------- ##
sc <- b |> group_by(Vacancy_Tier_NoOut) |>
  summarise(n=n(), Mean=mean(Linear_Score), SD=sd(Linear_Score), Min=min(Linear_Score),
            Median=median(Linear_Score), Max=max(Linear_Score), .groups="drop")
emit("T2.4", "Linear_Score distribution by tier", sc)

bpt2 <- bpt |> st_drop_geometry() |> select(GUID) |>
  left_join(b[, c("GUID","GeoDir_Time_Matched_Vacant","GeoDir_Persistent_Vacant",
                  "GeoDir_Newly_Vacant","Validation_Agreement","GeoDir_Q3_2022_Vacant",
                  "GeoDir_Q4_2023_Vacant","GeoDir_Record_Exists")], by = "GUID")
bpt <- cbind(bpt, bpt2[, -1])
st_write(bpt, GPKG, "buildings", delete_dsn = TRUE, quiet = TRUE)

## ================= 6.5 BUILDING HYBRID GEODIRECTORY LAYER =============== ##
gd22$Point_ID <- paste0("22_", 1:nrow(gd22)); gd23$Point_ID <- paste0("23_", 1:nrow(gd23))
if (!exists("poly")) {
  poly <- st_read(GDB, query = paste0("SELECT GUID, Shape FROM ", L_BLDG), quiet = TRUE) |> st_transform(ITM)
  poly <- poly[poly$GUID %in% b$GUID, ]
}
poly_eval <- poly[poly$GUID %in% b$GUID[b$Is_Evaluable == 1], ]
poly_eval <- poly_eval |> left_join(b[, c("GUID","StreetView_Capture_Vintage","GeoDir_Time_Matched_Vacant")], by="GUID")
j22 <- st_join(gd22, poly_eval, join = st_within, left = TRUE)
j23 <- st_join(gd23, poly_eval, join = st_within, left = TRUE)
keep_22 <- j22 |> filter(is.na(GUID) | StreetView_Capture_Vintage == "Q3_2022")
keep_23 <- j23 |> filter(!is.na(GUID) & StreetView_Capture_Vintage == "Q4_2023")
cols_to_keep <- c("Point_ID","gd_any","GUID","GeoDir_Time_Matched_Vacant")
keep_22 <- keep_22 |> select(any_of(cols_to_keep)); keep_23 <- keep_23 |> select(any_of(cols_to_keep))
gd_hybrid <- bind_rows(keep_22, keep_23) |>
  mutate(gd_any = ifelse(!is.na(GUID) & GeoDir_Time_Matched_Vacant == 1, 1, gd_any)) |>
  select(Point_ID, gd_any)
kv("T0.3", "Hybrid GeoDirectory register", list(
  Hybrid_points = nrow(gd_hybrid), Base_2022_points = nrow(gd22),
  Points_from_2023 = nrow(keep_23), Hybrid_vacant_or_derelict = sum(gd_hybrid$gd_any),
  Hybrid_rate = pct(mean(gd_hybrid$gd_any))))

## ==================== 7. THE ONE SMALL AREA TABLE ====================== ##
res_poly <- st_read(GDB, layer = L_RES_ALL, quiet = TRUE) |> st_transform(ITM)
res_pts  <- st_sf(geometry = st_point_on_surface(st_geometry(res_poly)))
res_counts <- st_join(res_pts, sa["Small_Area_ID"], join = st_intersects, left = FALSE) |>
  st_drop_geometry() |> group_by(Small_Area_ID) |>
  summarise(Total_Residential_Footprints = n(), .groups="drop")

bj <- st_join(bpt, sa["Small_Area_ID"], join = st_intersects, left = FALSE)
bj <- bj[!duplicated(bj$GUID), ]
agg <- bj |> st_drop_geometry() |> group_by(Small_Area_ID) |>
  summarise(Evaluable_Buildings     = sum(Is_Evaluable),
            Evaluable_Residential   = sum(Is_Evaluable == 1 & Is_Residential == 1, na.rm=TRUE),
            Tier1_Buildings         = sum(Is_Tier1_Vacant),
            Tier1_and_2_Buildings   = sum(Is_Tier1_or_2_Vacant),
            Tier1_and_2_Residential = sum(Is_Tier1_or_2_Vacant == 1 & Is_Residential == 1, na.rm=TRUE),
            Youden_Buildings        = sum(AI_Vacant_By_Threshold[Is_Evaluable==1], na.rm=TRUE),
            Youden_Residential      = sum(AI_Vacant_By_Threshold == 1 & Is_Evaluable == 1 &
                                          Is_Residential == 1, na.rm=TRUE),
            New_Imagery_Count       = sum(StreetView_Capture_Vintage == "Q4_2023" & Is_Evaluable == 1),
            Mean_Linear_Score       = mean(Linear_Score[Is_Evaluable == 1]),
            GeoDir_Matched_Count    = sum(GeoDir_Time_Matched_Vacant[Is_Evaluable == 1]),
            GeoDir_Persistent_Count = sum(GeoDir_Persistent_Vacant[Is_Evaluable == 1]),
            GeoDir_Coverage_Count   = sum(GeoDir_Record_Exists[Is_Evaluable == 1]),
            .groups="drop")

gd_rate <- function(gd, prefix) {
  key <- if ("SMALL_AREA_ID" %in% names(gd)) as.character(gd$SMALL_AREA_ID) else NA
  hit <- if (all(is.na(key))) 0 else mean(key %in% sa$Small_Area_ID)
  if (hit > 0.9) {
    out <- data.frame(Small_Area_ID = key, any = gd$gd_any) |> group_by(Small_Area_ID) |>
      summarise(tot = n(), vac = sum(any), .groups="drop")
  } else {
    out <- st_join(gd["gd_any"], sa["Small_Area_ID"], join = st_intersects, left = FALSE) |>
      st_drop_geometry() |> group_by(Small_Area_ID) |>
      summarise(tot = n(), vac = sum(gd_any), .groups="drop")
  }
  names(out)[-1] <- paste0(prefix, "_", names(out)[-1]); out
}
r_hyb <- gd_rate(gd_hybrid, "gd_hyb")

saf <- sa |> left_join(agg, by="Small_Area_ID") |> left_join(r_hyb, by="Small_Area_ID") |>
  left_join(res_counts, by="Small_Area_ID") |>
  mutate(
    AI_Tier1_Rate       = ifelse(Evaluable_Buildings > 0, 100*Tier1_Buildings/Evaluable_Buildings, NA_real_),
    AI_Residential_Rate = ifelse(Evaluable_Residential > 0, 100*Tier1_and_2_Residential/Evaluable_Residential, NA_real_),
    AI_Youden_Rate      = ifelse(Evaluable_Buildings > 0, 100*Youden_Buildings/Evaluable_Buildings, NA_real_),
    AI_Youden_Res_Rate  = ifelse(Evaluable_Residential > 0, 100*Youden_Residential/Evaluable_Residential, NA_real_),
    Census_Rate         = ifelse(Census_Total > 0, 100*Census_Vacant/Census_Total, NA_real_),
    GeoDir_Matched_Rate = ifelse(gd_hyb_tot > 0, 100*gd_hyb_vac/gd_hyb_tot, NA_real_),
    Percent_New_Imagery = ifelse(Evaluable_Buildings > 0, 100*New_Imagery_Count/Evaluable_Buildings, NA_real_),
    Total_Residential_Footprints = ifelse(is.na(Total_Residential_Footprints), 0, Total_Residential_Footprints),
    Observation_Coverage_Rate_Res = ifelse(Total_Residential_Footprints > 0,
                                           100*Evaluable_Residential/Total_Residential_Footprints, NA_real_))

## ---------- MIN_EVAL SENSITIVITY (Appendix B) --------------------------- ##
thresholds_to_test <- c(5,10,15,20,25,30,40)
total_sa <- nrow(saf); total_bldgs <- sum(saf$Evaluable_Buildings, na.rm=TRUE)
sens_rows <- list()
for (t in thresholds_to_test) {
  saf_t <- saf |> filter(!is.na(Evaluable_Buildings), Evaluable_Buildings >= t,
      Evaluable_Residential >= t, !is.na(AI_Tier1_Rate), !is.na(AI_Residential_Rate),
      !is.na(GeoDir_Matched_Rate), !is.na(Census_Rate), !is.na(Observation_Coverage_Rate_Res))
  nb_t <- poly2nb(saf_t, queen = TRUE)
  if (sum(card(nb_t) == 0) > 0) {
    coords <- st_coordinates(st_point_on_surface(st_geometry(saf_t)))
    nb_t <- knn2nb(knearneigh(coords, k = 6), sym = TRUE)
  }
  lw_t <- nb2listw(nb_t, style="W", zero.policy=TRUE)
  sens_rows[[paste0("T",t)]] <- data.frame(
    Threshold=t, SAs_Retained=nrow(saf_t),
    SA_Loss_Pct=round(100*(total_sa-nrow(saf_t))/total_sa,1),
    Bldg_Loss_Pct=round(100*(total_bldgs-sum(saf_t$Evaluable_Buildings,na.rm=TRUE))/total_bldgs,2),
    SD_Res_Rate=round(sd(saf_t$AI_Residential_Rate,na.rm=TRUE),2),
    SD_LT_Rate=round(sd(saf_t$AI_Tier1_Rate,na.rm=TRUE),2),
    Morans_Z_Res=round(moran.test(saf_t$AI_Residential_Rate,lw_t,zero.policy=TRUE)$statistic,2),
    Morans_Z_LT=round(moran.test(saf_t$AI_Tier1_Rate,lw_t,zero.policy=TRUE)$statistic,2))
}
emit("TB.1", "MIN_EVAL sensitivity analysis", do.call(rbind, sens_rows))
write.csv(do.call(rbind, sens_rows), file.path(OUT,"MIN_EVAL_Sensitivity_Analysis.csv"), row.names=FALSE)

n_before <- nrow(saf)
saf <- saf |> filter(!is.na(Evaluable_Buildings), Evaluable_Buildings >= MIN_EVAL,
    Evaluable_Residential >= MIN_EVAL, !is.na(AI_Tier1_Rate), !is.na(AI_Residential_Rate),
    !is.na(GeoDir_Matched_Rate), !is.na(Census_Rate), !is.na(Observation_Coverage_Rate_Res))
saf$Difference_AI_vs_Census <- saf$AI_Residential_Rate - saf$Census_Rate
saf$Difference_AI_vs_GeoDir <- saf$AI_Tier1_Rate - saf$GeoDir_Matched_Rate
st_write(saf, GPKG, "small_areas", delete_layer=TRUE, quiet=TRUE)

## ======================== 8. WEIGHTS AND MORAN ========================= ##
nb <- poly2nb(saf, queen = TRUE)
islands <- which(card(nb) == 0)
n_after_filter <- nrow(saf)
if (length(islands) > 0) { saf <- saf[-islands, ]; nb <- poly2nb(saf, queen = TRUE) }
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
kv("T0.4", "Small Area retention through the filters", list(
  Small_Areas_in_city = n_before,
  Passed_MIN_EVAL_and_completeness = n_after_filter,
  Dropped_as_islands_no_queen_neighbour = length(islands),
  FINAL_N_used_in_all_spatial_analysis = nrow(saf),
  Pct_of_city_SAs_retained = pct(nrow(saf)/n_before),
  Evaluable_buildings_in_retained_SAs = sum(saf$Evaluable_Buildings),
  Pct_of_evaluable_buildings_retained = pct(sum(saf$Evaluable_Buildings)/n_eval),
  Mean_neighbours = fm(mean(card(nb)),3)))

gm_rows <- list()
gm <- function(v, nm) {
  t <- moran.test(v, lw, zero.policy = TRUE)
  mc <- moran.mc(v, lw, nsim = 999, zero.policy = TRUE)
  gm_rows[[nm]] <<- data.frame(Variable=nm, Morans_I=round(as.numeric(t$estimate[1]),4),
    Expected_I=round(as.numeric(t$estimate[2]),5), Variance=signif(as.numeric(t$estimate[3]),4),
    Z_score=round(as.numeric(t$statistic),3), p_analytic=format.pval(t$p.value,digits=4),
    p_permutation_999=format.pval(mc$p.value,digits=4), stringsAsFactors=FALSE)
}
gm(saf$AI_Tier1_Rate,"AI_Tier1_Rate"); gm(saf$AI_Residential_Rate,"AI_Residential_Rate")
gm(saf$AI_Youden_Rate,"AI_Youden_Rate"); gm(saf$AI_Youden_Res_Rate,"AI_Youden_Res_Rate")
gm(saf$Census_Rate,"Census_Rate"); gm(saf$GeoDir_Matched_Rate,"GeoDir_Matched_Rate")
gm(saf$Percent_New_Imagery,"Percent_New_Imagery")
gm(saf$Observation_Coverage_Rate_Res,"Observation_Coverage_Rate_Res")
gm(saf$Mean_Linear_Score,"Mean_Linear_Score")
gm(saf$Difference_AI_vs_Census,"Difference_AI_vs_Census")
gm(saf$Difference_AI_vs_GeoDir,"Difference_AI_vs_GeoDir")
emit("T3.8", "Global Moran's I, Queen contiguity, row-standardised", do.call(rbind, gm_rows))

## ---- T3.6 descriptives, T3.7 correlations ------------------------------ ##
d <- st_drop_geometry(saf)
dvars <- c("AI_Tier1_Rate","AI_Residential_Rate","AI_Youden_Rate","AI_Youden_Res_Rate",
           "Census_Rate","GeoDir_Matched_Rate","Percent_New_Imagery",
           "Observation_Coverage_Rate_Res","Difference_AI_vs_Census","Difference_AI_vs_GeoDir",
           "Evaluable_Buildings","Evaluable_Residential","Total_Residential_Footprints")
desc <- do.call(rbind, lapply(dvars, function(v) data.frame(Variable=v, n=sum(!is.na(d[[v]])),
  Mean=round(mean(d[[v]],na.rm=TRUE),3), SD=round(sd(d[[v]],na.rm=TRUE),3),
  Min=round(min(d[[v]],na.rm=TRUE),3), Q1=unname(round(quantile(d[[v]],.25,na.rm=TRUE),3)),
  Median=round(median(d[[v]],na.rm=TRUE),3), Q3=unname(round(quantile(d[[v]],.75,na.rm=TRUE),3)),
  Max=round(max(d[[v]],na.rm=TRUE),3),
  Pct_zero=round(100*mean(d[[v]]==0,na.rm=TRUE),1), stringsAsFactors=FALSE)))
emit("T3.6", "Small Area variable descriptives", desc)

cvars <- c("AI_Tier1_Rate","AI_Residential_Rate","AI_Youden_Rate","Census_Rate",
           "GeoDir_Matched_Rate","Mean_Linear_Score")
cp <- round(cor(d[,cvars], use="pairwise.complete.obs", method="pearson"),3)
cs <- round(cor(d[,cvars], use="pairwise.complete.obs", method="spearman"),3)
emit("T3.7a", "Pearson correlations between the measures", data.frame(Variable=rownames(cp), cp, check.names=FALSE))
emit("T3.7b", "Spearman correlations between the measures", data.frame(Variable=rownames(cs), cs, check.names=FALSE))

## ============ 9. GLOBAL REGRESSIONS (all printed this time) ============= ##
ols_rows <- list()
ols <- function(dv, ivs, tag) {
  m <- lm(as.formula(paste(dv,"~",paste(ivs,collapse=" + "))), data=d); s <- summary(m)
  for (nm in rownames(s$coefficients)) {
    ols_rows[[paste(tag,nm)]] <<- data.frame(Model=tag, Dependent=dv, Term=nm,
      Estimate=round(s$coefficients[nm,1],4), SE=round(s$coefficients[nm,2],4),
      t=round(s$coefficients[nm,3],3), p=format.pval(s$coefficients[nm,4],digits=4),
      Adj_R2=round(s$adj.r.squared,4), AIC=round(AIC(m),2), n=length(m$residuals),
      stringsAsFactors=FALSE)
  }
  invisible(m)
}
ols("Census_Rate", "AI_Residential_Rate", "D1 Census baseline")
ols("Census_Rate", c("AI_Residential_Rate","Percent_New_Imagery"), "D2 + imagery vintage")
ols("Census_Rate", c("AI_Residential_Rate","Observation_Coverage_Rate_Res"), "D3 + observation coverage")
ols("Census_Rate", c("AI_Residential_Rate","Percent_New_Imagery","Observation_Coverage_Rate_Res"), "D4 + both")
ols("GeoDir_Matched_Rate", "AI_Tier1_Rate", "E1 GeoDirectory baseline")
ols("GeoDir_Matched_Rate", c("AI_Tier1_Rate","Percent_New_Imagery"), "E2 + imagery vintage")
ols("Census_Rate", c("AI_Youden_Res_Rate","Percent_New_Imagery"), "F1 Youden vs Census")
ols("GeoDir_Matched_Rate", c("AI_Youden_Rate","Percent_New_Imagery"), "F2 Youden vs GeoDirectory")
emit("T3.9", "Global OLS regressions", do.call(rbind, ols_rows))
cat("\nNOTE: Observation_Coverage_Rate_Res is defined over RESIDENTIAL footprints only.\n")
cat("It is a legitimate control for the Census models (dwellings) but NOT for the\n")
cat("GeoDirectory models, whose denominator includes commercial delivery points.\n")
cat("Percent_New_Imagery is defined over all evaluable buildings and is used instead.\n")

## ======================= 10. LOCAL MORAN, BOTH KINDS =================== ##
# Univariate LISA. Version 7 uses localmoran_perm() and the SAME permutation
# p-value column as the fixed bivariate function, so the two are on the same
# inferential footing. The analytic p-value is reported alongside so the effect
# of the change is visible.
if (!exists("localmoran_perm", where=asNamespace("spdep"), mode="function"))
  stop("This spdep has no localmoran_perm(). Update spdep, or set the univariate ",
       "LISA back to the analytic p-value and say so in the write-up.")
uni_rows <- list(); uni_cmp <- list()
uni <- function(v, nm) {
  z  <- as.numeric(scale(v)); lz <- lag.listw(lw, z, zero.policy = TRUE)
  q  <- ifelse(z>0 & lz>0,"High-High", ifelse(z<0 & lz<0,"Low-Low",
        ifelse(z>0 & lz<0,"High-Low","Low-High")))
  li_a <- localmoran(v, lw, zero.policy = TRUE)
  p_a  <- p.adjust(li_a[, ncol(li_a)], method = "fdr")
  cl_a <- ifelse(p_a <= 0.05, q, "Not significant")
  lp <- localmoran_perm(v, lw, nsim = NSIM, zero.policy = TRUE)
  stopifnot(BV_PCOL %in% colnames(lp))
  p_p  <- p.adjust(as.numeric(lp[, BV_PCOL]), method = "fdr")
  cl_p <- ifelse(p_p <= 0.05, q, "Not significant")
  saf[[paste0("Cluster_", nm)]]        <<- cl_p     # permutation is now primary
  saf[[paste0("Cluster_", nm, "_ANA")]] <<- cl_a    # analytic kept for comparison
  ORD <- c("High-High","Low-Low","High-Low","Low-High","Not significant")
  uni_rows[[nm]] <<- data.frame(Variable=nm, Method="permutation (Pr(folded) Sim)",
    t(sapply(ORD, function(k) sum(cl_p==k))), Significant_total=sum(cl_p!="Not significant"),
    stringsAsFactors=FALSE)
  uni_cmp[[nm]] <<- data.frame(Variable=nm, Method="analytic (Pr(z != E(Ii)))",
    t(sapply(ORD, function(k) sum(cl_a==k))), Significant_total=sum(cl_a!="Not significant"),
    stringsAsFactors=FALSE)
}
uni(saf$AI_Tier1_Rate,"AI_Tier1"); uni(saf$AI_Residential_Rate,"AI_Residential")
uni(saf$AI_Youden_Rate,"AI_Youden"); uni(saf$Census_Rate,"Census"); uni(saf$GeoDir_Matched_Rate,"GeoDir")
uni(saf$Difference_AI_vs_Census,"Diff_AI_Census"); uni(saf$Difference_AI_vs_GeoDir,"Diff_AI_GeoDir")
ulist <- do.call(rbind, uni_rows); names(ulist)[3:7] <- c("High_High","Low_Low","High_Low","Low_High","Not_significant")
clist <- do.call(rbind, uni_cmp);  names(clist)[3:7] <- c("High_High","Low_Low","High_Low","Low_High","Not_significant")
emit("T3.11", "Univariate LISA cluster counts (permutation, primary)", ulist)
emit("T3.11b","Univariate LISA cluster counts (analytic, for comparison only)", clist)
cat("\nIf T3.11 and T3.11b differ materially, report the permutation version and say so.\n")

## ---------------------- BIVARIATE LISA (FIXED) -------------------------- ##
# FIXED 2026-09-07. localmoran_bv() returns a MATRIX, so the old
# is.list() && !is.matrix() && !is.data.frame() branch never fired and the code
# fell through to grep("^Pr|^p", colnames(r)), which grabs "Pr(z != E(Ibvi))",
# the analytic p-value. That ignored nsim entirely and made every bivariate
# column identical to the univariate column of the second variable. The correct
# column is "Pr(folded) Sim", the skewness-corrected permutation p-value.
bv_lisa <- function(x, y, lw, nsim = NSIM, pcol = BV_PCOL) {
  zx <- as.numeric(scale(x)); zy <- as.numeric(scale(y))
  ly <- lag.listw(lw, zy, zero.policy = TRUE); Ib <- zx * ly
  r <- spdep::localmoran_bv(x, y, lw, nsim = nsim, scale = TRUE)
  if (!pcol %in% colnames(r))
    stop("Column '", pcol, "' not found. Available: ", paste(colnames(r), collapse=" | "))
  data.frame(Ib=Ib, p=as.numeric(r[, pcol]), zx=zx, ly=ly, stringsAsFactors=FALSE)
}
bv_rows <- list()
run_bv <- function(xn, yn, field, label) {
  r <- bv_lisa(saf[[xn]], saf[[yn]], lw)
  q <- ifelse(r$zx>0 & r$ly>0,"High-High", ifelse(r$zx<0 & r$ly<0,"Low-Low",
       ifelse(r$zx>0 & r$ly<0,"High-Low","Low-High")))
  cl <- ifelse(p.adjust(r$p, method="fdr") <= 0.05, q, "Not significant")
  saf[[field]] <<- cl
  ORD <- c("High-High","Low-Low","High-Low","Low-High","Not significant")
  bv_rows[[label]] <<- data.frame(Pair=label, X=xn, Y_lagged=yn,
    t(sapply(ORD, function(k) sum(cl==k))), Significant_total=sum(cl!="Not significant"),
    Pct_of_n=round(100*mean(cl!="Not significant"),2), stringsAsFactors=FALSE)
  invisible(cl)
}
run_bv("AI_Residential_Rate","Census_Rate","Bivariate_Cluster_AI_Census","AI_Residential vs Census")
run_bv("AI_Tier1_Rate","GeoDir_Matched_Rate","Bivariate_Cluster_AI_GeoDir","AI_Tier1 vs GeoDirectory")
run_bv("Census_Rate","AI_Residential_Rate","Bivariate_Cluster_Census_AI","Census vs AI_Residential")
run_bv("GeoDir_Matched_Rate","AI_Tier1_Rate","Bivariate_Cluster_GeoDir_AI","GeoDirectory vs AI_Tier1")
bvt <- do.call(rbind, bv_rows); names(bvt)[4:8] <- c("High_High","Low_Low","High_Low","Low_High","Not_significant")
emit("T3.14", "Bivariate LISA, corrected (Pr(folded) Sim, nsim = NSIM)", bvt)
cat("\nREADING GUIDE. X is the local value, Y_lagged is the neighbourhood average.\n")
cat("  High-Low = the AI sees vacancy the register does not record nearby (unreported vacancy).\n")
cat("  Low-High = the register records vacancy the AI cannot see (AI blind spot).\n")

## ---- proof the fix is real: rerun with the WRONG column ---------------- ##
r_chk <- spdep::localmoran_bv(saf$AI_Residential_Rate, saf$Census_Rate, lw, nsim=NSIM, scale=TRUE)
kv("T1.3a", "Columns returned by spdep::localmoran_bv (proof of the bug)", list(
  Class = paste(class(r_chk), collapse=", "),
  Columns = paste(colnames(r_chk), collapse = " | "),
  Column_the_old_grep_selected = colnames(r_chk)[grep("^Pr|^p", colnames(r_chk), ignore.case=TRUE)[1]],
  Column_now_used = BV_PCOL))
zc <- as.numeric(scale(saf$AI_Residential_Rate)); lyc <- lag.listw(lw, as.numeric(scale(saf$Census_Rate)), zero.policy=TRUE)
qc <- ifelse(zc>0 & lyc>0,"High-High", ifelse(zc<0 & lyc<0,"Low-Low", ifelse(zc>0 & lyc<0,"High-Low","Low-High")))
wrong_col <- colnames(r_chk)[grep("^Pr|^p", colnames(r_chk), ignore.case=TRUE)[1]]
cl_wrong <- ifelse(p.adjust(as.numeric(r_chk[,wrong_col]), method="fdr") <= 0.05, qc, "Not significant")
cl_right <- ifelse(p.adjust(as.numeric(r_chk[,BV_PCOL]),   method="fdr") <= 0.05, qc, "Not significant")
ORD <- c("High-High","Low-Low","High-Low","Low-High","Not significant")
cnt <- function(cl) sapply(ORD, function(k) sum(cl == k))
emit("T1.3b", "Old (wrong) column versus new (correct) column, AI_Residential vs Census",
  data.frame(Column = c(wrong_col, BV_PCOL),
    High_High = c(cnt(cl_wrong)[1], cnt(cl_right)[1]),
    Low_Low   = c(cnt(cl_wrong)[2], cnt(cl_right)[2]),
    High_Low  = c(cnt(cl_wrong)[3], cnt(cl_right)[3]),
    Low_High  = c(cnt(cl_wrong)[4], cnt(cl_right)[4]),
    Not_significant = c(cnt(cl_wrong)[5], cnt(cl_right)[5]),
    Identical_to_univariate_analytic_Census =
      c(identical(as.character(cl_wrong), as.character(saf$Cluster_Census_ANA)),
        identical(as.character(cl_right), as.character(saf$Cluster_Census_ANA))),
    stringsAsFactors = FALSE))

## ========================== 11. GETIS-ORD Gi* ========================== ##
lwG <- nb2listw(include.self(nb), style="W", zero.policy=TRUE)
hot_method <- "analytic"
g <- as.numeric(localG(saf$AI_Residential_Rate, lwG, zero.policy=TRUE))
pg <- p.adjust(2*pnorm(-abs(g)), method="fdr")
gp <- try(localG_perm(saf$AI_Residential_Rate, lwG, nsim=NSIM, zero.policy=TRUE), silent=TRUE)
if (!inherits(gp,"try-error")) {
  ig <- attr(gp,"internals")
  if (!is.null(ig) && BV_PCOL %in% colnames(ig)) {
    pg <- p.adjust(as.numeric(ig[, BV_PCOL]), method="fdr"); g <- as.numeric(gp)
    hot_method <- paste0("permutation (", BV_PCOL, ")")
  }
}
saf$Hotspot_Z_Score <- g
saf$Hotspot_Classification <- ifelse(pg > 0.05, "Not significant",
  ifelse(g > 0, ifelse(pg <= 0.01,"Hot 99%","Hot 95%"), ifelse(pg <= 0.01,"Cold 99%","Cold 95%")))
ht <- as.data.frame(table(saf$Hotspot_Classification), stringsAsFactors=FALSE)
names(ht) <- c("Hotspot_Classification","n"); ht$Pct <- round(100*ht$n/nrow(saf),2)
emit("T3.10c", paste0("Getis-Ord Gi* on AI_Residential_Rate (", hot_method, ")"), ht)
kv("T3.10d","Gi* z-score range", list(Method=hot_method, Min_Z=fm(min(g),3), Max_Z=fm(max(g),3),
  NOTE="No cold spots is expected: the rates are floored at zero and right-skewed, so a strongly negative z is unreachable. See T3.6 Pct_zero."))

## ---- T3.12 / T3.13 cluster crosstabs ----------------------------------- ##
x1 <- as.data.frame.matrix(table(saf$Cluster_AI_Residential, saf$Cluster_Census))
emit("T3.12", "AI_Residential LISA class versus Census LISA class", data.frame(AI_Residential=rownames(x1), x1, check.names=FALSE))
x2 <- as.data.frame.matrix(table(saf$Cluster_AI_Tier1, saf$Cluster_GeoDir))
emit("T3.13", "AI_Tier1 LISA class versus GeoDirectory LISA class", data.frame(AI_Tier1=rownames(x2), x2, check.names=FALSE))

## ---- T3.15 disagreement surface ---------------------------------------- ##
dif <- data.frame(
  Surface = c("AI_Residential minus Census","AI_Tier1 minus GeoDirectory"),
  Mean = c(mean(saf$Difference_AI_vs_Census), mean(saf$Difference_AI_vs_GeoDir)),
  SD   = c(sd(saf$Difference_AI_vs_Census), sd(saf$Difference_AI_vs_GeoDir)),
  Min  = c(min(saf$Difference_AI_vs_Census), min(saf$Difference_AI_vs_GeoDir)),
  Max  = c(max(saf$Difference_AI_vs_Census), max(saf$Difference_AI_vs_GeoDir)),
  SAs_AI_exceeds = c(sum(saf$Difference_AI_vs_Census>0), sum(saf$Difference_AI_vs_GeoDir>0)),
  SAs_AI_falls_short = c(sum(saf$Difference_AI_vs_Census<0), sum(saf$Difference_AI_vs_GeoDir<0)),
  SAs_equal = c(sum(saf$Difference_AI_vs_Census==0), sum(saf$Difference_AI_vs_GeoDir==0)),
  stringsAsFactors=FALSE)
emit("T3.15", "The disagreement surfaces (Moran's I for these is in T3.8, LISA counts in T3.11)", dif)

## ============================= 12. EXPORT ============================== ##
st_write(saf, GPKG, "sa_results", delete_layer=TRUE, quiet=TRUE)
write.csv(st_drop_geometry(saf), file.path(OUT,"SA_results_Readable.csv"), row.names=FALSE)
write.csv(b[, intersect(c("GUID","Linear_Score","Vacancy_Tier_NoOut","Is_Residential",
  "AI_Vacant_By_Threshold","StreetView_Capture_Vintage","StreetView_Capture_Date",
  "GeoDir_Q3_2022_Vacant","GeoDir_Q4_2023_Vacant","GeoDir_Time_Matched_Vacant",
  "GeoDir_Persistent_Vacant","GeoDir_Newly_Vacant","Validation_Agreement"), names(b))],
  file.path(OUT,"Building_results_Readable.csv"), row.names=FALSE)

## ===================== 13. GWR, WITH FULL REPORTING ==================== ##
sp <- as(saf, "Spatial")
gwr_rows <- list(); gwr_coef_rows <- list()

run_gwr <- function(dv, ivs, layer, label) {
  f <- as.formula(paste(dv, "~", paste(ivs, collapse=" + ")))
  bw <- bw.gwr(f, data=sp, approach="AICc", kernel="bisquare", adaptive=TRUE)
  gw <- gwr.basic(f, data=sp, bw=bw, kernel="bisquare", adaptive=TRUE)
  # GWmodel has renamed diagnostic elements between versions. Resolve by trying
  # each known name and failing loudly rather than silently writing NA.
  dg <- function(diag, nms, what) {
    for (nn in nms) if (!is.null(diag[[nn]])) return(as.numeric(diag[[nn]]))
    stop("Could not find ", what, " in GW.diagnostic. Available: ",
         paste(names(diag), collapse=", "))
  }
  gl_fit <- lm(f, data=st_drop_geometry(saf)); gl <- summary(gl_fit)
  n_obs <- length(gl_fit$residuals); k_gl <- length(coef(gl_fit)) + 1   # +1 for sigma
  aicc_global <- AIC(gl_fit) + (2*k_gl*(k_gl+1))/(n_obs - k_gl - 1)
  S  <- gw$SDF@data
  DG <- gw$GW.diagnostic
  enp <- dg(DG, c("enp","edf.used","ENP"), "effective number of parameters")
  gwR2  <- dg(DG, c("gw.R2","gwR2"), "GWR R2")
  gwR2a <- dg(DG, c("gwR2.adj","gw.R2.adj","gwR2adj"), "GWR adjusted R2")
  aicc_gwr <- dg(DG, c("AICc","AICc.value"), "GWR AICc")
  k <- length(ivs) + 1
  # Byrne / da Silva / Fotheringham corrected critical value for multiple local tests
  alpha_adj <- 0.05 / (enp / k); tcrit_adj <- qt(1 - alpha_adj/2, nrow(saf) - enp)
  gwr_rows[[label]] <<- data.frame(Model=label, Dependent=dv,
    Independents=paste(ivs, collapse=" + "), Adaptive_bandwidth_NN=bw, Kernel="bisquare adaptive",
    Global_adj_R2=round(gl$adj.r.squared,4), GWR_R2=round(gwR2,4),
    GWR_adj_R2=round(gwR2a,4), Effective_parameters=round(enp,2),
    AICc_global=round(aicc_global,2), AICc_GWR=round(aicc_gwr,2),
    AICc_improvement=round(aicc_global - aicc_gwr,2),
    Local_R2_min=round(min(S$Local_R2),4), Local_R2_median=round(median(S$Local_R2),4),
    Local_R2_max=round(max(S$Local_R2),4),
    SAs_local_R2_below_0.10=sum(S$Local_R2<0.10), SAs_local_R2_above_0.50=sum(S$Local_R2>0.50),
    SAs_local_R2_negative=sum(S$Local_R2<0),
    t_critical_unadjusted=1.96, t_critical_BFC_adjusted=round(tcrit_adj,3), n=nrow(saf),
    stringsAsFactors=FALSE)
  for (iv in ivs) {
    tv <- S[[paste0(iv,"_TV")]]; co <- S[[iv]]
    if (is.null(tv) || is.null(co))
      stop("Expected columns '", iv, "' and '", iv, "_TV' in gw$SDF. Found: ",
           paste(names(S), collapse=", "))
    gwr_coef_rows[[paste(label,iv)]] <<- data.frame(Model=label, Term=iv,
      Min=round(min(co),4), Q1=unname(round(quantile(co,.25),4)), Median=round(median(co),4),
      Mean=round(mean(co),4), Q3=unname(round(quantile(co,.75),4)), Max=round(max(co),4),
      SAs_negative=sum(co<0), Pct_negative=round(100*mean(co<0),1),
      SAs_sig_t_1.96=sum(abs(tv)>1.96), Pct_sig_t_1.96=round(100*mean(abs(tv)>1.96),1),
      SAs_sig_BFC=sum(abs(tv)>tcrit_adj), Pct_sig_BFC=round(100*mean(abs(tv)>tcrit_adj),1),
      stringsAsFactors=FALSE)
  }
  cat("\n--- full GWmodel print for ", label, " ---\n", sep=""); print(gw)
  st_write(st_as_sf(gw$SDF), GPKG, layer, delete_layer=TRUE, quiet=TRUE)
  invisible(gw)
}
BANNER("T3.10", "Geographically weighted regression")
cat("NOTE ON CONTROLS. Observation_Coverage_Rate_Res is the share of RESIDENTIAL\n")
cat("footprints observed. GeoDirectory counts commercial delivery points as well,\n")
cat("so that variable is not a valid control for the GeoDirectory model and is NOT\n")
cat("used there. Percent_New_Imagery is ALSO not used in the GeoDirectory model: it\n")
cat("is highest in the city centre (where imagery happened to be recaptured), which\n")
cat("is also where vacancy is concentrated, so it acts as a proxy for location\n")
cat("rather than a genuine imagery-vintage control and was making itself look\n")
cat("spuriously significant. The GeoDirectory GWR is therefore run univariate.\n")
run_gwr("Census_Rate", c("AI_Residential_Rate","Observation_Coverage_Rate_Res"), "gwr_census", "M1 Census (coverage control)")
run_gwr("GeoDir_Matched_Rate", c("AI_Tier1_Rate"), "gwr_geodirectory", "M2 GeoDirectory (no control)")
run_gwr("Census_Rate", c("AI_Youden_Res_Rate","Observation_Coverage_Rate_Res"), "gwr_census_youden", "M3 Census, Youden measure")
run_gwr("GeoDir_Matched_Rate", c("AI_Youden_Rate"), "gwr_geodir_youden", "M4 GeoDirectory, Youden measure (no control)")
emit("T3.10a", "GWR model diagnostics", do.call(rbind, gwr_rows))
emit("T3.10b", "GWR local coefficient distributions and local significance", do.call(rbind, gwr_coef_rows))
cat("\nBFC = Byrne / da Silva / Fotheringham correction for dependent multiple local tests.\n")
cat("Map local coefficients masked at the BFC critical value, not at 1.96.\n")

## ======= 15. ROBUSTNESS: THE YOUDEN MEASURE THROUGH THE PIPELINE ======= ##
BANNER("T3.16", "Robustness: the unanchored Youden classification through the same pipeline")
cat("Every spatial result above uses the quota-based tiers, whose city totals are fixed\n")
cat("by construction to the GeoDirectory and Census rates. This block repeats the core\n")
cat("spatial analysis on AI_Youden_Rate, which is anchored only to the ground truth.\n")
cat("If the conclusions hold, they do not depend on the prevalence anchor.\n")
yc <- data.frame(
  Comparison = c("Moran's I","Correlation with Census_Rate (Pearson)",
                 "Correlation with GeoDir_Matched_Rate (Pearson)",
                 "Correlation with Census_Rate (Spearman)",
                 "Correlation with GeoDir_Matched_Rate (Spearman)",
                 "LISA significant SAs","City-wide rate (%)"),
  Quota_Tier1 = c(round(as.numeric(moran.test(saf$AI_Tier1_Rate,lw,zero.policy=TRUE)$estimate[1]),4),
    round(cor(saf$AI_Tier1_Rate,saf$Census_Rate),3), round(cor(saf$AI_Tier1_Rate,saf$GeoDir_Matched_Rate),3),
    round(cor(saf$AI_Tier1_Rate,saf$Census_Rate,method="spearman"),3),
    round(cor(saf$AI_Tier1_Rate,saf$GeoDir_Matched_Rate,method="spearman"),3),
    sum(saf$Cluster_AI_Tier1!="Not significant"), round(100*sum(b$Is_Tier1_Vacant)/n_eval,3)),
  Quota_Tier1_2_Res = c(round(as.numeric(moran.test(saf$AI_Residential_Rate,lw,zero.policy=TRUE)$estimate[1]),4),
    round(cor(saf$AI_Residential_Rate,saf$Census_Rate),3), round(cor(saf$AI_Residential_Rate,saf$GeoDir_Matched_Rate),3),
    round(cor(saf$AI_Residential_Rate,saf$Census_Rate,method="spearman"),3),
    round(cor(saf$AI_Residential_Rate,saf$GeoDir_Matched_Rate,method="spearman"),3),
    sum(saf$Cluster_AI_Residential!="Not significant"), round(100*sum(b$Is_Tier1_or_2_Vacant)/n_eval,3)),
  Youden_unanchored = c(round(as.numeric(moran.test(saf$AI_Youden_Rate,lw,zero.policy=TRUE)$estimate[1]),4),
    round(cor(saf$AI_Youden_Rate,saf$Census_Rate),3), round(cor(saf$AI_Youden_Rate,saf$GeoDir_Matched_Rate),3),
    round(cor(saf$AI_Youden_Rate,saf$Census_Rate,method="spearman"),3),
    round(cor(saf$AI_Youden_Rate,saf$GeoDir_Matched_Rate,method="spearman"),3),
    sum(saf$Cluster_AI_Youden!="Not significant"),
    round(100*sum(b$AI_Vacant_By_Threshold[b$Is_Evaluable==1],na.rm=TRUE)/n_eval,3)),
  stringsAsFactors=FALSE)
emit("T3.16", "Anchored tiers versus the unanchored Youden measure", yc)

## ==== 16. ROBUSTNESS: COVERAGE EXCLUSION FOR THE BIVARIATE LISA ======== ##
BANNER("T3.17", "Bivariate LISA under observation-coverage exclusion thresholds")
cov_rows <- list()
for (thr in c(0, 50, 70)) {
  keep <- saf$Observation_Coverage_Rate_Res >= thr
  s2 <- saf[keep, ]
  nb2 <- poly2nb(s2, queen = TRUE); isl <- which(card(nb2) == 0)
  n_isl <- length(isl)
  if (n_isl > 0) { s2 <- s2[-isl, ]; nb2 <- poly2nb(s2, queen = TRUE) }
  lw2 <- nb2listw(nb2, style="W", zero.policy=TRUE)
  for (pr in list(c("AI_Residential_Rate","Census_Rate","AI vs Census"),
                  c("AI_Tier1_Rate","GeoDir_Matched_Rate","AI vs GeoDir"),
                  c("Census_Rate","AI_Residential_Rate","Census vs AI"),
                  c("GeoDir_Matched_Rate","AI_Tier1_Rate","GeoDir vs AI"))) {
    r <- bv_lisa(s2[[pr[1]]], s2[[pr[2]]], lw2)
    q <- ifelse(r$zx>0 & r$ly>0,"High-High", ifelse(r$zx<0 & r$ly<0,"Low-Low",
         ifelse(r$zx>0 & r$ly<0,"High-Low","Low-High")))
    cl <- ifelse(p.adjust(r$p, method="fdr") <= 0.05, q, "Not significant")
    cov_rows[[paste(thr,pr[3])]] <- data.frame(Coverage_threshold_pct=thr, Pair=pr[3],
      n=nrow(s2), Dropped_low_coverage=sum(!keep), Dropped_new_islands=n_isl,
      High_High=sum(cl=="High-High"), Low_Low=sum(cl=="Low-Low"),
      High_Low=sum(cl=="High-Low"), Low_High=sum(cl=="Low-High"),
      Significant_total=sum(cl!="Not significant"),
      Pct_significant=round(100*mean(cl!="Not significant"),2), stringsAsFactors=FALSE)
  }
}
emit("T3.17", "Bivariate LISA sensitivity to observation coverage", do.call(rbind, cov_rows))
cat("\nCAVEAT: raising the threshold changes two things at once, the reliability of the\n")
cat("retained rates AND the comparison population and neighbour network. Some of the\n")
cat("movement at 70% reflects a sparser, more fragmented map, not a coverage effect.\n")

## ================= 14. CENSUS CORRELATION EXPLORATION ================= ##
census_full <- st_read(GDB, layer=L_SA, quiet=TRUE) |> st_drop_geometry()
census_full$Small_Area_ID <- as.character(census_full$SA_GUID_2022)
joined_data <- st_drop_geometry(saf) |> left_join(census_full, by="Small_Area_ID")
numeric_data <- joined_data |> select(where(is.numeric))
cor_matrix <- cor(numeric_data, use="pairwise.complete.obs", method="spearman")
cor_for <- function(target) {
  cr <- data.frame(Census_Variable=rownames(cor_matrix),
                   Correlation_Strength=cor_matrix[, target], stringsAsFactors=FALSE)
  cr <- cr |> filter(grepl("^T\\d+_", Census_Variable)) |> arrange(desc(abs(Correlation_Strength)))
  if (file.exists(GLOSSARY)) {
    glossary <- readxl::read_excel(GLOSSARY)
    colnames(glossary)[3] <- "Census_Variable"; colnames(glossary)[4] <- "Description"
    cr <- cr |> left_join(glossary[, c("Census_Variable","Description")], by="Census_Variable")
  } else cr$Description <- "Glossary file not found"
  cr$Target <- target; cr
}
cr_res <- cor_for("AI_Residential_Rate")
cr_yj  <- cor_for("AI_Youden_Rate")
cr_cen <- cor_for("Census_Rate")
cr_gd  <- cor_for("GeoDir_Matched_Rate")
emit("T3.18", "Top 20 Census correlates of AI_Residential_Rate",
     head(cr_res[, c("Census_Variable","Description","Correlation_Strength")], 20))
emit("T3.18b", "Top 20 Census correlates of AI_Youden_Rate (robustness)",
     head(cr_yj[, c("Census_Variable","Description","Correlation_Strength")], 20))
emit("T3.18c", "Top 10 Census correlates of Census_Rate and GeoDir_Matched_Rate (for contrast)",
     rbind(head(cr_cen[, c("Target","Census_Variable","Description","Correlation_Strength")], 10),
           head(cr_gd[,  c("Target","Census_Variable","Description","Correlation_Strength")], 10)))
write.csv(rbind(cr_res, cr_yj, cr_cen, cr_gd), file.path(OUT,"Census_Correlations_Ranked.csv"), row.names=FALSE)

## ============ 17. FIELD VALIDATION SAMPLE SIZES AND DRAW =============== ##
n_req <- function(N, p=0.5, e=0.10, z=1.96) ceiling((z^2*p*(1-p)/e^2)/(1+((z^2*p*(1-p)/e^2)-1)/N))
strata <- list("Visible only"= ev$Validation_Agreement=="Visible only",
               "Recorded only"=ev$Validation_Agreement=="Recorded only",
               "Both"=ev$Validation_Agreement=="Both")
ss <- do.call(rbind, lapply(names(strata), function(s) {
  N <- sum(strata[[s]])
  data.frame(Stratum=s, N=N, n_for_10pct=n_req(N,0.5,0.10), n_for_7.5pct=n_req(N,0.5,0.075),
             n_for_5pct=n_req(N,0.5,0.05), n_for_10pct_if_p75=n_req(N,0.75,0.10),
             stringsAsFactors=FALSE) }))
emit("T5.1", "Field validation sample sizes (95% CI, finite population correction)", ss)
set.seed(20260905)
draw <- do.call(rbind, lapply(c("Visible only","Both"), function(s) {
  idx <- which(ev$Validation_Agreement == s)
  take <- sample(idx, min(n_req(length(idx),0.5,0.10), length(idx)))
  data.frame(Stratum=s, GUID=ev$GUID[take], Linear_Score=ev$Linear_Score[take],
             Tier=ev$Vacancy_Tier_NoOut[take], Vintage=ev$StreetView_Capture_Vintage[take],
             stringsAsFactors=FALSE) }))
xy <- st_coordinates(bpt)[match(draw$GUID, bpt$GUID), ]
draw$X_ITM <- round(xy[,1],2); draw$Y_ITM <- round(xy[,2],2)
write.csv(draw, file.path(OUT,"Field_Validation_Sample.csv"), row.names=FALSE)
cat("\nSeeded field sample written to Field_Validation_Sample.csv:", nrow(draw), "buildings.\n")
cat("Draw it BEFORE fieldwork and do not redraw. Record a three-level outcome\n")
cat("(clearly vacant / clearly occupied / cannot tell) and report the 'cannot tell' rate.\n")
print(table(draw$Stratum))

cat("\n\n", strrep("=",78), "\nRUN COMPLETE.\n", sep="")
cat("Console log : ", LOGFILE, "\n", sep="")
cat("Table CSVs  : ", file.path(OUT,"tables"), "\n", sep="")
cat("GeoPackage  : ", GPKG, "\n", sep="")
cat("Send the console log and the tables/ folder to fill the results document.\n")
sink()
