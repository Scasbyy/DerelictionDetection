## ============================================================================
##  AI SUBJECTIVE VACANCY SCORE vs TIER 1 / TIER 2 CLASSIFICATION
##  Shea Casby, MSc Geoinformatics, University College Cork
##
##  Your Tier 1 / Tier 2 classification (Vacancy_Tier_NoOut) is built from the
##  20 physical indicators via the Firth model + quota tiering. `vacancy_score`
##  is a DIFFERENT number: the AI's own single, holistic 0-5 opinion, given
##  directly in the same VLM pass ("vacancy_score: Integer from 0 to 5 ...",
##  0 = active/occupied, 2 = early/unsecured vacancy, 3 = secured vacancy,
##  4 = severe decay, 5 = catastrophic collapse).
##
##  This script checks whether that independent opinion agrees with your tier
##  assessment, in two ways:
##    PART A - Overall: your Tier 1+2 vs "AI opinion >= 2" (early-vacancy or
##             worse), as a single agreement/correlation figure.
##
##  CORRECTION (applied after this script was first written): the threshold
##  is >= 2, not > 2. vacancy_score 2 is itself "early/unsecured vacancy" -
##  a vacancy category, not a not-vacant one - so score 2 belongs on the
##  vacant side of the split. Every AI_Vacant definition, print label and
##  plot annotation below has been updated to >= 2 to match.
##    PART B - Severity-separated: Tier 1 and Tier 2 grade severity, and so
##             does the 0-5 AI score, so this checks whether the two severity
##             gradients line up (Tier 1 buildings should draw higher AI scores
##             than Tier 2, which should draw higher scores than Tier 3).
##
##  Outbuildings, and buildings the AI could not evaluate (occluded: its score
##  is a forced default there, not a real opinion), are excluded from both
##  parts and reported separately rather than folded into "not vacant" -
##  smoothing those in would bias the agreement figures.
##
##  Tags in this script are [AIC.x] (AI Comparison), a fresh namespace so they
##  don't collide with the [A]/[V]/[M]/[T] tags used elsewhere.
##
##  Requires: dplyr, readr, tidyr        Run time: seconds
## ============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr)
})

## ------------------------------ 0. CONFIG --------------------------------- ##
CSV          <- "C:/Users/sheac/Documents/College/Dissertation/P2_GeminiResults_rank3_point_short_res.csv"
OUT_DIR      <- "C:/Users/sheac/Documents/College/Dissertation/Claude/AI_Opinion_Comparison_Tables"
AI_THRESHOLD <- 2   # "AI opinion" counts as agreeing-vacant when vacancy_score >= this

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)
hdr <- function(x) cat("\n\n===========  ", x, "  ===========\n")

## ============================================================================
## 1. LOAD AND PREPARE
## ============================================================================
hdr("[AIC.0] LOAD")

b <- read.csv(CSV, stringsAsFactors = FALSE)
stopifnot("Vacancy_Tier_NoOut" %in% names(b), "vacancy_score" %in% names(b))

id_col <- intersect(c("GUID", "Bldg_GUID"), names(b))[1]
keep <- c(id_col, "Vacancy_Tier_NoOut", "vacancy_score")
if ("has_target_building" %in% names(b)) keep <- c(keep, "has_target_building")
b <- b[, keep]

cat(sprintf("[AIC.0] rows loaded: %d\n", nrow(b)))

## Same convention used throughout the rest of the analysis: the leading
## character of Vacancy_Tier_NoOut is the tier ("1","2","3","4" or "E" for
## excluded outbuilding) - see Vacancy_Comparison_Analysis_T1Geo.R.
b$tier <- substr(as.character(b$Vacancy_Tier_NoOut), 1, 1)
cat("[AIC.0] Vacancy_Tier_NoOut, by leading tier code:\n")
print(table(b$tier, useNA = "ifany"))

n_outbuilding <- sum(b$tier == "E", na.rm = TRUE)
n_occluded    <- sum(b$tier == "4", na.rm = TRUE)
cat(sprintf("\n[AIC.0] excluding %d outbuildings and %d occluded/unevaluable buildings.\n",
            n_outbuilding, n_occluded))
cat("[AIC.0] occluded buildings are excluded (not folded into 'not vacant'):\n")
cat("        the AI has no real opinion on a building it never saw - vacancy_score\n")
cat("        is a forced default there, not a judgement.\n")

d <- b %>% filter(tier %in% c("1", "2", "3"))

## Data-integrity check: an evaluable building should have had a real target
## to score. Flag rather than silently trust it.
if ("has_target_building" %in% names(d)) {
  bad <- d$has_target_building %in% c(0, FALSE, "False", "FALSE", "0")
  if (any(bad, na.rm = TRUE)) {
    cat(sprintf("\n[AIC.0] FLAG: %d Tier 1-3 rows have has_target_building = FALSE ",
                sum(bad, na.rm = TRUE)))
    cat("(vacancy_score there is not a real opinion) - dropping them.\n")
    d <- d[!bad, ]
  }
}

n_na_score <- sum(is.na(d$vacancy_score))
if (n_na_score > 0) {
  cat(sprintf("\n[AIC.0] FLAG: %d evaluable rows have a missing vacancy_score - dropping them.\n",
              n_na_score))
  d <- d %>% filter(!is.na(vacancy_score))
}

d$Tier <- as.integer(d$tier)
cat(sprintf("\n[AIC.0] comparison set: %d buildings (Tier 1: %d, Tier 2: %d, Tier 3: %d)\n",
            nrow(d), sum(d$Tier == 1), sum(d$Tier == 2), sum(d$Tier == 3)))

write_csv(d %>% count(Tier, vacancy_score) %>% arrange(Tier, vacancy_score),
          file.path(OUT_DIR, "AIC0_raw_crosstab.csv"))

## ============================================================================
## PART A. OVERALL AGREEMENT: TIER 1+2  vs  AI OPINION > 2
## ============================================================================
hdr("[AIC.1] OVERALL AGREEMENT - TIER 1+2 vs AI OPINION")

d <- d %>%
  mutate(
    My_Vacant = factor(Tier %in% c(1, 2), levels = c(FALSE, TRUE)),   # your Tier 1+2 call
    AI_Vacant = factor(vacancy_score >= AI_THRESHOLD, levels = c(FALSE, TRUE))  # AI's own opinion
  )

ct <- table(My = d$My_Vacant, AI = d$AI_Vacant)
cat(sprintf("Contingency table (rows = your Tier 1+2 assessment, cols = AI opinion >= %d):\n",
            AI_THRESHOLD))
print(ct)
write.csv(as.data.frame(ct), file.path(OUT_DIR, "AIC1_contingency.csv"), row.names = FALSE)

## Cast to double before any cross-product - the Analysis 8 2x2 stats hit a
## 32-bit integer overflow at this kind of n and silently returned NA.
a  <- as.numeric(ct["TRUE",  "TRUE"])   # both say vacant
bb <- as.numeric(ct["TRUE",  "FALSE"])  # you say vacant, AI does not
cc <- as.numeric(ct["FALSE", "TRUE"])   # AI says vacant, you do not
dd <- as.numeric(ct["FALSE", "FALSE"])  # both say not vacant
n  <- a + bb + cc + dd

zero_cell <- any(c(a, bb, cc, dd) == 0)
if (zero_cell) {
  cat("\n[AIC.1] FLAG: at least one cell of the 2x2 table is zero - the odds ratio\n")
  cat("        below is Haldane-Anscombe corrected (+0.5 to every cell); treat it\n")
  cat("        as approximate, not as a clean estimate.\n")
}
a_or <- a; bb_or <- bb; cc_or <- cc; dd_or <- dd
if (zero_cell) { a_or <- a + 0.5; bb_or <- bb + 0.5; cc_or <- cc + 0.5; dd_or <- dd + 0.5 }

po <- (a + dd) / n
pe <- ((a + bb) / n) * ((a + cc) / n) + ((cc + dd) / n) * ((bb + dd) / n)
kappa       <- (po - pe) / (1 - pe)
pabak       <- 2 * po - 1
phi         <- suppressWarnings(cor(as.numeric(d$My_Vacant) - 1, as.numeric(d$AI_Vacant) - 1))
jaccard     <- a / (a + bb + cc)
odds_ratio  <- (a_or * dd_or) / (bb_or * cc_or)
mcnemar     <- mcnemar.test(ct, correct = TRUE)
sensitivity <- a / (a + bb)   # of your Tier1+2, % the AI also calls >= 2
specificity <- dd / (cc + dd) # of your Tier3, % the AI also calls < 2
ppv         <- a / (a + cc)   # of AI-flagged buildings, % also your Tier1+2

cat(sprintf("\nn                                   : %.0f\n", n))
cat(sprintf("Percent agreement                  : %.2f%%\n", 100 * po))
cat(sprintf("Cohen's kappa                      : %.4f\n", kappa))
cat(sprintf("PABAK                              : %.4f\n", pabak))
cat(sprintf("Phi coefficient / correlation (r)  : %.4f\n", phi))
cat(sprintf("Jaccard index                      : %.4f\n", jaccard))
cat(sprintf("Odds ratio%s                : %.3f\n", ifelse(zero_cell, " (corrected)", "            "), odds_ratio))
cat(sprintf("McNemar chi-sq (df=1)              : %.3f, p = %s\n",
            mcnemar$statistic, format.pval(mcnemar$p.value, digits = 3, eps = 2.2e-16)))
cat(sprintf("Sensitivity (AI catches your T1+2)  : %.2f%%\n", 100 * sensitivity))
cat(sprintf("Specificity (AI agrees on your T3)  : %.2f%%\n", 100 * specificity))
cat(sprintf("PPV (AI-flagged that are your T1+2) : %.2f%%\n", 100 * ppv))

overall_stats <- tibble(
  n = n, threshold = AI_THRESHOLD, percent_agreement = po, kappa = kappa, pabak = pabak,
  phi_correlation = phi, jaccard = jaccard, odds_ratio = odds_ratio,
  odds_ratio_corrected = zero_cell,
  mcnemar_chisq = unname(mcnemar$statistic), mcnemar_p = mcnemar$p.value,
  sensitivity = sensitivity, specificity = specificity, ppv = ppv
)
write_csv(overall_stats, file.path(OUT_DIR, "AIC1_overall_agreement_stats.csv"))

## ============================================================================
## PART B. SEVERITY-SEPARATED: DOES THE AI'S 0-5 GRADIENT MATCH TIER 1 vs 2 vs 3?
## ============================================================================
hdr("[AIC.2] SEVERITY - AI SCORE ACROSS TIER 1 / TIER 2 / TIER 3")

d$Tier_Label <- factor(recode(d$Tier, `1` = "Tier 1", `2` = "Tier 2", `3` = "Tier 3"),
                        levels = c("Tier 1", "Tier 2", "Tier 3"))

sev_summary <- d %>%
  group_by(Tier_Label) %>%
  summarise(
    n = n(),
    mean_ai_score   = mean(vacancy_score),
    median_ai_score = median(vacancy_score),
    sd_ai_score     = sd(vacancy_score),
    pct_ai_at_or_above_threshold = 100 * mean(vacancy_score >= AI_THRESHOLD),
    .groups = "drop"
  )
cat("AI subjective score, summarised by your tier:\n"); print(as.data.frame(sev_summary))
write_csv(sev_summary, file.path(OUT_DIR, "AIC2_severity_by_tier_summary.csv"))

## Full 0-5 distribution by tier - not just the mean, so nothing is smoothed away.
dist_tbl <- d %>%
  count(Tier_Label, vacancy_score) %>%
  pivot_wider(names_from = vacancy_score, values_from = n, values_fill = 0,
              names_prefix = "score_")
cat("\nFull AI score distribution by tier:\n"); print(as.data.frame(dist_tbl))
write_csv(dist_tbl, file.path(OUT_DIR, "AIC2_severity_by_tier_full_distribution.csv"))

## Does the AI's opinion differ across the three tiers at all?
kw <- kruskal.test(vacancy_score ~ Tier_Label, data = d)
cat(sprintf("\n[AIC.2] Kruskal-Wallis (AI score ~ Tier): chi-sq = %.3f, df = %d, p = %s\n",
            kw$statistic, kw$parameter, format.pval(kw$p.value, digits = 3, eps = 2.2e-16)))

## Which specific tiers differ - in particular, is Tier 1's AI opinion more
## severe than Tier 2's, as the severity ordering would predict?
pw <- pairwise.wilcox.test(d$vacancy_score, d$Tier_Label, p.adjust.method = "holm")
cat("\n[AIC.2] Pairwise Wilcoxon (Holm-adjusted p-values):\n"); print(pw$p.value)
write.csv(as.data.frame(pw$p.value), file.path(OUT_DIR, "AIC2_pairwise_wilcoxon.csv"))

## Ordinal correlation: recode tier as a severity rank running in the SAME
## direction as vacancy_score (higher = more severe: Tier 1 = 3, Tier 2 = 2,
## Tier 3 = 1), then correlate the two severity gradients directly.
d$Tier_Severity_Rank <- recode(d$Tier, `1` = 3, `2` = 2, `3` = 1)
rho <- suppressWarnings(cor.test(d$Tier_Severity_Rank, d$vacancy_score,
                                  method = "spearman", exact = FALSE))
tau <- suppressWarnings(cor.test(d$Tier_Severity_Rank, d$vacancy_score, method = "kendall"))

cat(sprintf("\n[AIC.2] Spearman rho (your tier severity vs AI score): %.4f, p = %s\n",
            rho$estimate, format.pval(rho$p.value, digits = 3, eps = 2.2e-16)))
cat(sprintf("[AIC.2] Kendall tau  (your tier severity vs AI score) : %.4f, p = %s\n",
            tau$estimate, format.pval(tau$p.value, digits = 3, eps = 2.2e-16)))

ordinal_stats <- tibble(
  kruskal_chisq = unname(kw$statistic), kruskal_df = unname(kw$parameter), kruskal_p = kw$p.value,
  spearman_rho = unname(rho$estimate), spearman_p = rho$p.value,
  kendall_tau = unname(tau$estimate), kendall_p = tau$p.value
)
write_csv(ordinal_stats, file.path(OUT_DIR, "AIC2_severity_ordinal_correlation.csv"))

## Quick visual: does the AI score distribution actually step down Tier1 > Tier2 > Tier3?
png(file.path(OUT_DIR, "AIC2_boxplot_ai_score_by_tier.png"), width = 900, height = 650, res = 120)
boxplot(vacancy_score ~ Tier_Label, data = d,
        main = "AI subjective vacancy_score by Tier (outbuildings and occluded excluded)",
        xlab = "Your tier", ylab = "AI vacancy_score (0-5)", col = c("#c0392b", "#e67e22", "#95a5a6"))
abline(h = AI_THRESHOLD - 0.5, lty = 2, col = "grey40")
dev.off()

hdr("DONE")
cat(sprintf("Tables and figure written to: %s\n", OUT_DIR))
