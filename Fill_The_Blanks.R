###############################################################################
#  FILL THE BLANKS
#  Computes every outstanding number in "Methods and Results Structure.docx"
#  that can be computed from the files already on disk.
#
#  Shea Casby, MSc Geoinformatics, University College Cork.
#
#  HOW TO USE
#    1. Set the paths in section 0. Anything you cannot find, leave as "".
#       A block whose inputs are missing prints a SKIP line naming the exact
#       file it wanted and moves on. Nothing here stops on a missing input.
#    2. Run the whole file. It writes:
#         <OUT>/Fill_The_Blanks_Log.txt     the full console transcript
#         <OUT>/tables/F1_2.csv  etc.       one CSV per numbered block
#    3. Every block prints a "FILLS:" line naming the exact heading, table or
#       placeholder in the structure document that it answers. Search the log
#       for "FILLS:" to get the checklist.
#
#  CONVENTION
#    Block tags are [F<section>.<block>], deliberately in a different namespace
#    from the Analysis 8 [T...] tags so the two logs never collide.
#
#  DEPENDENCIES
#    Base R for everything except: logistf, pROC (already used by
#    05_FINAL_MODEL.R) and readxl (only for the .xls metadata file).
#
#  WHAT IT DOES NOT DO
#    It does not invent, interpolate or carry forward any figure. Where a
#    number depends on a reporting choice (an "extreme" field of view, a
#    keyword list for occlusion causes) the choice is a named constant in
#    section 0 and the block says so in its output.
#
#  BLOCK INDEX
#    F0.1  input inventory                          Appendix D, what is runnable
#    F1.1  human rated sample size and provenance    4.4.2 NUMBER NEEDED, 3.11.2
#    F1.2  TABLE 4.4 per indicator agreement         4.4.2 TABLE 4.4
#    F1.3  overall and binary agreement              4.4.2
#    F1.4  direction of disagreement                 4.4.2
#    F1.5  indicators too rare to assess             4.4.2
#    F1.6  TABLE 4.5 separation contingencies        4.4.3 TABLE 4.5
#    F1.7  boarded_up precision against truth        3.10.8
#    F2.1  TABLE 4.5.1 model comparison              4.5.1 three [confirm] cells
#    F2.2  DeLong tests                              4.5.1 NUMBER NEEDED
#    F2.3  deployed coefficients, CI, p, odds ratio  4.5.2
#    F2.4  worked example X and Y                    4.5.3
#    F2.5  overgrown_grounds in the ungrouped model  4.5.4 [CHECK]
#    F2.6  cross validation reconciliation           4.5.5 [RECONCILE], D.2
#    F2.7  threshold selection bootstrap             4.5.5
#    F2.8  events per variable                       D.1 item 15
#    F2.9  Youden deployment arithmetic              3.10.7 [CHECK]
#    F3.1  score and tier reconstruction, cross check 4.6.3
#    F3.2  field of view and camera distance         4.2.5
#    F3.3  occlusion causes from the rationale text  4.2.4
#    F3.4  FUNC_ID profile of flagged buildings      4.6.5 NUMBER NEEDED
#    F3.5  composition of the top 100, 200, 500      4.6.2
#    F3.6  ranking with and without outbuildings     4.6.2
#    F3.7  score distribution by tier                4.6.1
#    F3.8  zero indicator share and index profiles   4.6.1
#    F3.9  large building run                        D.2
#    F4.1  viable properties, lowest score quartile  4.7.5 NUMBER NEEDED
#    F4.2  TABLE 4.1 capture funnel                  4.2.2 TABLE 4.1
#    F4.3  non capture reasons                       4.2.3, D.2
#    F5.1  field verification sample design          4.9
#    F5.2  field verification results                4.9 RESULTS OUTSTANDING
#    F5.3  derelict sites register match rate        D.3
#    F5.4  resolution sensitivity                    3.11.4, D.2
#    F9.1  what could not be computed and why        Appendix D
###############################################################################


## ============================ 0. CONFIGURATION ========================== ##

DIS <- "C:/Users/sheac/Documents/College/Dissertation"
CLA <- file.path(DIS, "Claude")
DOC <- "C:/Users/sheac/Documents/Claude"

OUT <- file.path(DIS, "Analysis9_FillTheBlanks")

## Inputs. Set to "" if you do not have the file and the block will skip.
F_GT      <- file.path(CLA, "GROUND_TRUTH_MASTER.csv")
F_HUMAN   <- file.path(DOC, "Human_Ground_Truth_2026-09-01 (1).csv")
F_CITY    <- file.path(DIS, "August/GeminiResults2.csv")   # 60,090 VLM rows
F_META    <- file.path(DIS, "August/P2_Gemini_rank.xls")   # FUNC_ID, geometry
F_RANK3   <- file.path(DIS, "P2_GeminiResults_rank3_point_short_res.csv")
F_BLDG    <- file.path(DIS, "Analysis8/Building_results_T1Geo_Readable.csv")
F_JOIN    <- file.path(CLA, "ArcGIS_Vacancy_Join_boost.csv")
F_BIG     <- ""   # large building extraction, one row per image
F_TRACK   <- "G:/My Drive/StreetView/ColstoPulls/GSV_Extraction_Tracker.csv"   # ...GSV_Extraction_Tracker.csv  (59,783 rows)
F_REVIEW  <- "G:/My Drive/StreetView/ColstoPulls/Requires_Review.csv"   # ...Requires_Review.csv         (2,598 rows)
F_FIELDS  <- ""   # Field_Validation_Sample.csv    (the 155 drawn)
F_FIELDR  <- ""   # Field_Validation_Results.csv   (fill after the visits)
F_DSR     <- ""   # Derelict Sites Register, needs a Bldg_GUID or coordinates
F_RESOL   <- "G:/My Drive/StreetView/FOConnor_Resolution_Discrepancies.csv"  # FOConnor_Resolution_Discrepancies.csv

## Analytical constants. These must match 05_FINAL_MODEL.R.
TARGET_GEO    <- 0.01705
TARGET_CEN    <- 0.05555
PRIORITY_IND  <- "boarded_up"
MAX_OCC       <- 2
DROP_FOCONNOR <- TRUE
CV_SEED       <- 42        # same seed as 05_FINAL_MODEL.R, folds are comparable
BOOT_SEED     <- 20260908
N_BOOT        <- 2000

## Reporting choices, not derived quantities. Change them and say so in text.
FOV_NARROW    <- 20        # degrees, below this counts as a narrow crop
FOV_WIDE      <- 90        # degrees, above this counts as a wide crop
RARE_MIN      <- 5         # marginal count below which kappa is called unstable
TOPN          <- c(100, 200, 500)
N_EVAL        <- 53436     # evaluable buildings, used by F2.9. F3.1 checks it.

## Keyword sets for the occlusion cause classification in F3.3.
OCC_PATTERNS <- list(
  vegetation = "veget|tree|hedge|bush|shrub|foliage|overgrow|ivy|branch|leaf|leaves",
  vehicle    = "vehicle|\\bcar\\b|\\bvan\\b|truck|lorry|\\bbus\\b|parked",
  barrier    = "wall|fence|gate|hoard|railing|barrier|screen",
  built      = "building|structure|obstruct|block(ed|ing)?|adjacent|neighbour",
  framing    = "angle|oblique|off.?centre|off.?center|framing|cropped|edge of",
  distance   = "distan|too far|small|resolution|blurr|unclear|indistinct",
  scaffold   = "scaffold|sheeting|tarpaulin|construction"
)


## ============================= 1. SETUP ================================= ##

options(stringsAsFactors = FALSE, width = 130)
## If anything below does fail, close the transcript so the console comes back.
options(error = function() { try(while (sink.number() > 0) sink(), silent = TRUE) })
dir.create(file.path(OUT, "tables"), recursive = TRUE, showWarnings = FALSE)

LOGFILE <- file.path(OUT, "Fill_The_Blanks_Log.txt")
sink(LOGFILE, split = TRUE)

cat("###############################################################\n")
cat("#  FILL THE BLANKS\n")
cat("#  run at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", sep = "")
cat("#  R ", R.version.string, "\n", sep = "")
cat("#  output folder ", OUT, "\n", sep = "")
cat("###############################################################\n")

have_pkg <- function(p) requireNamespace(p, quietly = TRUE)
PKG_LOGISTF <- have_pkg("logistf")
PKG_PROC    <- have_pkg("pROC")
PKG_READXL  <- have_pkg("readxl")
cat("\npackages: logistf ", PKG_LOGISTF, " | pROC ", PKG_PROC,
    " | readxl ", PKG_READXL, "\n", sep = "")
if (PKG_LOGISTF) suppressPackageStartupMessages(library(logistf))
if (PKG_PROC)    suppressPackageStartupMessages(library(pROC))

## ---- helpers ---------------------------------------------------------- ##

hascols <- function(df, cols) all(cols %in% names(df))

exists_file <- function(...) {
  p <- c(...)
  length(p) > 0 && all(nzchar(p)) && all(file.exists(p))
}

rd <- function(path) {
  ## read.csv that survives a UTF-8 byte order mark on any platform.
  ## fileEncoding = "UTF-8-BOM" is not portable, so the BOM is stripped by hand
  ## and only when it is actually there, which keeps large files fast.
  con <- file(path, "rb"); b <- readBin(con, "raw", 3L); close(con)
  has_bom <- length(b) == 3L && identical(as.integer(b), c(239L, 187L, 191L))
  out <- if (has_bom) {
    tx <- readLines(path, warn = FALSE, encoding = "UTF-8")
    tx[1] <- sub("^\uFEFF", "", tx[1])
    read.csv(text = tx, stringsAsFactors = FALSE)
  } else {
    read.csv(path, stringsAsFactors = FALSE)
  }
  ## belt and braces: R sometimes renders a surviving BOM as an X... prefix
  names(out)[1] <- sub("^(X\\.\\.\\.|\uFEFF)", "", names(out)[1])
  out
}

num0 <- function(x) { x <- suppressWarnings(as.numeric(x)); x[is.na(x)] <- 0; x }

rs <- function(d, cols) {
  cols <- cols[cols %in% names(d)]
  if (!length(cols)) return(rep(0, nrow(d)))
  rowSums(as.data.frame(lapply(d[, cols, drop = FALSE], num0)))
}

find_col <- function(df, patterns) {
  for (p in patterns) {
    hit <- grep(p, names(df), ignore.case = TRUE, value = TRUE)
    if (length(hit)) return(hit[1])
  }
  NA_character_
}

blk <- function(tag, title, fills) {
  cat("\n\n")
  cat(strrep("=", 78), "\n", sep = "")
  cat("### [", tag, "] ", title, " ###\n", sep = "")
  cat("FILLS: ", fills, "\n", sep = "")
  cat(strrep("=", 78), "\n", sep = "")
}

skipblk <- function(tag, title, fills, why) {
  blk(tag, title, fills)
  cat("SKIPPED. ", why, "\n", sep = "")
}

emit <- function(tag, df, note = NULL, digits = 4) {
  f <- file.path(OUT, "tables", paste0(gsub("[.]", "_", tag), ".csv"))
  write.csv(df, f, row.names = FALSE)
  p <- df
  for (j in seq_along(p)) if (is.numeric(p[[j]])) p[[j]] <- round(p[[j]], digits)
  print(p, row.names = FALSE)
  if (!is.null(note)) cat("\nNOTE: ", note, "\n", sep = "")
  cat("  -> ", f, "\n", sep = "")
  invisible(df)
}

pct <- function(x, n) { r <- 100 * x / n; r[!is.finite(r)] <- NA_real_; r }

wilson <- function(x, n, conf = 0.95) {
  if (n == 0) return(c(NA_real_, NA_real_))
  z <- qnorm(1 - (1 - conf) / 2); p <- x / n; d <- 1 + z^2 / n
  c((p + z^2 / (2 * n) - z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / d,
    (p + z^2 / (2 * n) + z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2))) / d)
}

## Cohen's kappa on a 2 by 2, with the large sample normal approximation for
## the interval. Returns NA where one rater shows no variation at all.
kappa2 <- function(h, a) {
  n <- length(h)
  n11 <- sum(h == 1 & a == 1); n10 <- sum(h == 1 & a == 0)
  n01 <- sum(h == 0 & a == 1); n00 <- sum(h == 0 & a == 0)
  po  <- (n11 + n00) / n
  pe  <- ((n11 + n10) * (n11 + n01) + (n01 + n00) * (n10 + n00)) / n^2
  k   <- if (abs(1 - pe) < 1e-12) NA_real_ else (po - pe) / (1 - pe)
  se  <- if (is.na(k)) NA_real_ else sqrt(po * (1 - po) / (n * (1 - pe)^2))
  list(n = n, TP = n11, FN = n10, FP = n01, TN = n00,
       po = po, pe = pe, kappa = k, se = se,
       lo = if (is.na(k)) NA_real_ else k - 1.96 * se,
       hi = if (is.na(k)) NA_real_ else k + 1.96 * se)
}

## Exact McNemar on the discordant pairs. Direction is from the model's side.
mcnemar_exact <- function(fp, fn) {
  d <- fp + fn
  if (d == 0) return(list(p = NA_real_, dir = "no disagreement"))
  bt <- binom.test(fp, d, 0.5)
  list(p = bt$p.value,
       dir = if (fp > fn) "model over reports" else
             if (fn > fp) "model under reports" else "symmetric")
}

## Youden operating point off a pROC roc object.
youden <- function(r) {
  cc <- pROC::coords(r, "best", ret = c("threshold", "specificity", "sensitivity"),
                     best.method = "youden", transpose = FALSE)
  cc <- cc[1, , drop = FALSE]
  list(thr = cc$threshold, spec = cc$specificity, sens = cc$sensitivity)
}

brier <- function(p, y) mean((p - y)^2)

## The 20 indicators, in the group order used by 05_FINAL_MODEL.R.
DEFENCE <- c("boarded_up", "infilled_apertures")
STRUCT  <- c("smashed_glass", "roof_damage", "internal_debris",
             "structural_vegetation", "exposed_wiring")
NEGLECT <- c("facade_decay", "overgrown_grounds", "threshold_debris",
             "graffiti", "obsolete_signage")
INTERV  <- c("fixed_grilles", "external_padlocks", "notices", "for_sale_sign")
OCCUP   <- c("active_vehicles", "domestic_items", "waste_management",
             "maintained_curtilage")
IND20   <- c(DEFENCE, STRUCT, NEGLECT, INTERV, OCCUP)
GROUP_OF <- setNames(
  c(rep("Defence", length(DEFENCE)), rep("Structural", length(STRUCT)),
    rep("Neglect", length(NEGLECT)), rep("Interventions", length(INTERV)),
    rep("Occupancy", length(OCCUP))), IND20)


## ===================== F0.1  INPUT INVENTORY ============================ ##

blk("F0.1", "Input inventory",
    "Appendix D. Tells you which blocks below can run at all.")

inv <- data.frame(
  handle = c("F_GT","F_HUMAN","F_CITY","F_META","F_RANK3","F_BLDG","F_JOIN",
             "F_BIG","F_TRACK","F_REVIEW","F_FIELDS","F_FIELDR","F_DSR","F_RESOL"),
  path   = c(F_GT,F_HUMAN,F_CITY,F_META,F_RANK3,F_BLDG,F_JOIN,
             F_BIG,F_TRACK,F_REVIEW,F_FIELDS,F_FIELDR,F_DSR,F_RESOL),
  stringsAsFactors = FALSE)
inv$set     <- nzchar(inv$path)
inv$present <- inv$set & file.exists(inv$path)
inv$blocks  <- c("F1.x, F2.x", "F1.1", "F3.x", "F3.4", "F3.2", "F4.1",
                 "F3.1 cross check", "F3.9", "F4.2, F4.3", "F4.2, F4.3",
                 "F5.1", "F5.2", "F5.3", "F5.4")
emit("F0.1", inv)


## ####################################################################### ##
##  F1.  GROUND TRUTH, HUMAN AGREEMENT AND SEPARATION
## ####################################################################### ##

GT_OK <- exists_file(F_GT)
if (GT_OK) {
  gt_all <- rd(F_GT)
  for (v in IND20) {
    if (paste0("AI_", v) %in% names(gt_all)) gt_all[[paste0("AI_", v)]] <- num0(gt_all[[paste0("AI_", v)]])
    if (paste0("H_",  v) %in% names(gt_all)) gt_all[[paste0("H_",  v)]] <- num0(gt_all[[paste0("H_",  v)]])
  }
  gt_all$GROUND_TRUTH <- suppressWarnings(as.numeric(gt_all$GROUND_TRUTH))

  ## the modelling set, exactly as 05_FINAL_MODEL.R builds it
  gt <- gt_all[!is.na(gt_all$GROUND_TRUTH), ]
  gt$is_from_handpicked <- as.integer(gt$sample_source == "handpicked_v1")
  gt$is_from_foconnor   <- as.integer(gt$sample_source == "FOConnor_vacant")
  gt$Idx_Defence5 <- rs(gt, paste0("AI_", DEFENCE))
  gt$Idx_Struct5  <- rs(gt, paste0("AI_", STRUCT))
  gt$Idx_Neglect  <- rs(gt, paste0("AI_", NEGLECT))
  gt$Idx_Interv   <- rs(gt, paste0("AI_", INTERV))
  gt$Idx_Occ      <- rs(gt, paste0("AI_", OCCUP))
  gt$Idx_Struct4  <- gt$Idx_Defence5 + gt$Idx_Struct5   # four group specification
  gt$AI_vacancy_score <- num0(gt$AI_vacancy_score)

  ## the human rated set, which is a different and overlapping population
  hum <- gt_all[!is.na(gt_all$human_rated) & gt_all$human_rated == 1, ]
}

## ---- F1.1 -------------------------------------------------------------- ##
if (!GT_OK) {
  skipblk("F1.1", "Human rated sample size and provenance",
          "Results 4.4.2 NUMBER NEEDED, Methods 3.11.2",
          paste("needs", F_GT))
} else {
  blk("F1.1", "Human rated sample size and provenance",
      "Results 4.4.2 [NUMBER NEEDED: final human-rated sample size, and whether the images were drawn randomly or stratified], Methods 3.11.2")

  a <- data.frame(
    quantity = c("rows in GROUND_TRUTH_MASTER",
                 "buildings with human_rated = 1",
                 "of those, usable binary verdict",
                 "of those, cannot tell",
                 "human rated and vacant",
                 "human rated and occupied",
                 "modelling set, non missing GROUND_TRUTH",
                 "modelling set vacant",
                 "modelling set occupied"),
    n = c(nrow(gt_all),
          nrow(hum),
          sum(!is.na(hum$GROUND_TRUTH)),
          sum(is.na(hum$GROUND_TRUTH)),
          sum(hum$GROUND_TRUTH == 1, na.rm = TRUE),
          sum(hum$GROUND_TRUTH == 0, na.rm = TRUE),
          nrow(gt), sum(gt$GROUND_TRUTH == 1), sum(gt$GROUND_TRUTH == 0)))
  emit("F1.1a", a,
       "The human rated sample size to quote in 3.11.2 is the second row. The document currently says 'Total of 216 (I think)'. The per indicator agreement in F1.2 uses every human rated building including the cannot tell cases, because a cannot tell verdict on vacancy does not invalidate the indicator coding. The binary agreement in F1.3 uses only the usable verdicts.")

  ## provenance, which answers the random versus stratified half of the question
  b <- as.data.frame(table(sample_source = hum$sample_source), responseName = "n")
  b$pct <- pct(b$n, nrow(hum))
  tb <- table(hum$sample_source, ifelse(is.na(hum$GROUND_TRUTH), "cannot_tell",
                                 ifelse(hum$GROUND_TRUTH == 1, "vacant", "occupied")))
  emit("F1.1b", b,
       "sample_source is the answer to 'randomly or stratified'. GD_Q3_2022_human is the GeoDirectory drawn frame, handpicked_v1 is the enriched positive set and FOConnor_vacant is the external vacant set. Any source other than a single random frame means the sample is stratified by construction and must be described that way.")
  cat("\nHuman rated verdict by source:\n"); print(tb)

  ## external cross check against the rating application export
  if (exists_file(F_HUMAN)) {
    hf <- rd(F_HUMAN)
    key <- if ("Bldg_GUID" %in% names(hf)) "Bldg_GUID" else names(hf)[1]
    d <- data.frame(
      quantity = c("rows in the rating app export",
                   "unique buildings in the export",
                   "rated, verdict_label not NOT RATED",
                   "carried into GROUND_TRUTH_MASTER as human_rated",
                   "in the export but not carried into the master"),
      n = c(nrow(hf),
            length(unique(hf[[key]])),
            sum(!grepl("NOT RATED", hf$verdict_label, ignore.case = TRUE)),
            nrow(hum),
            length(setdiff(unique(hf[[key]][!grepl("NOT RATED", hf$verdict_label,
                                                   ignore.case = TRUE)]),
                           hum$Bldg_GUID))))
    emit("F1.1c", d,
         "If the last row is not zero, say so in 3.11.2 rather than quoting only the master count. A non zero value means the export holds ratings that never reached the analysis file.")
  }
}

## ---- F1.2  TABLE 4.4 ---------------------------------------------------- ##
if (!GT_OK) {
  skipblk("F1.2", "TABLE 4.4 human versus model agreement per indicator",
          "Results 4.4.2 TABLE 4.4", paste("needs", F_GT))
} else {
  blk("F1.2", "TABLE 4.4 human versus model agreement per indicator",
      "Results 4.4.2 [TABLE 4.4: per indicator, human positive count, model positive count, agreement percentage, Cohen's kappa, and prevalence in the sample. Order by kappa.]")

  rows <- lapply(IND20, function(v) {
    h <- as.integer(hum[[paste0("H_",  v)]] > 0)
    a <- as.integer(hum[[paste0("AI_", v)]] > 0)
    k <- kappa2(h, a)
    m <- mcnemar_exact(k$FP, k$FN)
    data.frame(
      indicator      = v,
      group          = unname(GROUP_OF[v]),
      n              = k$n,
      human_n        = k$TP + k$FN,
      model_n        = k$TP + k$FP,
      human_prev_pct = pct(k$TP + k$FN, k$n),
      model_prev_pct = pct(k$TP + k$FP, k$n),
      agreement_pct  = 100 * k$po,
      kappa          = k$kappa,
      kappa_lo       = k$lo,
      kappa_hi       = k$hi,
      TP = k$TP, FP = k$FP, FN = k$FN, TN = k$TN,
      model_recall    = if ((k$TP + k$FN) > 0) k$TP / (k$TP + k$FN) else NA_real_,
      model_precision = if ((k$TP + k$FP) > 0) k$TP / (k$TP + k$FP) else NA_real_,
      mcnemar_p       = m$p,
      direction       = m$dir,
      stability       = if (min(k$TP + k$FN, k$TP + k$FP) < RARE_MIN)
                          "kappa unstable, rare" else "ok")
  })
  t44 <- do.call(rbind, rows)
  t44 <- t44[order(-replace(t44$kappa, is.na(t44$kappa), -Inf)), ]
  emit("F1.2", t44,
       paste0("Human is the reference and the model is the test, so recall and precision read from the model's side. ",
              "kappa intervals are the large sample normal approximation and are wide at these counts, quote them. ",
              "Rows flagged 'kappa unstable, rare' have a marginal below ", RARE_MIN,
              "; report the raw counts for those rather than the kappa. ",
              "mcnemar_p is the exact binomial on the discordant pairs and is what answers 'report the direction of disagreement, not just its magnitude'."))
}

## ---- F1.3 --------------------------------------------------------------- ##
if (!GT_OK) {
  skipblk("F1.3", "Overall agreement and agreement on the binary classification",
          "Results 4.4.2", paste("needs", F_GT))
} else {
  blk("F1.3", "Overall agreement and agreement on the binary classification",
      "Results 4.4.2, 'Report overall agreement, and separately the agreement on the derived binary vacancy classification'")

  H <- as.matrix(hum[, paste0("H_",  IND20)]) > 0
  A <- as.matrix(hum[, paste0("AI_", IND20)]) > 0
  cells <- length(H)
  ov <- data.frame(
    quantity = c("image by indicator cells", "cells agreeing",
                 "overall cell agreement pct",
                 "cells the model calls positive and the human does not",
                 "cells the human calls positive and the model does not",
                 "pooled kappa across all cells"),
    value = c(cells, sum(H == A), 100 * mean(H == A),
              sum(!H & A), sum(H & !A),
              kappa2(as.integer(H), as.integer(A))$kappa))
  emit("F1.3a", ov,
       "Pooled kappa across all cells is dominated by the rare indicators and is reported as context only. The per indicator values in F1.2 are the ones to discuss.")

  ## binary vacancy agreement, on the human rated buildings with a usable verdict
  hb <- hum[!is.na(hum$GROUND_TRUTH), ]
  defs <- list(
    "AI_status == VACANT"      = as.integer(hb$AI_status == "VACANT"),
    "AI vacancy_score >= 3"    = as.integer(num0(hb$AI_vacancy_score) >= 3),
    "AI vacancy_score >= 2"    = as.integer(num0(hb$AI_vacancy_score) >= 2))
  bin <- do.call(rbind, lapply(names(defs), function(nm) {
    a <- defs[[nm]]; h <- as.integer(hb$GROUND_TRUTH == 1)
    k <- kappa2(h, a); m <- mcnemar_exact(k$FP, k$FN)
    data.frame(definition = nm, n = k$n,
               human_vacant = k$TP + k$FN, model_vacant = k$TP + k$FP,
               agreement_pct = 100 * k$po, kappa = k$kappa,
               kappa_lo = k$lo, kappa_hi = k$hi,
               TP = k$TP, FP = k$FP, FN = k$FN, TN = k$TN,
               sensitivity = if ((k$TP + k$FN) > 0) k$TP / (k$TP + k$FN) else NA_real_,
               specificity = if ((k$TN + k$FP) > 0) k$TN / (k$TN + k$FP) else NA_real_,
               mcnemar_p = m$p, direction = m$dir)
  }))
  emit("F1.3b", bin,
       "This is the agreement that matters more than any single indicator, because it is the judgement the classification actually makes. Three definitions of the model's binary call are given because the document does not fix one; quote whichever you describe in the text and say which.")

  ## severity score agreement
  cat("\nHuman vacancy_score against model vacancy_score, human rated set:\n")
  hs <- suppressWarnings(as.numeric(hum$human_vacancy_score))
  as_ <- suppressWarnings(as.numeric(hum$AI_vacancy_score))
  keep <- !is.na(hs) & !is.na(as_)
  print(table(human = hs[keep], model = as_[keep]))
  cat(sprintf("\nexact agreement %.2f%% | within one point %.2f%% | Spearman rho %.4f | n %d\n",
              100 * mean(hs[keep] == as_[keep]),
              100 * mean(abs(hs[keep] - as_[keep]) <= 1),
              suppressWarnings(cor(hs[keep], as_[keep], method = "spearman")),
              sum(keep)))
}

## ---- F1.4 --------------------------------------------------------------- ##
if (!GT_OK) {
  skipblk("F1.4", "Which indicators drive the disagreement", "Results 4.4.2",
          paste("needs", F_GT))
} else {
  blk("F1.4", "Which indicators drive the disagreement",
      "Results 4.4.2, 'Identify which indicators drive the disagreements' and 'Report the direction of disagreement'")

  d <- do.call(rbind, lapply(IND20, function(v) {
    h <- as.integer(hum[[paste0("H_",  v)]] > 0)
    a <- as.integer(hum[[paste0("AI_", v)]] > 0)
    fp <- sum(!h & a); fn <- sum(h & !a)
    data.frame(indicator = v, group = unname(GROUP_OF[v]),
               disagreements = fp + fn, model_over = fp, model_under = fn,
               net = fp - fn)
  }))
  tot <- sum(d$disagreements)
  d$share_of_all_disagreement_pct <- pct(d$disagreements, tot)
  d <- d[order(-d$disagreements), ]
  d$cumulative_share_pct <- cumsum(d$share_of_all_disagreement_pct)
  emit("F1.4", d,
       paste0("Total disagreeing cells ", tot,
              ". The cumulative column tells you how few indicators carry the disagreement, which is the sentence the section asks for. ",
              "A positive 'net' is systematic over reporting by the model, which biases the city wide results; a net near zero with a high count is noise, which does not."))
}

## ---- F1.5 --------------------------------------------------------------- ##
if (!GT_OK) {
  skipblk("F1.5", "Indicators too rare to assess", "Results 4.4.2",
          paste("needs", F_GT))
} else {
  blk("F1.5", "Indicators too rare to assess",
      "Results 4.4.2, 'Discuss the indicators that could not be assessed because they are too rare in the sample'")
  r <- do.call(rbind, lapply(IND20, function(v) {
    h <- sum(hum[[paste0("H_", v)]] > 0); a <- sum(hum[[paste0("AI_", v)]] > 0)
    data.frame(indicator = v, human_n = h, model_n = a,
               min_marginal = min(h, a),
               verdict = if (h == 0 && a == 0) "never observed by either rater"
                         else if (min(h, a) < RARE_MIN) "too rare, report counts not kappa"
                         else "assessable")
  }))
  emit("F1.5", r[order(r$min_marginal), ],
       paste0("Threshold is RARE_MIN = ", RARE_MIN,
              ", which is a reporting choice set at the top of this script, not a derived value. State it in the text."))
}

## ---- F1.6  TABLE 4.5 ---------------------------------------------------- ##
if (!GT_OK) {
  skipblk("F1.6", "TABLE 4.5 separation contingencies", "Results 4.4.3 TABLE 4.5",
          paste("needs", F_GT))
} else {
  blk("F1.6", "TABLE 4.5 separation contingencies against ground truth",
      "Results 4.4.3 [TABLE 4.5: contingency counts per indicator against ground truth, showing the empty or near-empty cells that generate the separation]")

  y <- gt$GROUND_TRUTH
  rows <- lapply(IND20, function(v) {
    x <- as.integer(gt[[paste0("AI_", v)]] > 0)
    n11 <- sum(x == 1 & y == 1); n10 <- sum(x == 1 & y == 0)
    n01 <- sum(x == 0 & y == 1); n00 <- sum(x == 0 & y == 0)
    zero <- min(n11, n10, n01, n00) == 0
    quasi <- !zero && min(n11, n10, n01, n00) <= 2
    ## unpenalised fit, purely to demonstrate why Firth is needed
    gl <- suppressWarnings(try(glm(y ~ x, family = binomial), silent = TRUE))
    ml_b <- ml_se <- NA_real_
    if (!inherits(gl, "try-error")) {
      s <- suppressWarnings(summary(gl)$coefficients)
      if (nrow(s) > 1) { ml_b <- s[2, 1]; ml_se <- s[2, 2] }
    }
    fb <- fp <- NA_real_
    if (PKG_LOGISTF) {
      lf <- try(logistf(y ~ x, data = data.frame(y = y, x = x)), silent = TRUE)
      if (!inherits(lf, "try-error")) { fb <- coef(lf)[2]; fp <- lf$prob[2] }
    }
    data.frame(
      indicator = v, group = unname(GROUP_OF[v]),
      present_vacant = n11, present_occupied = n10,
      absent_vacant = n01, absent_occupied = n00,
      min_cell = min(n11, n10, n01, n00),
      pct_vacant_when_present = pct(n11, n11 + n10),
      separation = if (zero) "complete or quasi complete, a zero cell"
                   else if (quasi) "near separation, min cell 1 or 2" else "none",
      ml_coef = ml_b, ml_se = ml_se,
      firth_coef = unname(fb), firth_p = unname(fp))
  })
  t45 <- do.call(rbind, rows)
  t45 <- t45[order(t45$min_cell, -t45$present_vacant), ]
  emit("F1.6", t45,
       "The ml_coef and ml_se columns are the argument for Firth made visible: where a cell is empty the unpenalised estimate runs away and its standard error explodes, while the Firth estimate stays finite. Quote one such row in the text at the point where the reader meets the penalised likelihood. The document names internal_debris, infilled_apertures, roof_damage, obsolete_signage and smashed_glass as the separating indicators; the min_cell column confirms or corrects that list from the data.")
}

## ---- F1.7 --------------------------------------------------------------- ##
if (!GT_OK) {
  skipblk("F1.7", "Precision of the priority indicator", "Methods 3.10.8",
          paste("needs", F_GT))
} else {
  blk("F1.7", "Precision of the priority indicator against ground truth",
      "Methods 3.10.8, the 98.5 per cent boarded_up precision claim and the untested occupancy gate")
  b  <- gt[[paste0("AI_", PRIORITY_IND)]] > 0
  occ <- gt$Idx_Occ
  ci <- wilson(sum(gt$GROUND_TRUTH[b]), sum(b))
  s <- data.frame(
    quantity = c("buildings with the priority indicator",
                 "of those, truly vacant", "precision pct",
                 "precision 95 pct lower", "precision 95 pct upper",
                 paste0("of those, occupancy index >= ", MAX_OCC, " (the withdrawn band)")),
    value = c(sum(b), sum(gt$GROUND_TRUTH[b]),
              100 * mean(gt$GROUND_TRUTH[b]), 100 * ci[1], 100 * ci[2],
              sum(b & occ >= MAX_OCC)))
  emit("F1.7a", s,
       "If the last row is zero the occupancy gate is untested against ground truth and must be described as a precautionary extrapolation, which is what the document already says. This block is here so the claim carries a number rather than an assertion.")
  cat("\nBoarded buildings split by occupancy load:\n")
  print(table(occ_band = ifelse(occ[b] < MAX_OCC, "below gate", "at or above gate"),
              truth = gt$GROUND_TRUTH[b]))
}


## ####################################################################### ##
##  F2.  MODEL COMPARISON, SELECTION AND VALIDATION
## ####################################################################### ##

MODEL_OK <- GT_OK && PKG_LOGISTF && PKG_PROC
CTRL <- if (DROP_FOCONNOR) "is_from_handpicked" else "is_from_handpicked + is_from_foconnor"

SPECS <- list(
  "Holistic score only"          = "AI_vacancy_score",
  "Four-group indices"           = "Idx_Struct4 + Idx_Neglect + Idx_Interv + Idx_Occ",
  "Five-group indices"           = "Idx_Defence5 + Idx_Struct5 + Idx_Neglect + Idx_Interv + Idx_Occ",
  "All 20 indicators"            = paste(paste0("AI_", IND20), collapse = " + "),
  "Five-group plus holistic"     = "Idx_Defence5 + Idx_Struct5 + Idx_Neglect + Idx_Interv + Idx_Occ + AI_vacancy_score",
  "All 20 plus holistic"         = paste(c(paste0("AI_", IND20), "AI_vacancy_score"), collapse = " + ")
)

if (MODEL_OK) {
  fit_spec <- function(rhs, with_ctrl = TRUE) {
    f <- as.formula(paste("GROUND_TRUTH ~", rhs, if (with_ctrl) paste("+", CTRL) else ""))
    m <- logistf(f, data = gt)
    r <- pROC::roc(gt$GROUND_TRUTH, m$predict, quiet = TRUE, direction = "<")
    list(formula = f, model = m, roc = r, p = m$predict)
  }
  FITS <- lapply(SPECS, function(rhs) fit_spec(rhs, TRUE))
}

## ---- F2.1 --------------------------------------------------------------- ##
if (!MODEL_OK) {
  skipblk("F2.1", "TABLE 4.5.1 model comparison", "Results 4.5.1, the three [confirm] cells",
          "needs GROUND_TRUTH_MASTER.csv plus the logistf and pROC packages")
} else {
  blk("F2.1", "TABLE 4.5.1 model comparison",
      "Results 4.5.1, the three cells currently marked [confirm] and the empty threshold, sensitivity and specificity columns")

  cmp <- do.call(rbind, lapply(names(FITS), function(nm) {
    f <- FITS[[nm]]; yv <- youden(f$roc)
    data.frame(model = nm,
               specification = SPECS[[nm]],
               n_params = length(coef(f$model)),
               AUC = as.numeric(pROC::auc(f$roc)),
               threshold_pct = 100 * yv$thr,
               sensitivity_pct = 100 * yv$sens,
               specificity_pct = 100 * yv$spec,
               brier = brier(f$p, gt$GROUND_TRUTH),
               events_per_variable = sum(gt$GROUND_TRUTH) / (length(coef(f$model)) - 1))
  }))
  emit("F2.1", cmp,
       paste0("All six rows are fitted on the same ", nrow(gt),
              " buildings with the same control term (", CTRL,
              "), so the column is internally comparable. The 0.9771 and 0.9779 pair currently in the document came from an earlier run; replace the whole table with this one rather than filling only the three [confirm] cells, otherwise the rows are not comparable with each other. AUC here is apparent, not cross validated: see F2.6."))
  cat(sprintf("\nLowest AUC across all specifications: %.4f (%s)\n",
              min(cmp$AUC), cmp$model[which.min(cmp$AUC)]))
  cat(sprintf("Highest AUC: %.4f (%s)\n", max(cmp$AUC), cmp$model[which.max(cmp$AUC)]))
  cat("The document asserts 'no model returned an AUC below 0.9289' and 'the strongest was the combined indicators plus holistic score'. Check both sentences against the two lines above.\n")
  cat(sprintf("Four-group versus five-group AUC difference: %.4f\n",
              as.numeric(pROC::auc(FITS[["Five-group indices"]]$roc)) -
              as.numeric(pROC::auc(FITS[["Four-group indices"]]$roc))))
}

## ---- F2.2 --------------------------------------------------------------- ##
if (!MODEL_OK) {
  skipblk("F2.2", "DeLong tests", "Results 4.5.1 [NUMBER NEEDED: the DeLong p value and confidence interval]",
          "needs GROUND_TRUTH_MASTER.csv plus logistf and pROC")
} else {
  blk("F2.2", "DeLong tests between nested specifications",
      "Results 4.5.1 [NUMBER NEEDED: the DeLong p value and confidence interval]")

  pairs <- list(
    c("All 20 indicators", "All 20 plus holistic"),
    c("Five-group indices", "Five-group plus holistic"),
    c("Four-group indices", "Five-group indices"),
    c("Holistic score only", "Five-group indices"))

  dl <- do.call(rbind, lapply(pairs, function(pr) {
    r1 <- FITS[[pr[1]]]$roc; r2 <- FITS[[pr[2]]]$roc
    tt <- pROC::roc.test(r1, r2, method = "delong")
    a1 <- as.numeric(pROC::auc(r1)); a2 <- as.numeric(pROC::auc(r2))
    diff <- a2 - a1
    z <- unname(tt$statistic)
    se <- if (is.finite(z) && abs(z) > 1e-9) abs(diff / z) else NA_real_
    data.frame(model_A = pr[1], model_B = pr[2],
               AUC_A = a1, AUC_B = a2, difference = diff,
               delong_Z = z, p_value = tt$p.value,
               diff_lo95 = diff - 1.96 * se, diff_hi95 = diff + 1.96 * se)
  }))
  emit("F2.2", dl,
       "The interval is the DeLong difference plus or minus 1.96 standard errors, with the standard error recovered from the test statistic; report it as the interval on the AUC difference, not on either AUC. Flag honestly in the text that DeLong on apparent, in sample predictions from nested models is anticonservative, so a non significant result here is a conservative basis for choosing the simpler model, which is the direction your argument runs anyway.")
}

## ---- F2.3 --------------------------------------------------------------- ##
if (!MODEL_OK) {
  skipblk("F2.3", "Deployed model coefficients", "Results 4.5.2", "needs logistf and pROC")
} else {
  blk("F2.3", "Deployed model coefficients, intervals and odds ratios",
      "Results 4.5.2 coefficient table, and 4.5.3 'Convert the key coefficients to odds ratios'")
  m5 <- FITS[["Five-group indices"]]$model
  co <- data.frame(term = names(coef(m5)),
                   coef = unname(coef(m5)),
                   lower = unname(m5$ci.lower),
                   upper = unname(m5$ci.upper),
                   p = unname(m5$prob))
  co$odds_ratio <- exp(co$coef)
  co$OR_lower <- exp(co$lower); co$OR_upper <- exp(co$upper)
  emit("F2.3", co,
       "Compare every value against the table currently in 4.5.2. If any coefficient differs, the table in the document came from a different run and the whole table must be replaced, not patched.")
  yv <- youden(FITS[["Five-group indices"]]$roc)
  cat(sprintf("\nAUC %.4f | Youden threshold %.1f%% | sensitivity %.1f%% | specificity %.1f%% | n %d (vacant %d)\n",
              as.numeric(pROC::auc(FITS[["Five-group indices"]]$roc)),
              100 * yv$thr, 100 * yv$sens, 100 * yv$spec,
              nrow(gt), sum(gt$GROUND_TRUTH)))
}

## ---- F2.4 --------------------------------------------------------------- ##
if (!MODEL_OK) {
  skipblk("F2.4", "Worked example", "Results 4.5.3", "needs logistf and pROC")
} else {
  blk("F2.4", "Worked example, the X and Y in the text",
      "Results 4.5.3, 'a building with two defence indicators and no signs of occupancy scores X, while the same building with a car in the drive and a maintained garden scores Y'")
  W <- coef(FITS[["Five-group indices"]]$model)
  mk <- function(d, s, ng, i, o) unname(
    d * W["Idx_Defence5"] + s * W["Idx_Struct5"] + ng * W["Idx_Neglect"] +
    i * W["Idx_Interv"] + o * W["Idx_Occ"])
  ex <- data.frame(
    case = c("two defence, nothing else",
             "two defence plus car and maintained garden",
             "one defence only",
             "no dereliction, car and maintained garden only",
             "all five defence and structural, no occupancy"),
    defence = c(2, 2, 1, 0, 2), structural = c(0, 0, 0, 0, 5),
    neglect = c(0, 0, 0, 0, 0), interventions = c(0, 0, 0, 0, 0),
    occupancy = c(0, 2, 0, 2, 0))
  ex$linear_score <- mapply(mk, ex$defence, ex$structural, ex$neglect,
                            ex$interventions, ex$occupancy)
  ex$difference_from_first <- ex$linear_score - ex$linear_score[1]
  ex$odds_ratio_vs_first <- exp(ex$difference_from_first)
  emit("F2.4", ex,
       "linear_score is on the same scale as the city wide Linear_Score, so these rows can be placed directly against the tier boundaries reported in F3.7. The probability depends on which anchor intercept is added, so quote the score and the odds ratio here rather than a probability.")
}

## ---- F2.5 --------------------------------------------------------------- ##
if (!MODEL_OK) {
  skipblk("F2.5", "The overgrown_grounds anomaly", "Results 4.5.4 [CHECK]", "needs logistf")
} else {
  blk("F2.5", "The overgrown_grounds anomaly, checked in the final ungrouped model",
      "Results 4.5.4 [CHECK: confirm whether this holds in the final Firth ungrouped model, or only in the earlier bootstrapped one]")
  m20 <- FITS[["All 20 indicators"]]$model
  u <- data.frame(term = names(coef(m20)), coef = unname(coef(m20)),
                  lower = unname(m20$ci.lower), upper = unname(m20$ci.upper),
                  p = unname(m20$prob))
  u$odds_ratio <- exp(u$coef)
  u <- u[order(u$coef), ]
  emit("F2.5", u,
       "This is also the secondary ungrouped table promised at 4.5.2 as [TABLE 4.7]. Read the AI_overgrown_grounds row: if the coefficient is negative the 4.5.4 finding survives into the deployed family of models and can be reported as a finding; if it is positive or the interval spans zero widely, report it as an observation from model development on the earlier bootstrapped model, which is exactly the fallback the document already specifies.")
  r <- u[u$term == "AI_overgrown_grounds", ]
  if (nrow(r) == 1)
    cat(sprintf("\nVERDICT: AI_overgrown_grounds coefficient %.4f (%.4f to %.4f), p = %.4f. Sign is %s. The document's -0.682 came from the earlier ungrouped bootstrapped model.\n",
                r$coef, r$lower, r$upper, r$p,
                ifelse(r$coef < 0, "NEGATIVE, the finding survives",
                       "POSITIVE, the finding does not survive")))
}

## ---- F2.6 --------------------------------------------------------------- ##
if (!MODEL_OK) {
  skipblk("F2.6", "Cross validation reconciliation", "Results 4.5.5 [RECONCILE], Appendix D.2",
          "needs logistf and pROC")
} else {
  blk("F2.6", "Cross validation reconciliation, the 0.9730 against the 0.9411",
      "Results 4.5.5 [RECONCILE BEFORE SUBMISSION], Methods 3.10.6, Appendix D.2")

  set.seed(CV_SEED)
  i0 <- sample(which(gt$GROUND_TRUTH == 0)); i1 <- sample(which(gt$GROUND_TRUTH == 1))
  folds <- lapply(1:5, function(k) c(i0[seq(k, length(i0), 5)], i1[seq(k, length(i1), 5)]))

  cv_one <- function(rhs, with_ctrl) {
    f <- as.formula(paste("GROUND_TRUTH ~", rhs, if (with_ctrl) paste("+", CTRL) else ""))
    m_app <- logistf(f, data = gt)
    r_app <- pROC::roc(gt$GROUND_TRUTH, m_app$predict, quiet = TRUE, direction = "<")
    oof <- rep(NA_real_, nrow(gt))
    for (fo in folds) {
      mt <- logistf(f, data = gt[-fo, ])
      X <- model.matrix(f, gt[fo, ])
      oof[fo] <- as.vector(1 / (1 + exp(-(X %*% coef(mt)))))
    }
    r_cv <- pROC::roc(gt$GROUND_TRUTH, oof, quiet = TRUE, direction = "<")
    ci <- as.numeric(pROC::ci.auc(r_app, method = "delong"))
    list(apparent = as.numeric(pROC::auc(r_app)),
         cv = as.numeric(pROC::auc(r_cv)),
         lo = ci[1], hi = ci[3])
  }

  variants <- list(
    "Five indices plus is_from_handpicked (as 05_FINAL_MODEL.R)" =
      list("Idx_Defence5 + Idx_Struct5 + Idx_Neglect + Idx_Interv + Idx_Occ", TRUE),
    "Five indices only, no control (as scored at city scale)" =
      list("Idx_Defence5 + Idx_Struct5 + Idx_Neglect + Idx_Interv + Idx_Occ", FALSE))

  rec <- do.call(rbind, lapply(names(variants), function(nm) {
    v <- variants[[nm]]; o <- cv_one(v[[1]], v[[2]])
    data.frame(variant = nm, control_in_folds = v[[2]],
               apparent_AUC = o$apparent, cv_AUC = o$cv,
               optimism = o$apparent - o$cv,
               apparent_lo95 = o$lo, apparent_hi95 = o$hi)
  }))
  emit("F2.6", rec,
       paste0("Both rows use the identical folds from set.seed(", CV_SEED,
              "), so the only difference between them is the control term. That is the one sentence 4.5.5 asks for. The second row is the more conservative and the more honest headline, because the city wide scoring carries no is_from_handpicked term; the first row is what Methods 3.10.6 currently reports. Pick one, put the other in a footnote, and make 3.10.6, 4.5.5 and Appendix D.2 agree."))
}

## ---- F2.7 --------------------------------------------------------------- ##
if (!MODEL_OK) {
  skipblk("F2.7", "Threshold selection bootstrap", "Results 4.5.5", "needs pROC")
} else {
  blk("F2.7", "Threshold selection optimism, separate from model optimism",
      "Results 4.5.5, the 2,000 replicate cutoff bootstrap")
  p <- FITS[["Five-group indices"]]$p
  y <- gt$GROUND_TRUTH
  J_at <- function(thr, pp, yy) {
    pred <- as.integer(pp >= thr)
    sens <- if (sum(yy == 1) > 0) sum(pred == 1 & yy == 1) / sum(yy == 1) else NA
    spec <- if (sum(yy == 0) > 0) sum(pred == 0 & yy == 0) / sum(yy == 0) else NA
    sens + spec - 1
  }
  yv <- youden(FITS[["Five-group indices"]]$roc)
  J_app <- J_at(yv$thr, p, y)
  set.seed(BOOT_SEED)
  opt <- numeric(0)
  for (b in seq_len(N_BOOT)) {
    ix <- sample(seq_along(y), replace = TRUE)
    if (length(unique(y[ix])) < 2) next
    rb <- suppressMessages(pROC::roc(y[ix], p[ix], quiet = TRUE, direction = "<"))
    tb <- youden(rb)$thr
    opt <- c(opt, J_at(tb, p[ix], y[ix]) - J_at(tb, p, y))
  }
  s <- data.frame(
    quantity = c("apparent Youden J at the selected cutoff", "selected cutoff",
                 "replicates used", "mean optimism",
                 "optimism corrected J", "optimism 2.5 pct", "optimism 97.5 pct"),
    value = c(J_app, yv$thr, length(opt), mean(opt), J_app - mean(opt),
              quantile(opt, 0.025), quantile(opt, 0.975)))
  emit("F2.7", s,
       "This is threshold selection optimism only. It is a different quantity from the model optimism in F2.6 and the document is right to insist they are reported separately.")
}

## ---- F2.8 --------------------------------------------------------------- ##
if (!MODEL_OK) {
  skipblk("F2.8", "Events per variable", "Appendix D.1 item 15", "needs logistf")
} else {
  blk("F2.8", "Events per variable for every specification",
      "Appendix D.1, the EPV arithmetic that does not reconcile (6.7 quoted, 149 over 37 gives 4.0)")
  epv <- do.call(rbind, lapply(names(FITS), function(nm) {
    k <- length(coef(FITS[[nm]]$model)) - 1
    data.frame(model = nm, events = sum(gt$GROUND_TRUTH),
               non_events = sum(gt$GROUND_TRUTH == 0),
               predictors_excluding_intercept = k,
               EPV = sum(gt$GROUND_TRUTH) / k,
               EPV_on_minority = min(sum(gt$GROUND_TRUTH),
                                     sum(gt$GROUND_TRUTH == 0)) / k)
  }))
  emit("F2.8", epv,
       "Quote the row for the deployed specification, not for the 20 indicator one, and say which convention you are using. The 6.7 in the notes matches neither convention on 37 parameters, so it should be dropped rather than reconciled.")
}

## ---- F2.9 --------------------------------------------------------------- ##
blk("F2.9", "Youden deployment arithmetic",
    "Methods 3.10.7 [CHECK: recomputed from sensitivity 96.0 and specificity 89.5 applied to 53,436 evaluable buildings. Decide which prevalence assumption to lead with.]")
youden_cost <- function(N, prev, sens, spec) {
  vac <- N * prev; occ <- N * (1 - prev)
  data.frame(N = N, prevalence_pct = 100 * prev,
             sensitivity_pct = 100 * sens, specificity_pct = 100 * spec,
             true_vacant = vac, true_positives = vac * sens,
             false_negatives = vac * (1 - sens),
             false_positives = occ * (1 - spec),
             true_negatives = occ * spec,
             flagged_total = vac * sens + occ * (1 - spec),
             precision_pct = 100 * (vac * sens) / (vac * sens + occ * (1 - spec)),
             false_alarms_per_true_detection = (occ * (1 - spec)) / (vac * sens))
}
yc <- rbind(
  cbind(assumption = "GeoDirectory anchored 1.705 pct", youden_cost(N_EVAL, 0.01705, 0.960, 0.895)),
  cbind(assumption = "Census anchored 5.555 pct",       youden_cost(N_EVAL, 0.05555, 0.960, 0.895)),
  cbind(assumption = "Unanchored 6.308 pct",            youden_cost(N_EVAL, 0.06308, 0.960, 0.895)))
emit("F2.9", yc,
     "Sensitivity, specificity and N are inputs, not outputs of this block, so this is arithmetic rather than a result: say so in the text. The ratio column is the number the argument in 3.10.7 turns on. If F3.1 reconstructs a different evaluable count, change N_EVAL at the top of this block and rerun.")


## ####################################################################### ##
##  F3.  THE CITY WIDE DATASET
## ####################################################################### ##

CITY_OK <- exists_file(F_CITY)
if (CITY_OK) {
  city <- rd(F_CITY)
  for (v in IND20) if (v %in% names(city)) city[[v]] <- num0(city[[v]])
  city$has_target_building <- as.integer(
    toupper(as.character(city$has_target_building)) %in% c("TRUE", "T", "1", "YES", "Y"))
  city$Index_Defence       <- rs(city, DEFENCE)
  city$Index_Structural    <- rs(city, STRUCT)
  city$Index_Neglect       <- rs(city, NEGLECT)
  city$Index_Interventions <- rs(city, INTERV)
  city$Index_Occupancy     <- rs(city, OCCUP)

  ## FUNC_ID from the metadata sheet
  if (exists_file(F_META) && PKG_READXL) {
    meta <- as.data.frame(readxl::read_excel(F_META))
    idc <- intersect(c("Bldg_GUID", "GUID", "GEO_ID", "GeoID"), names(meta))[1]
    if (!is.na(idc)) {
      names(meta)[names(meta) == idc] <- "Bldg_GUID"
      if ("FUNC_ID" %in% names(city)) city$FUNC_ID <- NULL
      keep <- unique(c("Bldg_GUID", intersect(c("FUNC_ID"), names(meta))))
      md <- meta[!duplicated(meta$Bldg_GUID), keep, drop = FALSE]
      city <- merge(city, md, by = "Bldg_GUID", all.x = TRUE)
    }
  }
  city$is_outbuilding <- !is.na(city$FUNC_ID) &
    (tolower(trimws(as.character(city$FUNC_ID))) == "outbuilding" |
       trimws(as.character(city$FUNC_ID)) == "325")
}

## ---- F3.1 --------------------------------------------------------------- ##
if (!(CITY_OK && MODEL_OK)) {
  skipblk("F3.1", "Score and tier reconstruction", "Results 4.6.3",
          paste("needs", F_CITY, "plus the ground truth file, logistf and pROC"))
} else {
  blk("F3.1", "Score and tier reconstruction, with a cross check against the export",
      "Results 4.6.3, and the denominator every other block depends on")

  W <- coef(FITS[["Five-group indices"]]$model)
  city$Linear_Score <-
    city$Index_Defence * W["Idx_Defence5"] + city$Index_Structural * W["Idx_Struct5"] +
    city$Index_Neglect * W["Idx_Neglect"]  + city$Index_Interventions * W["Idx_Interv"] +
    city$Index_Occupancy * W["Idx_Occ"]
  city$Priority <- as.integer(city[[PRIORITY_IND]] > 0 & city$Index_Occupancy < MAX_OCC)

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

  run_set <- function(df, label) {
    e <- df[df$has_target_building == 1, ]
    n1 <- round(TARGET_GEO * nrow(e)); n2c <- round(TARGET_CEN * nrow(e))
    e$Tier <- assign_tiers(e, n1, n2c)
    list(label = label, all = df, eval = e, n1 = n1, n2c = n2c)
  }
  RUN_ALL   <- run_set(city, "all buildings")
  RUN_NOOUT <- run_set(city[!city$is_outbuilding, ], "excluding outbuildings")

  counts <- data.frame(
    run = c("all buildings", "excluding outbuildings"),
    rows_in = c(nrow(RUN_ALL$all), nrow(RUN_NOOUT$all)),
    evaluable = c(nrow(RUN_ALL$eval), nrow(RUN_NOOUT$eval)),
    tier4_unevaluable = c(nrow(RUN_ALL$all) - nrow(RUN_ALL$eval),
                          nrow(RUN_NOOUT$all) - nrow(RUN_NOOUT$eval)),
    tier1 = c(sum(RUN_ALL$eval$Tier == 1), sum(RUN_NOOUT$eval$Tier == 1)),
    tier2 = c(sum(RUN_ALL$eval$Tier == 2), sum(RUN_NOOUT$eval$Tier == 2)),
    tier3 = c(sum(RUN_ALL$eval$Tier == 3), sum(RUN_NOOUT$eval$Tier == 3)),
    quota_t1 = c(RUN_ALL$n1, RUN_NOOUT$n1),
    quota_t1_2 = c(RUN_ALL$n2c, RUN_NOOUT$n2c),
    tier1_pct_of_evaluable = c(pct(sum(RUN_ALL$eval$Tier == 1), nrow(RUN_ALL$eval)),
                               pct(sum(RUN_NOOUT$eval$Tier == 1), nrow(RUN_NOOUT$eval))))
  emit("F3.1a", counts,
       "The document's 4.6.3 table gives 936 / 2,112 / 51,821 / 5,221 for all buildings and 911 / 2,057 / 50,468 / 3,944 excluding outbuildings, on 2,710 outbuildings. If any cell here differs, this reconstruction is authoritative and the table must be replaced, because these numbers come from the model fitted in F2.3 rather than from a stored export.")
  cat(sprintf("\nOutbuildings flagged: %d\n", sum(city$is_outbuilding, na.rm = TRUE)))
  cat("Note the two Tier 4 counts differ between runs by exactly the number of occluded buildings that are also outbuildings. That is the source of the 5,221 against 3,944 confusion in Appendix D.1b, and it is not a dataset version difference.\n")
  cat(sprintf("Occluded buildings that are also outbuildings: %d\n",
              sum(city$has_target_building == 0 & city$is_outbuilding, na.rm = TRUE)))
  if (nrow(RUN_NOOUT$eval) != N_EVAL)
    cat(sprintf("WARNING: N_EVAL at the top of this script is %d but the reconstruction gives %d. Change N_EVAL and rerun, because block F2.9 used the old value.\n",
                N_EVAL, nrow(RUN_NOOUT$eval)))

  ## anchored intercepts, reported as a diagnostic
  anchor <- function(s, t) uniroot(function(c0) mean(1 / (1 + exp(-(c0 + s)))) - t,
                                   c(-40, 40), extendInt = "yes")$root
  ev <- RUN_NOOUT$eval
  cg <- anchor(ev$Linear_Score, TARGET_GEO); cc_ <- anchor(ev$Linear_Score, TARGET_CEN)
  ints <- data.frame(
    anchor = c("GeoDirectory", "Census"),
    target_prevalence = c(TARGET_GEO, TARGET_CEN),
    solved_intercept = c(cg, cc_),
    n_above_p_0.5 = c(sum(1 / (1 + exp(-(cg + ev$Linear_Score))) >= 0.5),
                      sum(1 / (1 + exp(-(cc_ + ev$Linear_Score))) >= 0.5)),
    quota_count = c(round(TARGET_GEO * nrow(ev)), round(TARGET_CEN * nrow(ev))))
  ints$shortfall_pct <- 100 * (1 - ints$n_above_p_0.5 / ints$quota_count)
  emit("F3.1b", ints,
       "Solved on the evaluable set of the run named in the label. The document quotes intercepts solved on 54,869 visible buildings including outbuildings, so if you want that variant change ev to RUN_ALL$eval. The shortfall column is the 'roughly 40 per cent' claim in 3.10.7 made explicit.")

  ## optional cross check against a stored export
  if (exists_file(F_JOIN)) {
    j <- rd(F_JOIN)
    tcol <- find_col(j, c("^Vacancy_Tier_NoOut$", "Vacancy_Tier_NoOut", "Vacancy_Tier"))
    if (!is.na(tcol)) {
      cat("\nCross check against ", basename(F_JOIN), ":\n", sep = "")
      print(table(substr(as.character(j[[tcol]]), 1, 1)))
      cat("If the tier 1 count here does not match F3.1a, the export on disk is from a different run and must not be used for any figure in the chapter. Reconstruct instead.\n")
    }
  }
}

## ---- F3.2 --------------------------------------------------------------- ##
blk("F3.2", "Field of view and camera to building distance",
    "Results 4.2.5, 'Report the distribution of applied field of view and camera-to-building distance' and 'the proportion of images at extreme field of view values'; Appendix D.2")
{
  src <- NULL
  for (cand in c(F_RANK3, F_CITY, F_META)) {
    if (!exists_file(cand)) next
    dd <- if (grepl("\\.xls[x]?$", cand, ignore.case = TRUE)) {
      if (PKG_READXL) as.data.frame(readxl::read_excel(cand)) else NULL
    } else rd(cand)
    if (is.null(dd)) next
    fv <- find_col(dd, c("^FOV$", "fov", "field_of_view", "Applied_FOV"))
    ds <- find_col(dd, c("^Distance", "dist.*m$", "cam.*dist", "NEAR_DIST", "MIN_NEAR_DIST", "dist"))
    if (!is.na(fv)) { src <- list(d = dd, f = cand, fov = fv, dist = ds); break }
  }
  if (is.null(src)) {
    cat("SKIPPED. No field of view column found in any of the configured files.\n")
    cat("Columns were searched for the patterns FOV, field_of_view, Applied_FOV.\n")
    cat("The applied field of view is set at the targeting stage, so the file to point F_RANK3 or F_META at is the Stage 3 output, Final_FOV.xls or its CSV equivalent.\n")
  } else {
    cat("source file: ", src$f, "\n", sep = "")
    cat("field of view column: ", src$fov, " | distance column: ",
        ifelse(is.na(src$dist), "NOT FOUND", src$dist), "\n", sep = "")
    fov <- suppressWarnings(as.numeric(src$d[[src$fov]])); fov <- fov[is.finite(fov)]
    st <- function(x, nm) data.frame(
      variable = nm, n = length(x), mean = mean(x), sd = sd(x),
      min = min(x), q05 = quantile(x, .05), q25 = quantile(x, .25),
      median = median(x), q75 = quantile(x, .75), q95 = quantile(x, .95), max = max(x))
    out <- st(fov, "applied field of view, degrees")
    if (!is.na(src$dist)) {
      dv <- suppressWarnings(as.numeric(src$d[[src$dist]])); dv <- dv[is.finite(dv)]
      out <- rbind(out, st(dv, paste0("camera to building distance (", src$dist, ")")))
    }
    emit("F3.2a", out,
         "The document currently states an average field of view of 35.69 degrees and an average median distance of 16.73 m. Check both against this table and note which file each came from.")
    ex <- data.frame(
      band = c(paste0("narrow, below ", FOV_NARROW, " degrees"),
               paste0("normal, ", FOV_NARROW, " to ", FOV_WIDE),
               paste0("wide, above ", FOV_WIDE, " degrees")),
      n = c(sum(fov < FOV_NARROW), sum(fov >= FOV_NARROW & fov <= FOV_WIDE),
            sum(fov > FOV_WIDE)))
    ex$pct <- pct(ex$n, length(fov))
    emit("F3.2b", ex,
         paste0("FOV_NARROW = ", FOV_NARROW, " and FOV_WIDE = ", FOV_WIDE,
                " are reporting choices set at the top of this script, not thresholds that exist in the targeting code. State the cut points in the text or take them from the narrow field of view penalty in GSVTargeting.py so they are defensible."))
  }
}

## ---- F3.3 --------------------------------------------------------------- ##
if (!CITY_OK) {
  skipblk("F3.3", "Occlusion causes", "Results 4.2.4", paste("needs", F_CITY))
} else {
  blk("F3.3", "Occlusion causes read from the rationale text",
      "Results 4.2.4, 'Report the causes qualitatively from the rationale field, which states the obstruction in every occluded case'")
  rc <- find_col(city, c("score_rationale", "rationale", "AI_rationale", "reason"))
  if (is.na(rc)) {
    cat("SKIPPED. No rationale column found in ", basename(F_CITY), ".\n", sep = "")
    cat("Searched for: score_rationale, rationale, AI_rationale, reason.\n")
  } else if (sum(city$has_target_building == 0) == 0) {
    cat("SKIPPED. No occluded buildings in the file.\n")
  } else {
    occl <- city[city$has_target_building == 0, ]
    txt <- tolower(as.character(occl[[rc]]))
    hits <- vapply(OCC_PATTERNS, function(p) grepl(p, txt, perl = TRUE),
                   logical(length(txt)))
    if (is.null(dim(hits))) hits <- matrix(hits, nrow = length(txt))
    colnames(hits) <- names(OCC_PATTERNS)
    tab <- data.frame(cause = colnames(hits),
                      n_any_mention = colSums(hits))
    tab$pct_of_occluded <- pct(tab$n_any_mention, nrow(occl))
    tab <- tab[order(-tab$n_any_mention), ]
    ## a single label per building, first match in the priority order given above
    first <- apply(hits, 1, function(r) if (any(r)) colnames(hits)[which(r)[1]] else "unclassified")
    ex <- as.data.frame(table(primary_cause = first), responseName = "n")
    ex$pct_of_occluded <- pct(ex$n, nrow(occl))
    ex <- ex[order(-ex$n), ]
    cat(sprintf("occluded buildings: %d of %d (%.2f%%)\n",
                nrow(occl), nrow(city), pct(nrow(occl), nrow(city))))
    emit("F3.3a", tab,
         "Counts overlap because one rationale can name several obstructions. Report these as 'mentioned in' percentages, not as a partition.")
    emit("F3.3b", ex,
         paste0("This one is a partition: the first matching cause in the order the OCC_PATTERNS list is written. The unclassified share is the honest measure of how well the keyword list covers the text; if it is large, read a sample of the unclassified rationales and extend OCC_PATTERNS. The document says 'vegetation dominated', so the vegetation row is the sentence to support."))
    cat("\nTen unclassified rationales, to check the keyword list:\n")
    un <- occl[[rc]][first == "unclassified"]
    if (length(un)) print(utils::head(unname(un), 10)) else cat("(none)\n")
  }
}

## ---- F3.4 --------------------------------------------------------------- ##
if (!(CITY_OK && MODEL_OK) || !exists("RUN_ALL")) {
  skipblk("F3.4", "FUNC_ID profile of flagged buildings", "Results 4.6.5 [NUMBER NEEDED]",
          "needs the city file, the metadata file for FUNC_ID, and F3.1 to have run")
} else if (!"FUNC_ID" %in% names(city)) {
  skipblk("F3.4", "FUNC_ID profile of flagged buildings", "Results 4.6.5 [NUMBER NEEDED]",
          paste("FUNC_ID not present. Set F_META to the Prime2 metadata sheet, currently", F_META))
} else {
  blk("F3.4", "FUNC_ID profile of flagged buildings",
      "Results 4.6.5 'Roughly 15 per cent of buildings flagged as vacant are coded as outbuildings in Prime2. Report the exact figure. [NUMBER NEEDED]', and 'Report the breakdown of flagged buildings by Prime2 FUNC_ID'")

  e <- RUN_ALL$eval    # the all buildings run, the only one in which outbuildings can be flagged
  t1 <- e[e$Tier == 1, ]; t12 <- e[e$Tier %in% c(1, 2), ]
  hl <- data.frame(
    definition = c("Tier 1, all buildings run", "Tier 1 and 2, all buildings run"),
    n_flagged = c(nrow(t1), nrow(t12)),
    n_outbuilding = c(sum(t1$is_outbuilding, na.rm = TRUE),
                      sum(t12$is_outbuilding, na.rm = TRUE)))
  hl$pct_outbuilding <- pct(hl$n_outbuilding, hl$n_flagged)
  emit("F3.4a", hl,
       "This must come from the all buildings run. In the excluding outbuildings run the answer is zero by construction, and quoting that would be a category error. Say in the text which run the percentage is from.")

  fb <- as.data.frame(table(FUNC_ID = as.character(t1$FUNC_ID), useNA = "ifany"),
                      responseName = "n_tier1")
  base <- as.data.frame(table(FUNC_ID = as.character(e$FUNC_ID), useNA = "ifany"),
                        responseName = "n_evaluable")
  fb <- merge(fb, base, by = "FUNC_ID", all = TRUE)
  fb$n_tier1[is.na(fb$n_tier1)] <- 0
  fb$pct_of_tier1 <- pct(fb$n_tier1, nrow(t1))
  fb$tier1_rate_within_code_pct <- pct(fb$n_tier1, fb$n_evaluable)
  fb <- fb[order(-fb$n_tier1), ]
  emit("F3.4b", fb,
       "Two different quantities: pct_of_tier1 is the composition of the flagged list, tier1_rate_within_code is how derelict each code looks. The outbuilding argument in 4.6.5 needs the first; the data infrastructure argument is stronger with both.")

  ob <- e[e$is_outbuilding, ]
  cat(sprintf("\nExcluded outbuildings: n = %d, Linear_Score from %.3f to %.3f, mean %.3f\n",
              nrow(ob), min(ob$Linear_Score), max(ob$Linear_Score), mean(ob$Linear_Score)))
  cat(sprintf("Outbuildings scoring above the Tier 1 boundary of the no outbuilding run (%.4f): %d\n",
              min(RUN_NOOUT$eval$Linear_Score[RUN_NOOUT$eval$Tier == 1]),
              sum(ob$Linear_Score >= min(RUN_NOOUT$eval$Linear_Score[RUN_NOOUT$eval$Tier == 1]))))
}

## ---- F3.5 --------------------------------------------------------------- ##
if (!exists("RUN_ALL")) {
  skipblk("F3.5", "Composition of the ranked shortlist", "Results 4.6.2", "needs F3.1 to have run")
} else {
  blk("F3.5", "Composition of the top 100, 200 and 500",
      "Results 4.6.2, 'Report the composition of the top 100, 200 and 500 by indicator profile, building function and location'")
  e <- RUN_NOOUT$eval[order(-RUN_NOOUT$eval$Linear_Score), ]
  prof <- do.call(rbind, lapply(c(TOPN, sum(RUN_NOOUT$eval$Tier == 1)), function(n) {
    h <- utils::head(e, n)
    d <- data.frame(top_n = n, min_score = min(h$Linear_Score),
                    mean_score = mean(h$Linear_Score),
                    pct_priority = pct(sum(h$Priority == 1), n),
                    pct_any_occupancy_sign = pct(sum(h$Index_Occupancy > 0), n),
                    mean_defence = mean(h$Index_Defence),
                    mean_structural = mean(h$Index_Structural),
                    mean_neglect = mean(h$Index_Neglect),
                    mean_interventions = mean(h$Index_Interventions),
                    mean_occupancy = mean(h$Index_Occupancy))
    for (v in IND20) d[[paste0("pct_", v)]] <- pct(sum(h[[v]] > 0), n)
    d
  }))
  emit("F3.5a", prof,
       "The last row is the Tier 1 quota of the excluding outbuildings run, so the shortlist and the tier can be compared directly.")
  if ("FUNC_ID" %in% names(e)) {
    fn <- do.call(rbind, lapply(TOPN, function(n) {
      h <- utils::head(e, n)
      tt <- as.data.frame(table(FUNC_ID = as.character(h$FUNC_ID)), responseName = "n")
      tt$top_n <- n; tt$pct <- pct(tt$n, n); tt[order(-tt$n), ]
    }))
    emit("F3.5b", fn, "Building function mix of the shortlist.")
  }
  ## location, if any areal unit travelled with the city file
  lc <- find_col(e, c("^ED$", "Small_Area", "SA_GUID", "Electoral", "EDNAME"))
  if (!is.na(lc)) {
    loc <- do.call(rbind, lapply(TOPN, function(n) {
      h <- utils::head(e, n)
      tt <- as.data.frame(table(unit = as.character(h[[lc]])), responseName = "n")
      tt$top_n <- n; tt$pct <- pct(tt$n, n)
      utils::head(tt[order(-tt$n), ], 15)
    }))
    emit("F3.5c", loc,
         paste0("Areal unit taken from the column '", lc,
                "'. Top 15 units per shortlist length, which is the 'and location' half of the section."))
  } else {
    cat("\nNo areal unit column in the city file, so the location half of this section is not computed.\n")
    cat("Point F_RANK3 at the point export, which carries Lat, Lon and the Small Area join, and merge it in if you want the shortlist broken down by area.\n")
  }
}

## ---- F3.6 --------------------------------------------------------------- ##
if (!exists("RUN_ALL")) {
  skipblk("F3.6", "Ranking with and without outbuildings", "Results 4.6.2", "needs F3.1 to have run")
} else {
  blk("F3.6", "How much the top of the list changes when outbuildings are excluded",
      "Results 4.6.2, 'Report both rankings, including and excluding outbuildings, and note how much the top of the list changes between them'")
  a <- RUN_ALL$eval[order(-RUN_ALL$eval$Linear_Score), "Bldg_GUID"]
  b <- RUN_NOOUT$eval[order(-RUN_NOOUT$eval$Linear_Score), "Bldg_GUID"]
  ov <- do.call(rbind, lapply(TOPN, function(n) {
    ta <- utils::head(a, n); tb <- utils::head(b, n)
    data.frame(top_n = n, in_both = length(intersect(ta, tb)),
               overlap_pct = pct(length(intersect(ta, tb)), n),
               dropped_as_outbuilding = sum(RUN_ALL$eval$is_outbuilding[
                 match(ta, RUN_ALL$eval$Bldg_GUID)], na.rm = TRUE),
               promoted_from_below = length(setdiff(tb, ta)))
  }))
  emit("F3.6", ov,
       "dropped_as_outbuilding is how many of the all buildings top n carry the outbuilding code, and promoted_from_below is how far the filtered list has to reach to refill. Those two numbers are the sentence the section asks for.")
}

## ---- F3.7 --------------------------------------------------------------- ##
if (!exists("RUN_ALL")) {
  skipblk("F3.7", "Score distribution by tier", "Results 4.6.1", "needs F3.1 to have run")
} else {
  blk("F3.7", "Score distribution by tier",
      "Results 4.6.1, the mean, standard deviation and range by tier, and the two features that look like errors")
  by_tier <- function(run) {
    e <- run$eval
    d <- do.call(rbind, lapply(sort(unique(e$Tier)), function(t) {
      s <- e$Linear_Score[e$Tier == t]
      data.frame(run = run$label, tier = t, n = length(s), mean = mean(s),
                 sd = sd(s), min = min(s), median = median(s), max = max(s))
    }))
    un <- run$all$Linear_Score[run$all$has_target_building == 0]
    d <- rbind(d, data.frame(run = run$label, tier = 4, n = length(un),
                             mean = mean(un), sd = sd(un), min = min(un),
                             median = median(un), max = max(un)))
    d
  }
  st <- rbind(by_tier(RUN_ALL), by_tier(RUN_NOOUT))
  emit("F3.7", st,
       "Tier 4 carries a score only because the indicators are all zero for an unevaluable building, so its row is an artefact, not a measurement. Never summarise the score across all buildings, only across the evaluable ones. If Tier 1's minimum sits below Tier 2's minimum, that is the priority admission working as designed and needs the sentence the document already drafts.")
}

## ---- F3.8 --------------------------------------------------------------- ##
if (!exists("RUN_ALL")) {
  skipblk("F3.8", "Zero indicator share and index profiles", "Results 4.6.1", "needs F3.1 to have run")
} else {
  blk("F3.8", "Baseline share and the distribution of each index at city scale",
      "Results 4.6.1, 'Report the proportion of buildings scoring zero on every indicator' and 'Report the distribution of each of the five component indices'")
  e <- RUN_NOOUT$eval
  M <- as.matrix(e[, IND20]) > 0
  zero <- rowSums(M) == 0
  b <- data.frame(
    quantity = c("evaluable buildings", "zero on all 20 indicators",
                 "zero on all 20, pct", "one indicator only", "two or more",
                 "mean indicators present", "max indicators present"),
    value = c(nrow(e), sum(zero), pct(sum(zero), nrow(e)),
              sum(rowSums(M) == 1), sum(rowSums(M) >= 2),
              mean(rowSums(M)), max(rowSums(M))))
  emit("F3.8a", b, "This is the baseline the section asks you to characterise.")

  idx <- c("Index_Defence", "Index_Structural", "Index_Neglect",
           "Index_Interventions", "Index_Occupancy")
  di <- do.call(rbind, lapply(idx, function(v) {
    x <- e[[v]]
    data.frame(index = v, mean = mean(x), sd = sd(x), median = median(x),
               max = max(x), pct_above_zero = pct(sum(x > 0), length(x)))
  }))
  emit("F3.8b", di, NULL)

  pv <- data.frame(indicator = IND20, group = unname(GROUP_OF[IND20]),
                   n_present = colSums(M),
                   pct_of_evaluable = pct(colSums(M), nrow(e)))
  pv <- pv[order(-pv$n_present), ]
  emit("F3.8c", pv,
       "City scale prevalence per indicator. Put this beside the sample prevalence in F1.2: the section asks which indicators drive the score at city scale as opposed to in the training sample, and the two columns side by side are that comparison.")
}

## ---- F3.9 --------------------------------------------------------------- ##
if (!exists_file(F_BIG)) {
  skipblk("F3.9", "Large building run", "Appendix D.2",
          "set F_BIG to the large building extraction, one row per image, with a building identifier")
} else {
  blk("F3.9", "Total images captured for the large buildings and the mean per building",
      "Appendix D.2, 'Total images captured for the 727 large buildings, and the mean per building'")
  bg <- rd(F_BIG)
  idc <- intersect(c("Bldg_GUID", "GUID", "GEO_ID"), names(bg))[1]
  n_b <- length(unique(bg[[idc]]))
  per <- as.numeric(table(bg[[idc]]))
  s <- data.frame(quantity = c("image rows", "distinct buildings",
                               "mean images per building", "median", "min", "max"),
                  value = c(nrow(bg), n_b, mean(per), median(per), min(per), max(per)))
  emit("F3.9", s, NULL)
}


## ####################################################################### ##
##  F4.  COMPARISON AND FUNNEL
## ####################################################################### ##

## ---- F4.1 --------------------------------------------------------------- ##
if (!exists_file(F_BLDG)) {
  skipblk("F4.1", "Viable properties", "Results 4.7.5 [NUMBER NEEDED]",
          paste("needs", F_BLDG))
} else {
  blk("F4.1", "Viable properties, GeoDirectory vacancies at the low end of the visual score",
      "Results 4.7.5 [NUMBER NEEDED: how many GeoDirectory vacant properties score in the lowest quartile of visual dereliction]")

  bb <- rd(F_BLDG)
  tcol <- find_col(bb, c("^Vacancy_Tier_NoOut$", "Vacancy_Tier"))
  bb$tier_num <- substr(as.character(bb[[tcol]]), 1, 1)
  ev <- bb[bb$tier_num %in% c("1", "2", "3") & is.finite(bb$Linear_Score), ]
  qs <- quantile(ev$Linear_Score, c(0, .25, .5, .75, 1))
  ev$quartile <- cut(ev$Linear_Score, breaks = qs, include.lowest = TRUE,
                     labels = c("Q1 least derelict", "Q2", "Q3", "Q4 most derelict"))
  cat("Quartile cut points of Linear_Score on ", nrow(ev), " evaluable buildings:\n", sep = "")
  print(round(qs, 4))

  gdcols <- intersect(c("GeoDir_Time_Matched_Vacant", "GeoDir_Q3_2022_Vacant",
                        "GeoDir_Q4_2023_Vacant", "GeoDir_Persistent_Vacant"), names(bb))
  res <- do.call(rbind, lapply(gdcols, function(g) {
    v <- ev[num0(ev[[g]]) == 1, ]
    tt <- table(factor(v$quartile, levels = levels(ev$quartile)))
    data.frame(geodirectory_definition = g, n_flagged = nrow(v),
               Q1 = as.integer(tt[1]), Q2 = as.integer(tt[2]),
               Q3 = as.integer(tt[3]), Q4 = as.integer(tt[4]),
               pct_in_Q1 = pct(as.integer(tt[1]), nrow(v)),
               pct_in_Q1_or_Q2 = pct(as.integer(tt[1]) + as.integer(tt[2]), nrow(v)),
               median_score = median(v$Linear_Score))
  }))
  emit("F4.1a", res,
       "Q1 is the lowest quartile of visual dereliction, so the Q1 column is the number the viable properties argument needs: recorded vacant, externally sound, therefore cheapest to bring back into use. Quartiles are computed on the evaluable population, not on the flagged subset, which is what makes the comparison meaningful.")

  ## a stricter reading, for the sentence about externally sound buildings
  if (exists("city") && "Bldg_GUID" %in% names(city) &&
      all(c("Index_Defence", "Index_Structural", "Index_Neglect",
            "Index_Interventions") %in% names(city))) {
    idc <- intersect(c("GUID", "Bldg_GUID"), names(bb))[1]
    mm <- merge(bb[, c(idc, gdcols[1])], city[, c("Bldg_GUID", "Index_Defence",
                "Index_Structural", "Index_Neglect", "Index_Interventions")],
                by.x = idc, by.y = "Bldg_GUID")
    sound <- rowSums(mm[, c("Index_Defence", "Index_Structural", "Index_Neglect")]) == 0
    v <- num0(mm[[gdcols[1]]]) == 1
    s <- data.frame(quantity = c("GeoDirectory flagged and matched",
                                 "of those, zero defence, structural and neglect indicators",
                                 "pct externally sound"),
                    value = c(sum(v), sum(v & sound), pct(sum(v & sound), sum(v))))
    emit("F4.1b", s,
         "A stricter definition of externally sound than the quartile: no dereliction indicator of any kind. Report whichever you argue for, and say which.")
  }
}

## ---- F4.2 --------------------------------------------------------------- ##
blk("F4.2", "TABLE 4.1 capture funnel",
    "Results 4.2.2 [TABLE 4.1: capture funnel, from buildings in the area of interest, to buildings with a candidate panorama, to buildings passing the date filter, to images retrieved, to images successfully evaluated]")
{
  ## Counted rows are computed here. Stated rows come from your notes and are
  ## labelled as such so the table never implies a count it does not have.
  fn <- data.frame(
    step = c("building polygons in Cork City",
             "above the 24 square metre floor",
             "with a panorama within 35 m",
             "with a panorama within 35 m after the date cutoff",
             "locations targeted at stage 3",
             "images acquired at stage 4",
             "rows in the VLM output",
             "rows after deduplication",
             "buildings evaluated, has_target_building true",
             "buildings unevaluable, occluded"),
    n = NA_real_,
    source = c(rep("stated in notes, not recomputed here", 5),
               rep("stated in notes, not recomputed here", 2),
               "counted from the city file", "counted from the city file",
               "counted from the city file"),
    stringsAsFactors = FALSE)
  fn$n[1:7] <- c(109272, 87927, 79638, 63624, 60993, 59514, 59544)
  if (CITY_OK) {
    fn$n[8]  <- nrow(city)
    fn$n[9]  <- sum(city$has_target_building == 1)
    fn$n[10] <- sum(city$has_target_building == 0)
  } else {
    fn$source[8:10] <- paste("needs", F_CITY)
  }
  fn$pct_of_previous <- c(NA, 100 * fn$n[-1] / fn$n[-length(fn$n)])
  fn$pct_of_polygons <- 100 * fn$n / fn$n[1]
  emit("F4.2", fn,
       "Rows 1 to 7 are transcribed from your notes and are NOT recomputed by this script; the source column says so and must stay in the caption or be replaced once you point this block at the stage 3 output. Row 6 is derived by subtraction in the notes rather than counted, which is worth a footnote. Only the last three rows are counted here.")
}

## ---- F4.3 --------------------------------------------------------------- ##
if (!exists_file(F_TRACK, F_REVIEW)) {
  skipblk("F4.3", "Non capture reasons", "Results 4.2.3, Appendix D.2",
          "set F_TRACK and F_REVIEW to the final run tracker and review pair, the one whose row counts sum to 60,993")
} else {
  blk("F4.3", "Non capture reasons as a distribution",
      "Results 4.2.3, Appendix D.2 'Non-capture reasons as a distribution'")
  tr <- rd(F_TRACK); rv <- rd(F_REVIEW)
  rc <- find_col(rv, c("Review_Reason", "reason", "status"))
  cat(sprintf("tracker rows %d | review rows %d | sum %d\n",
              nrow(tr), nrow(rv), nrow(tr) + nrow(rv)))
  if (!is.na(rc)) {
    ## First match wins, so the order of this list matters. Moderate drift is
    ## tested before severe drift, and the date rule last, because the word
    ## "date" appears inside more than one reason string.
    pats <- c(moderate_drift     = "moderate",
              metadata_failure   = "metadata|deprecat",
              severe_drift       = "abort|severe",
              no_acceptable_date = "older than|threshold|date")
    txt <- tolower(as.character(rv[[rc]]))
    cls <- rep("unmatched", length(txt))
    for (nm in names(pats)) {
      hit <- cls == "unmatched" & grepl(pats[[nm]], txt, perl = TRUE)
      cls[hit] <- nm
    }
    d <- as.data.frame(table(reason = cls), responseName = "n")
    d$pct_of_review_rows <- pct(d$n, nrow(rv))
    d$counts_as <- ifelse(d$reason == "moderate_drift",
                          "RETAINED image, not a non capture",
                          ifelse(d$reason == "unmatched",
                                 "read these and extend the pattern list",
                                 "non capture"))
    d <- d[order(-d$n), ]
    fails <- sum(d$n[!d$reason %in% c("moderate_drift", "unmatched")])
    cat(sprintf("non captures (excluding moderate drift and unmatched): %d\n", fails))
    emit("F4.3a", d,
         "This is a first match partition, so the rows sum to the review file. Moderate drift is a retained image and must be reported separately from the failure total, which is what the counts_as column enforces. If 'unmatched' is not zero, read those strings and extend the pattern list before quoting any percentage.")
    cat("\nFull Review_Reason table, for the categories the patterns above may have missed:\n")
    print(utils::head(sort(table(rv[[rc]]), decreasing = TRUE), 20))
  }
  ## failure clustering by electoral division, if an ED column exists
  ec <- find_col(tr, c("^ED$", "electoral", "ED_NAME", "EDNAME"))
  if (!is.na(ec)) {
    cat("\nED column found: ", ec, ". Chi square of failure count by ED:\n", sep = "")
    cat("Build the contingency table from your own success flag column and run chisq.test; the column name for success differs between runs so it is not hardcoded here.\n")
  }
}


## ####################################################################### ##
##  F5.  FIELD VERIFICATION AND THE REMAINING EXTERNAL DATASETS
## ####################################################################### ##

## ---- F5.1 --------------------------------------------------------------- ##
blk("F5.1", "Field verification sample design",
    "Results 4.9, the design table that must be reported before any fieldwork result")
{
  ss <- function(N, e, p = 0.5, conf = 0.95) {
    z <- qnorm(1 - (1 - conf) / 2)
    n0 <- z^2 * p * (1 - p) / e^2
    ceiling(n0 / (1 + (n0 - 1) / N))
  }
  strata <- data.frame(stratum = c("Visible only", "Recorded only", "Both"),
                       N = c(652, 505, 259))
  strata$n_for_10pct <- mapply(ss, strata$N, 0.10)
  strata$n_for_5pct  <- mapply(ss, strata$N, 0.05)
  emit("F5.1a", strata,
       "Simple random sample within stratum, 95 per cent confidence, finite population correction, p assumed 0.5 as the most conservative case. The N column comes from the Analysis 8 two by two and should be checked against it rather than typed in. This reproduces the 84 / 242, 81 / 219 and 71 / 155 figures already in 4.9.")
  if (exists_file(F_FIELDS)) {
    fs <- rd(F_FIELDS)
    sc <- find_col(fs, c("stratum", "strata", "group"))
    cat("\nThe drawn sample, ", basename(F_FIELDS), ":\n", sep = "")
    if (!is.na(sc)) print(table(fs[[sc]])) else cat("rows: ", nrow(fs), "\n", sep = "")
    cat("State that the draw was made before observation, with a fixed seed, and was not redrawn.\n")
  }
}

## ---- F5.2 --------------------------------------------------------------- ##
if (!exists_file(F_FIELDR)) {
  skipblk("F5.2", "Field verification results", "Results 4.9 [RESULTS OUTSTANDING]",
          "set F_FIELDR once the visits are done. Expected columns: a building identifier, a stratum label, and an outcome coded clearly vacant / clearly occupied / cannot tell")
} else {
  blk("F5.2", "Field verification results by stratum",
      "Results 4.9 [RESULTS OUTSTANDING: agreement between the classification and observed condition, for each stratum separately]")
  fr <- rd(F_FIELDR)
  sc <- find_col(fr, c("stratum", "strata", "group"))
  oc <- find_col(fr, c("outcome", "observed", "field_verdict", "verdict", "result"))
  if (is.na(sc) || is.na(oc)) {
    cat("Could not find a stratum column and an outcome column. Found: ",
        paste(names(fr), collapse = ", "), "\n", sep = "")
  } else {
    o <- tolower(as.character(fr[[oc]]))
    fr$is_vacant  <- grepl("vacant|derelict", o) & !grepl("not|occupied", o)
    fr$cannot     <- grepl("cannot|unclear|unknown|unsure", o)
    res <- do.call(rbind, lapply(unique(fr[[sc]]), function(s) {
      d <- fr[fr[[sc]] == s, ]
      dd <- d[!d$cannot, ]
      ci <- wilson(sum(dd$is_vacant), nrow(dd))
      data.frame(stratum = s, visited = nrow(d),
                 cannot_tell = sum(d$cannot),
                 cannot_tell_pct = pct(sum(d$cannot), nrow(d)),
                 resolved = nrow(dd), confirmed_vacant = sum(dd$is_vacant),
                 precision_pct = pct(sum(dd$is_vacant), nrow(dd)),
                 lo95 = 100 * ci[1], hi95 = 100 * ci[2])
    }))
    emit("F5.2a", res,
         "The Visible only row is the quantity the whole section exists to estimate. The Both row is the control: if the two precisions are close, the 652 are as real as the 259, and that is the finding.")
    vo <- res[grepl("visible", tolower(res$stratum)), ]
    bo <- res[grepl("both",    tolower(res$stratum)), ]
    if (nrow(vo) == 1 && nrow(bo) == 1) {
      m <- matrix(c(vo$confirmed_vacant, vo$resolved - vo$confirmed_vacant,
                    bo$confirmed_vacant, bo$resolved - bo$confirmed_vacant),
                  nrow = 2, byrow = TRUE)
      cat("\nTest of the difference between strata:\n"); print(fisher.test(m))
    }
    cat("\nEvery disagreement, to be described individually in the text:\n")
    print(fr[!fr$is_vacant & !fr$cannot, ])
  }
}

## ---- F5.3 --------------------------------------------------------------- ##
if (!exists_file(F_DSR)) {
  skipblk("F5.3", "Derelict Sites Register match rate", "Appendix D.3",
          "set F_DSR to the register. It needs either a Bldg_GUID or coordinates to join on; if it is an address list it cannot be matched by this script and needs geocoding first")
} else {
  blk("F5.3", "Derelict Sites Register match rate", "Appendix D.3")
  ds <- rd(F_DSR)
  cat("columns: ", paste(names(ds), collapse = ", "), "\n", sep = "")
  idc <- intersect(c("Bldg_GUID", "GUID", "GEO_ID"), names(ds))[1]
  if (is.na(idc) || !exists("RUN_ALL")) {
    cat("No building identifier in the register, or F3.1 has not run. A spatial join is needed; do it in ArcGIS and export a Bldg_GUID list, then point F_DSR at that.\n")
  } else {
    e <- RUN_NOOUT$eval
    e$on_register <- e$Bldg_GUID %in% ds[[idc]]
    tt <- table(tier = e$Tier, on_register = e$on_register)
    print(tt)
    d <- data.frame(tier = rownames(tt), n = rowSums(tt),
                    on_register = tt[, "TRUE"],
                    match_rate_pct = 100 * tt[, "TRUE"] / rowSums(tt))
    emit("F5.3", d, "Register entries with no matched building are the other half of this and should be counted too.")
  }
}

## ---- F5.4 --------------------------------------------------------------- ##
if (!exists_file(F_RESOL)) {
  skipblk("F5.4", "Resolution sensitivity", "Methods 3.11.4, Appendix D.2",
          "set F_RESOL to FOConnor_Resolution_Discrepancies.csv. The figures already in 3.11.4 are complete, so this block is a verification pass rather than a gap")
} else {
  blk("F5.4", "Resolution sensitivity, verification of the figures already in 3.11.4",
      "Methods 3.11.4, Appendix D.2")
  rz <- rd(F_RESOL)
  cat("rows ", nrow(rz), " | columns: ", paste(names(rz), collapse = ", "), "\n", sep = "")
  cat("The structure of this file varies. Confirm the column layout against the printout above, then compare the counts to the 348 of 405 identical cells, the 89.9 / 90.9 / 91.1 pairwise agreements and the 24 against 13 asymmetry quoted in 3.11.4.\n")
  print(utils::head(rz, 5))
}


## ####################################################################### ##
##  F9.  WHAT THIS SCRIPT CANNOT COMPUTE
## ####################################################################### ##

blk("F9.1", "Outstanding items this script cannot reach, and what each one needs",
    "Appendix D, the residue after everything above has run")
gap <- data.frame(
  item = c(
    "Road network and building coverage percentages, all dates and usable dates",
    "Effect of the five pass metadata merge, count and percentage improved",
    "ESB and CSO electricity comparison by Electoral Division",
    "Socio economic association with the Pobal HP Deprivation Index",
    "Total inference cost of the city wide run",
    "Intra model reliability, 10 images by 8 iterations",
    "Facade extraction accuracy and the 65.45 per cent facade rate",
    "Manual overrides of camera selection, count and effect",
    "The 151 building gap between 60,241 and 60,090",
    "Photo grids and every figure"),
  needs = c(
    "the stage 1 and 2 coverage outputs, road centreline length and the per date panorama join",
    "the five metadata passes as separate logs or a pass number column",
    "an ESB or CSO consumption table keyed to Electoral Division, which is not in any file listed in section 0",
    "the Pobal HP index joined to Small Area, then a correlation against SA_results_T1Geo_Readable.csv",
    "the batch API billing export or the token counts per request",
    "Intra_Model_Variance_Test.xlsx, which is a different shape from every file here",
    "the facade validation layers read into R, 2,372 plus 1,812 rows",
    "a flag on the targeting output marking manually overridden camera choices",
    "the ArcGIS or ModelBuilder step that writes Building_results_T1Geo_Readable.csv from the combined VLM output. This is a join audit, not a statistic",
    "ArcGIS Pro and the image folders"),
  stringsAsFactors = FALSE)
emit("F9.1", gap,
     "Point the relevant handle in section 0 at the file and rerun if any of these becomes available. Items with no file cannot be closed by code.")

cat("\n\n")
cat(strrep("=", 78), "\n", sep = "")
cat("DONE. Log: ", LOGFILE, "\n", sep = "")
cat("Tables: ", file.path(OUT, "tables"), "\n", sep = "")
cat("Search the log for 'FILLS:' to get the checklist, and for 'SKIPPED' to see what still needs a path.\n")
cat(strrep("=", 78), "\n", sep = "")

sink()
