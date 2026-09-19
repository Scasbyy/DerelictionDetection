###############################################################################
#  VACANCY COMPARISON ANALYSIS  --  Cork City
#  Shea Casby, MSc Geoinformatics, UCC.   Version 4 (prevalence matching).
#
#  NEW FILE. Does not overwrite Vacancy_Comparison_Analysis.R.
#  Writes Vacancy_Compare_T1Geo.gpkg, SA_results_T1Geo.csv,
#  Building_results_T1Geo.csv so the previous run is left intact.
#
#  Two currencies of vacancy (prevalence matching):
#    GeoDirectory (~1.7%):  Tier 1 only.  Flag_LT / my_rate_lt.
#                           Tier 2 is treated as occupied in this comparison.
#    Census / CSO (~5.55%): Tier 1 + Tier 2, residential only.
#                           Flag_All / my_rate_res.
#
#  Reads everything straight out of StreetViewTesting.gdb. Nothing is exported
#  to shapefile at any point.
#
#  Results are tagged [A1], [V3], [M2] and so on. Those tags match the rows in
#  Vacancy_Results_Log.xlsx. When you see a tag, type the number in.
#
#  RUN IT IN BLOCKS. There are four STOP points and they are there for a reason.
###############################################################################

## ============================== 0. CONFIG ============================== ##
## Edit this block only.

GDB  <- "C:/Users/sheac/Documents/ArcGIS/Projects/StreetViewTesting/StreetViewTesting.gdb"
OUT  <- "C:/Users/sheac/Documents/College/Dissertation/Analysis2"  # where results go
CSV  <- "C:/Users/sheac/Documents/College/Dissertation/P2_GeminiResults_rank3_point_short_res.csv"
GPKG <- file.path(OUT, "Vacancy_Compare_T1Geo.gpkg")

## Layer names, read out of your geodatabase.
L_SA     <- "Cork_SAPs_Analysis"        # Small Areas WITH the Census columns
L_CITY   <- "CorkCity_Boundary"
L_BLDG   <- "P2_GeminiResults_rank3"      # building footprints, keyed on GUID
L_GD22   <- "GeoDirectoryQ32022_CorkCity"     # Q3 2022, has VACANT and DERELICT
L_GD23   <- "GeoDirectoryQ42023_CorkCity"   # Q4 2023, has vacant and derelict

## WARNING. GeoDirectoryQ32022_CorkCity, the layer you named, holds only
## OBJECTID, Shape, GUID and ORIG_FID. It has no VACANT or DERELICT column, so
## it cannot be used here. GeoDirectoryQ322_Cork and GeoDirectoryQ32022_cork
## both carry the full 43 fields. Section 2 prints the row count of whichever
## you point at, so you can confirm you picked the Cork City one.

CUTOFF   <- as.Date("2023-05-01")   # photos before this get Q3 2022, after get Q4 2023
MIN_EVAL <- 20                      # minimum evaluable buildings per Small Area
NSIM     <- 999                     # LISA permutations
ITM      <- 2157                    # Irish Transverse Mercator
USE_POLYGONS <- TRUE                # FALSE falls back to a nearest-point match
MATCH_TOL    <- 8                   # metres, only used when USE_POLYGONS is FALSE

set.seed(20260905)
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
if (!dir.exists(OUT)) stop("Could not create output folder: ", OUT)
cat("\nOutput folder:\n  ", normalizePath(OUT, winslash = "/"), "\n", sep = "")
cat("GeoPackage:\n  ", GPKG, "\n", sep = "")

## ============================= 1. PACKAGES ============================= ##
need <- c("sf","dplyr","spdep","lmtest")
new  <- setdiff(need, rownames(installed.packages()))
if (length(new)) install.packages(new)
invisible(lapply(need, library, character.only = TRUE))
cat("sf", as.character(packageVersion("sf")),
    " spdep", as.character(packageVersion("spdep")), "\n")

## Close ArcGIS Pro before running this. R reads the geodatabase through GDAL,
## and Pro holding a lock on it is the commonest reason st_read fails here.
## If you want to see what else is in there:
##   print(st_layers(GDB), n = 400)

## ===================== 2. SMALL AREAS AND THE CENSUS ==================== ##
sa <- st_read(GDB, layer = L_SA, quiet = TRUE) |> st_transform(ITM)
cat("[A5] Small Areas loaded:", nrow(sa), "\n")

## Census 2022 occupancy status lives in SAPS table 6.8:
##   T6_8_O    occupied
##   T6_8_TA   temporarily absent
##   T6_8_UHH  unoccupied holiday homes
##   T6_8_OVD  other vacant dwellings
##   T6_8_T    total dwellings
## Three defensible definitions of "vacant" come out of that. Print all three
## city-wide and use whichever reproduces the 5.555% you already quote.
tot <- sum(sa$T6_8_T, na.rm = TRUE)
cat("\n--- which Census definition gives your 5.555%? ---\n")
cat(sprintf("  other vacant only        : %5d / %d = %.3f%%\n",
            sum(sa$T6_8_OVD, na.rm=TRUE), tot, 100*sum(sa$T6_8_OVD,na.rm=TRUE)/tot))
cat(sprintf("  vacant + holiday homes   : %5d / %d = %.3f%%\n",
            sum(sa$T6_8_OVD+sa$T6_8_UHH, na.rm=TRUE), tot,
            100*sum(sa$T6_8_OVD+sa$T6_8_UHH,na.rm=TRUE)/tot))
cat(sprintf("  everything not occupied  : %5d / %d = %.3f%%\n",
            tot-sum(sa$T6_8_O,na.rm=TRUE), tot,
            100*(tot-sum(sa$T6_8_O,na.rm=TRUE))/tot))

## >>> STOP 1. Set this to whichever line matched, then carry on.
CEN_VAC <- sa$T6_8_OVD      # <-- change if a different line matched
sa$cen_vac <- CEN_VAC
sa$cen_tot <- sa$T6_8_T
sa$SA_KEY  <- as.character(sa$SA_GUID_2022)

city <- st_read(GDB, layer = L_CITY, quiet = TRUE) |> st_transform(ITM)
inside <- st_within(st_point_on_surface(st_geometry(sa)),
                    st_union(city), sparse = FALSE)[,1]
sa <- sa[inside, c("SA_KEY","cen_vac","cen_tot")]
cat("[A5] Small Areas inside the city boundary:", nrow(sa), "\n")
## Selected by interior point, not clipped. Clipping makes sliver polygons at
## the edge whose dwelling counts are wrong.

## ==================== 3. YOUR BUILDINGS AND THEIR DATES ================= ##
b <- read.csv(CSV, stringsAsFactors = FALSE)
b <- b[, c("GUID","Linear_Score","City_Rank","Priority_Admission",
           "Vacancy_Tier_All","Vacancy_Tier_NoOut","has_target_building",
           "Capture_Date_Converted","Shape_Area2","Lat","Lon","Is_Residential")]
b$Is_Residential <- as.integer(b$Is_Residential)
n_res_na <- sum(is.na(b$Is_Residential))
## Blanks in the CSV become NA. Treat those as not residential (0), matching
## the coding you described (1 = residential, 0 = everything else).
if (n_res_na > 0) {
  cat("     Is_Residential is NA for", n_res_na, "buildings; coding those as 0\n")
  b$Is_Residential[is.na(b$Is_Residential)] <- 0L
}
cat("\n[A1] buildings loaded:", nrow(b), "\n")
cat("     residential (Is_Residential==1):", sum(b$Is_Residential == 1),
    "  non-residential:", sum(b$Is_Residential == 0), "\n")

b$tier      <- substr(as.character(b$Vacancy_Tier_NoOut), 1, 1)  # 1 2 3 4 or E
b$Evaluable <- as.integer(b$tier %in% c("1","2","3"))
b$Flag_LT   <- as.integer(b$tier == "1")            # GeoDirectory currency (~1.7%)
b$Flag_All  <- as.integer(b$tier %in% c("1","2"))    # Census currency (~5.55%)
b$cap_date  <- as.Date(substr(b$Capture_Date_Converted, 1, 10))
b$vintage   <- ifelse(b$cap_date < CUTOFF, "Q3_2022", "Q4_2023")

cat("[A2] evaluable, outbuildings excluded:", sum(b$Evaluable), "\n")
cat("[A3] Tier 1  :", sum(b$Flag_LT),
    sprintf("(%.3f%%)", 100*sum(b$Flag_LT)/sum(b$Evaluable)), "\n")
cat("[A4] Tier 1+2:", sum(b$Flag_All),
    sprintf("(%.3f%%)", 100*sum(b$Flag_All)/sum(b$Evaluable)), "\n")
## Expect 60090 / 53436 / 911 (1.705%) / 2968 (5.554%).

cat("\n[V1] photographs by year, evaluable buildings only\n")
print(table(format(b$cap_date[b$Evaluable==1], "%Y")))
cat("\n[V2] which GeoDirectory snapshot each building is matched to\n")
print(table(b$vintage[b$Evaluable==1]))

## Your capture dates have a 16 month hole in them: nothing between October
## 2022 and March 2024. Any cutoff inside that hole gives an identical split,
## so the choice of 1 May 2023 is arbitrary in the best sense. Check it:
for (d in c("2023-01-01","2023-05-01","2023-12-01"))
  cat(sprintf("   cutoff %s -> %d before, %d after\n", d,
      sum(b$cap_date <  as.Date(d) & b$Evaluable==1),
      sum(b$cap_date >= as.Date(d) & b$Evaluable==1)))

bpt <- st_as_sf(b, coords = c("Lon","Lat"), crs = 4326) |> st_transform(ITM)

## ======================= 4. THE TWO GEODIRECTORY SNAPSHOTS ============== ##
## Field names are UPPER CASE on the 2022 layer and lower case on the 2023 one,
## so everything is forced to upper case on load and the rest of the script
## stops caring.
read_gd <- function(layer, tag) {
  g <- st_read(GDB, layer = layer, quiet = TRUE) |> st_transform(ITM)
  ## toupper() renames Shape -> SHAPE and leaves sf pointing at the old name.
  ## Reset the active geometry or st_join later fails with sf_column error.
  geom <- attr(g, "sf_column")
  names(g) <- toupper(names(g))
  st_geometry(g) <- toupper(geom)
  yn <- function(x) as.integer(toupper(trimws(as.character(x))) %in% c("Y","YES","1","TRUE"))
  g$gd_vac <- yn(g$VACANT)
  g$gd_der <- yn(g$DERELICT)
  g$gd_any <- as.integer(g$gd_vac == 1 | g$gd_der == 1)
  cat(sprintf("\n[A7] %s (%s): %d records, %.3f%% vacant or derelict\n",
              tag, layer, nrow(g), 100*mean(g$gd_any)))
  g
}
gd22 <- read_gd(L_GD22, "GeoDirectory Q3 2022")
gd23 <- read_gd(L_GD23, "GeoDirectory Q4 2023")

## The Q3 2022 figure should land near the 1.705% you anchored your tiers on.
## If it is far off, you are probably reading a county-wide layer rather than
## the Cork City one, or a subset such as GeoDirectoryQ322_vacantderelict.

## >>> STOP 2. Confirm both row counts and both rates look right.

## ====================== 5. THE COVERAGE CHECK ========================== ##
## Street View recoverage is not spread evenly. Google reimages the core and
## the main roads far more often than the suburbs, and that is also where the
## vacancy is. So the two capture years are photographing different parts of
## the city, and the flag rate difference between them is mostly composition.
## This block measures how much of it is composition. Section 2 of the guide
## has the reasoning. Budget ten minutes.
ev <- b[b$Evaluable == 1, ]
cat("\n========================= COVERAGE CHECK =========================\n")
vt <- ev |> group_by(vintage) |>
  summarise(n = n(),
            tier1  = 100*mean(Flag_LT),
            tier12 = 100*mean(Flag_All),
            score  = mean(Linear_Score),
            area   = mean(Shape_Area2, na.rm = TRUE), .groups = "drop")
print(as.data.frame(vt), digits = 4)
cat(sprintf("\n[V3] city-wide, Tier 1 is %.2f times higher on the newer imagery\n",
            vt$tier1[vt$vintage=="Q4_2023"] / vt$tier1[vt$vintage=="Q3_2022"]))
cat(sprintf("[V4] %.1f%% of the stock is newer imagery, supplying %.1f%% of Tier 1\n",
            100*mean(ev$vintage=="Q4_2023"),
            100*mean(ev$vintage[ev$Flag_LT==1]=="Q4_2023")))
cat("     Report that as coverage, not as error. The newer imagery is the\n")
cat("     centre and the main roads, which is where the vacancy is.\n")

evp <- bpt[bpt$Evaluable == 1, ]
xy  <- st_coordinates(evp)
newv <- as.integer(evp$vintage == "Q4_2023")
t1v  <- evp$Flag_LT

## --- distance rings: does the difference survive controlling for how far
##     out you are? In the core, where both years are well represented, the
##     two should agree.
ctr <- st_transform(st_sfc(st_point(c(-8.4756, 51.8985)), crs = 4326), ITM)
km  <- as.numeric(st_distance(evp, ctr)) / 1000
ring <- cut(km, c(0,0.5,1,1.5,2,3,5,Inf),
            labels = c("<0.5","0.5-1","1-1.5","1.5-2","2-3","3-5","5+"))
cat("\n[V5] Tier 1 rate by distance from the city centre and capture year\n")
rt <- do.call(rbind, lapply(levels(ring), function(r) {
  k <- ring == r & !is.na(ring)
  o <- t1v[k & newv==0]; n <- t1v[k & newv==1]
  if (length(o) < 100 || length(n) < 100) return(NULL)
  data.frame(ring=r, n_2022=length(o), n_2024=length(n),
             pc_2022=round(100*mean(o),3), pc_2024=round(100*mean(n),3),
             ratio=round(mean(n)/mean(o),2))
}))
print(rt, row.names = FALSE)

## --- like with like: compare buildings inside the same small neighbourhood,
##     using only cells that hold a decent number of both capture years.
##     If the ratio shrinks as the cells get smaller, the difference is
##     composition rather than measurement.
cat("\n[V6] Tier 1 rate by capture year, comparing like with like\n")
lw_rows <- do.call(rbind, lapply(c(400, 250, 150), function(cell) {
  key  <- paste(floor(xy[,1]/cell), floor(xy[,2]/cell))
  n_o  <- tapply(1-newv, key, sum); n_n <- tapply(newv, key, sum)
  keep <- names(n_o)[n_o >= 15 & n_n >= 15]
  k    <- key %in% keep
  if (!any(k)) return(NULL)
  r0 <- mean(t1v[k & newv==0]); r1 <- mean(t1v[k & newv==1])
  data.frame(cell_m=cell, mixed_cells=length(keep), buildings=sum(k),
             pc_2022=round(100*r0,3), pc_2024=round(100*r1,3),
             ratio=round(r1/r0,2))
}))
print(lw_rows, row.names = FALSE)
cat("     Compare the last column against [V3]. A ratio that falls as the\n")
cat("     cells shrink means the two capture years are photographing\n")
cat("     different places, not photographing the same places differently.\n")

## >>> STOP 3. This is a limitations table, not a headline result. Record it
##     and move on. Ten minutes is the right amount of time to spend here.

## ================= 6. MATCHING BUILDINGS TO GEODIRECTORY =============== ##
## Both GeoDirectory snapshots are joined to every building, not just the
## matching one. That costs one extra join and buys three measures instead of
## one: the temporally matched status, persistence across both snapshots, and
## newly recorded vacancy.
join_gd <- function(target, gd, prefix) {
  j <- st_join(gd[, c("gd_vac","gd_der","gd_any")], target["GUID"],
               join = st_within, left = FALSE)
  out <- j |> st_drop_geometry() |> group_by(GUID) |>
    summarise(n = n(), vac = as.integer(any(gd_vac == 1)),
              der = as.integer(any(gd_der == 1)),
              any = as.integer(any(gd_any == 1)), .groups = "drop")
  names(out)[-1] <- paste0(prefix, "_", names(out)[-1])
  out
}

if (USE_POLYGONS) {
  ## Only two columns are read, which keeps a very large footprint layer
  ## manageable. Reading every field would pull hundreds of megabytes.
  poly <- st_read(GDB, query = paste0("SELECT GUID, Shape FROM ", L_BLDG),
                  quiet = TRUE) |> st_transform(ITM)
  poly <- poly[poly$GUID %in% b$GUID, ]
  cat("\n[M1] footprints matched to the scored table:", nrow(poly),
      "of", nrow(b), "\n")
  m22 <- join_gd(poly, gd22, "g22")
  m23 <- join_gd(poly, gd23, "g23")
} else {
  ## Fallback: nearest GeoDirectory record within MATCH_TOL metres. Weaker,
  ## because in a terrace the nearest record can belong to next door.
  nn <- function(gd, prefix) {
    i <- st_nearest_feature(bpt, gd)
    d <- as.numeric(st_distance(bpt, gd[i,], by_element = TRUE))
    ok <- d <= MATCH_TOL
    out <- data.frame(GUID = bpt$GUID,
                      n = as.integer(ok),
                      vac = ifelse(ok, gd$gd_vac[i], NA_integer_),
                      der = ifelse(ok, gd$gd_der[i], NA_integer_),
                      any = ifelse(ok, gd$gd_any[i], NA_integer_))
    names(out)[-1] <- paste0(prefix, "_", names(out)[-1]); out
  }
  m22 <- nn(gd22, "g22"); m23 <- nn(gd23, "g23")
}

b <- b |> left_join(m22, by = "GUID") |> left_join(m23, by = "GUID")
for (c_ in c("g22_any","g23_any","g22_vac","g23_vac","g22_der","g23_der"))
  b[[c_]][is.na(b[[c_]])] <- 0L
b$g22_n[is.na(b$g22_n)] <- 0L; b$g23_n[is.na(b$g23_n)] <- 0L

b$gd_matched    <- ifelse(b$vintage == "Q3_2022", b$g22_any, b$g23_any)
b$gd_persistent <- as.integer(b$g22_any == 1 & b$g23_any == 1)
b$gd_new        <- as.integer(b$g22_any == 0 & b$g23_any == 1)
b$gd_gone       <- as.integer(b$g22_any == 1 & b$g23_any == 0)
b$gd_covered    <- as.integer(b$g22_n > 0 | b$g23_n > 0)

ev <- b[b$Evaluable == 1, ]
cat("\n[M2] buildings with at least one GeoDirectory record:",
    sum(ev$gd_covered), sprintf("(%.1f%%)", 100*mean(ev$gd_covered)), "\n")
cat("[M3] recorded vacant or derelict:  Q3 2022 =", sum(ev$g22_any),
    "  Q4 2023 =", sum(ev$g23_any),
    "  persistent =", sum(ev$gd_persistent),
    "  newly recorded =", sum(ev$gd_new), "\n")

## A free cross-check on the coverage question from section 5. Compare how
## much more often you flag on newer imagery against how much more often
## GeoDirectory records vacancy in those same places, at that same time. If
## both rise together, the difference between capture years is the city
## rather than the camera.
cov <- ev[ev$gd_covered == 1, ]
r_you <- tapply(cov$Flag_LT,     cov$vintage, mean)
r_gd  <- tapply(cov$gd_matched,  cov$vintage, mean)
cat(sprintf("\n[V7] on newer imagery YOU flag %.2f times more often\n",
            r_you["Q4_2023"]/r_you["Q3_2022"]))
cat(sprintf("[V8] in the same places GEODIRECTORY records %.2f times more often\n",
            r_gd["Q4_2023"]/r_gd["Q3_2022"]))
cat("     Both rising together supports the coverage reading: the newer\n")
cat("     imagery is in places that genuinely have more vacancy.\n")

## ---------------- building level agreement, GeoDirectory = Tier 1 only ----
## Tier 2 is treated as occupied (Flag_LT == 0) in every GeoDirectory table.
agree <- function(mine, theirs, label, tag) {
  tt <- table(mine = mine, geodirectory = theirs)
  ## as.numeric: table counts are integer; n^2 overflows 32-bit int (~50k^2).
  a <- as.numeric(tt["1","1"]); bb <- as.numeric(tt["1","0"])
  c_ <- as.numeric(tt["0","1"]); d <- as.numeric(tt["0","0"])
  prec <- a/(a+bb); rec <- a/(a+c_)
  n <- a+bb+c_+d
  po <- (a+d)/n
  pe <- ((a+bb)*(a+c_) + (c_+d)*(bb+d))/n^2
  cat("\n[", tag, "] ", label, "\n", sep="")
  print(tt)
  cat(sprintf("   precision %.3f   recall %.3f   kappa %.3f\n",
              prec, rec, (po-pe)/(1-pe)))
  invisible(tt)
}
agree(cov$Flag_LT, cov$gd_matched,    "Tier 1 against the matched snapshot", "M4")
agree(cov$Flag_LT, cov$gd_persistent, "Tier 1 against persistent vacancy",   "M6")
agree(cov$Flag_LT, cov$gd_new,        "Tier 1 against newly recorded",       "M7")

## Four categories, one field, strictly comparing Tier 1 against GeoDirectory.
## Tier 2 + GeoDirectory miss  -> Neither
## Tier 2 + GeoDirectory hit   -> Recorded only
b$agreement <- ifelse(b$Evaluable == 0, NA_character_,
  ifelse(b$Flag_LT == 1 & b$gd_matched == 1, "Both",
  ifelse(b$Flag_LT == 1 & b$gd_matched == 0, "Visible only",
  ifelse(b$Flag_LT == 0 & b$gd_matched == 1, "Recorded only", "Neither"))))
cat("\n[M8] agreement categories (Tier 1 vs GeoDirectory)\n"); print(table(b$agreement))
cat("   Visible only  = Tier 1 dereliction the record misses.        RQ2\n")
cat("   Recorded only = recorded vacancy with no Tier 1 flag.        RQ3\n")

bpt2 <- bpt |> st_drop_geometry() |> select(GUID) |>
  left_join(b[, c("GUID","gd_matched","gd_persistent","gd_new","agreement",
                  "g22_any","g23_any","gd_covered")], by = "GUID")
bpt <- cbind(bpt, bpt2[, -1])
st_write(bpt, GPKG, "buildings", delete_dsn = TRUE, quiet = TRUE)
cat("\nwritten buildings layer to\n  ", GPKG, "\n", sep = "")

## ==================== 7. THE ONE SMALL AREA TABLE ====================== ##
bj <- st_join(bpt, sa["SA_KEY"], join = st_intersects, left = FALSE)
bj <- bj[!duplicated(bj$GUID), ]

agg <- bj |> st_drop_geometry() |> group_by(SA_KEY) |>
  summarise(n_eval     = sum(Evaluable),
            n_eval_res = sum(Evaluable == 1 & Is_Residential == 1, na.rm = TRUE),
            n_lt       = sum(Flag_LT),
            n_all      = sum(Flag_All),
            n_all_res  = sum(Flag_All == 1 & Is_Residential == 1, na.rm = TRUE),
            n_new_img  = sum(vintage == "Q4_2023"),
            mn_scr     = mean(Linear_Score[Evaluable == 1]),
            gd_match   = sum(gd_matched[Evaluable == 1]),
            gd_pers    = sum(gd_persistent[Evaluable == 1]),
            gd_cov     = sum(gd_covered[Evaluable == 1]),
            .groups    = "drop")

## GeoDirectory already carries a Small Area identifier, so its own rate needs
## no spatial join at all. The script checks that the identifier lines up with
## the boundary key and falls back to a spatial join if it does not.
gd_rate <- function(gd, prefix) {
  key <- if ("SMALL_AREA_ID" %in% names(gd)) as.character(gd$SMALL_AREA_ID) else NA
  hit <- if (all(is.na(key))) 0 else mean(key %in% sa$SA_KEY)
  cat(sprintf("[A8] %s SMALL_AREA_ID matches the boundary key for %.1f%% of records\n",
              prefix, 100*hit))
  if (hit > 0.9) {
    out <- data.frame(SA_KEY = key, any = gd$gd_any) |>
      group_by(SA_KEY) |> summarise(tot = n(), vac = sum(any), .groups="drop")
  } else {
    cat("      falling back to a spatial join\n")
    j <- st_join(gd["gd_any"], sa["SA_KEY"], join = st_intersects, left = FALSE)
    out <- j |> st_drop_geometry() |> group_by(SA_KEY) |>
      summarise(tot = n(), vac = sum(gd_any), .groups="drop")
  }
  names(out)[-1] <- paste0(prefix, "_", names(out)[-1]); out
}
r22 <- gd_rate(gd22, "g22"); r23 <- gd_rate(gd23, "g23")

saf <- sa |>
  left_join(agg, by = "SA_KEY") |>
  left_join(r22, by = "SA_KEY") |>
  left_join(r23, by = "SA_KEY") |>
  mutate(
    ## GeoDirectory currency: Tier 1 / all evaluable buildings (~1.7%).
    my_rate_lt  = ifelse(n_eval     > 0, 100 * n_lt      / n_eval,     NA_real_),
    ## Census currency: Tier 1+2 / residential evaluable buildings (~5.55%).
    my_rate_res = ifelse(n_eval_res > 0, 100 * n_all_res / n_eval_res, NA_real_),
    cen_rate    = ifelse(cen_tot    > 0, 100 * cen_vac   / cen_tot,    NA_real_),
    geo22       = ifelse(g22_tot    > 0, 100 * g22_vac   / g22_tot,    NA_real_),
    geo23       = ifelse(g23_tot    > 0, 100 * g23_vac   / g23_tot,    NA_real_),
    pct_new     = ifelse(n_eval     > 0, 100 * n_new_img / n_eval,     NA_real_)
  )
## The GeoDirectory rate each Small Area is compared against is the snapshot
## that matches the imagery in that Small Area.
saf$geo_rate <- ifelse(saf$pct_new >= 50, saf$geo23, saf$geo22)

# =============================================================================
# SENSITIVITY ANALYSIS: EMPIRICAL JUSTIFICATION FOR MIN_EVAL CUTOFF
# =============================================================================
# Run this right before STOP 4, ensuring 'saf' currently holds all 849 Small Areas.

library(sf)
library(spdep)
library(dplyr)

# 1. Define the thresholds you want to test
thresholds_to_test <- c(5, 10, 15, 20, 25, 30, 40)

# Total baseline counts (before any filtering)
total_sa <- nrow(saf)
total_bldgs <- sum(saf$n_eval, na.rm = TRUE)

cat("\n--- Running Sensitivity Analysis for MIN_EVAL ---\n")

# Create an empty list to store the results
sensitivity_results <- list()

for (t in thresholds_to_test) {
  
  # 2. Filter a temporary dataset based on the current threshold 't'
  saf_t <- saf |> 
    filter(!is.na(n_eval), n_eval >= t,
           n_eval_res >= t,
           !is.na(my_rate_lt), !is.na(my_rate_res),
           !is.na(geo_rate), !is.na(cen_rate))
  
  # 3. Calculate Data Loss Metrics
  sa_retained <- nrow(saf_t)
  sa_lost_pct <- 100 * (total_sa - sa_retained) / total_sa
  
  bldgs_retained <- sum(saf_t$n_eval, na.rm = TRUE)
  bldgs_lost_pct <- 100 * (total_bldgs - bldgs_retained) / total_bldgs
  
  # 4. Calculate Variance (Standard Deviation of the localized rates)
  sd_res <- sd(saf_t$my_rate_res, na.rm = TRUE)
  sd_lt  <- sd(saf_t$my_rate_lt, na.rm = TRUE)
  
  # 5. Calculate Spatial Stability (Moran's I Z-Score)
  # We MUST rebuild the spatial weights matrix for each new subset of polygons
  nb_t <- poly2nb(saf_t, queen = TRUE)
  
  # Handle islands dynamically (fallback to 6-nearest neighbors if polygons get isolated)
  if (sum(card(nb_t) == 0) > 0) {
    coords <- st_coordinates(st_point_on_surface(st_geometry(saf_t)))
    nb_t <- knn2nb(knearneigh(coords, k = 6), sym = TRUE)
  }
  
  lw_t <- nb2listw(nb_t, style = "W", zero.policy = TRUE)
  
  # Extract Moran's I Z-scores for both currencies
  moran_res <- moran.test(saf_t$my_rate_res, lw_t, zero.policy = TRUE)
  moran_lt  <- moran.test(saf_t$my_rate_lt, lw_t, zero.policy = TRUE)
  
  # 6. Store the iteration's results
  sensitivity_results[[paste0("T", t)]] <- data.frame(
    Threshold = t,
    SAs_Retained = sa_retained,
    SA_Loss_Pct = round(sa_lost_pct, 1),
    Bldg_Loss_Pct = round(bldgs_lost_pct, 2),
    SD_Res_Rate = round(sd_res, 2),
    SD_LT_Rate = round(sd_lt, 2),
    Morans_Z_Res = round(moran_res$statistic, 2),
    Morans_Z_LT = round(moran_lt$statistic, 2)
  )
}

# Combine all results into a single neat table
sensitivity_table <- do.call(rbind, sensitivity_results)
rownames(sensitivity_table) <- NULL

# Print the final justification table to the console
print(sensitivity_table)

# Export for your methodology chapter
write.csv(sensitivity_table, file.path(OUT, "MIN_EVAL_Sensitivity_Analysis.csv"), row.names = FALSE)
cat("\nSaved sensitivity table to: MIN_EVAL_Sensitivity_Analysis.csv\n")

## Sensitivity analysis over


saf$geo_rate <- ifelse(saf$pct_new >= 50, saf$geo23, saf$geo22)

n_before <- nrow(saf)
saf <- saf |> filter(!is.na(n_eval), n_eval >= MIN_EVAL,
                     n_eval_res >= MIN_EVAL,
                     !is.na(my_rate_lt), !is.na(my_rate_res),
                     !is.na(geo_rate), !is.na(cen_rate))
cat("\n[A6] Small Areas retained:", nrow(saf), "of", n_before,
    sprintf("(dropped %d below %d evaluable buildings)", n_before-nrow(saf), MIN_EVAL), "\n")

saf$d_cen <- saf$my_rate_res - saf$cen_rate
saf$d_geo <- saf$my_rate_lt  - saf$geo_rate

## >>> STOP 4. THE SANITY CHECK. Ten minutes here saves the afternoon.
print(summary(st_drop_geometry(saf)[, c("my_rate_lt","my_rate_res","geo_rate",
                                        "cen_rate","pct_new","n_eval","n_eval_res")]))
cat("[B1a] my_rate_lt  mean/median:", round(mean(saf$my_rate_lt),2),  round(median(saf$my_rate_lt),2),  "\n")
cat("[B1b] my_rate_res mean/median:", round(mean(saf$my_rate_res),2), round(median(saf$my_rate_res),2), "\n")
cat("[B2] cen_rate mean/median:", round(mean(saf$cen_rate),2), round(median(saf$cen_rate),2), "\n")
cat("[B3] geo_rate mean/median:", round(mean(saf$geo_rate),2), round(median(saf$geo_rate),2), "\n")
cat("[B4] my_res~cen Pearson", round(cor(saf$my_rate_res,saf$cen_rate),3),
    " Spearman", round(cor(saf$my_rate_res,saf$cen_rate,method="spearman"),3), "\n")
cat("[B5] my_lt~geo  Pearson", round(cor(saf$my_rate_lt,saf$geo_rate),3),
    " Spearman", round(cor(saf$my_rate_lt,saf$geo_rate,method="spearman"),3), "\n")
cat("[V9] my_rate_lt ~ pct_new imagery, Pearson", round(cor(saf$my_rate_lt,saf$pct_new),3), "\n")
cat("     This tells you how much work the coverage control will do in step 7.\n")

st_write(saf, GPKG, "small_areas", delete_layer = TRUE, quiet = TRUE)

## ======================== 8. WEIGHTS AND MORAN ========================= ##
## ======================== 8. WEIGHTS AND MORAN ========================= ##
# 1. Build the initial Queen's Contiguity network
nb <- poly2nb(saf, queen = TRUE)

# 2. Check for islands (polygons with 0 neighbors)
islands <- which(card(nb) == 0)

if (length(islands) > 0) {
  cat("\n[C4] WARNING: Found", length(islands), "island polygon(s).\n")
  cat("     Dropping them to preserve Queen's Contiguity for the rest of the city.\n")
  
  # Drop the islands from the dataset
  saf <- saf[-islands, ]
  
  # Rebuild the Queen's network without the islands
  nb <- poly2nb(saf, queen = TRUE)
}

cat("\n[C5] Final Small Areas in spatial network:", nrow(saf))
cat("\n[C6] Mean neighbours (Queen's Contiguity):", round(mean(card(nb)),2), "\n")

# Build the spatial weights list
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

gm <- function(v, tag) {
  t <- moran.test(v, lw, zero.policy = TRUE)
  cat(sprintf("[%s] Moran's I = %.4f  z = %.3f  p = %.4g\n",
              tag, t$estimate[1], t$statistic, t$p.value)); invisible(t)
}
gm(saf$my_rate_lt,"C1a"); gm(saf$my_rate_res,"C1b"); gm(saf$cen_rate,"C2"); gm(saf$geo_rate,"C3"); gm(saf$pct_new,"C7")

## ========= 9. GLOBAL BASELINE, WITH A CONTROL FOR UNEVEN COVERAGE ====== ##
## Two models per comparison. The first is the relationship you want to
## report. The second adds the share of newer imagery in each Small Area as a
## standard control for uneven coverage. If the relationship survives the
## control, that is a STRONGER result than the uncontrolled one: present it
## that way round rather than as a defence.
baseline <- function(dv, iv, tag) {
  d  <- st_drop_geometry(saf)
  m1 <- lm(as.formula(paste(dv, "~", iv)), data = d)
  m2 <- lm(as.formula(paste(dv, "~", iv, "+ pct_new")), data = d)
  s1 <- summary(m1); s2 <- summary(m2)
  k <- 3; n <- nrow(d)
  aicc <- AIC(m1) + (2*k*(k+1))/(n-k-1)
  bp  <- lmtest::bptest(m1, studentize = TRUE)
  rmi <- lm.morantest(m1, lw, zero.policy = TRUE)
  cat("\n--- global baseline:", dv, "on", iv, "---\n")
  cat(sprintf("[%s1] %s coefficient %.4f  SE %.4f  p %.4g\n",
              tag, iv, coef(m1)[2], s1$coefficients[2,2], s1$coefficients[2,4]))
  cat(sprintf("[%s2] R2 %.4f    [%s3] adjusted R2 %.4f\n", tag, s1$r.squared, tag, s1$adj.r.squared))
  cat(sprintf("[%s4] AIC %.2f  AICc %.2f\n", tag, AIC(m1), aicc))
  cat(sprintf("[%s5] Koenker studentised BP %.3f  df %d  p %.4g\n",
              tag, bp$statistic, bp$parameter, bp$p.value))
  cat(sprintf("[%s6] Moran's I of residuals %.4f  z %.3f  p %.4g\n",
              tag, rmi$estimate[1], rmi$statistic, rmi$p.value))
  cat(sprintf("[%s7] WITH pct_new: %s %.4f (p %.4g), pct_new %.4f (p %.4g), R2 %.4f\n",
              tag, iv, coef(m2)[2], s2$coefficients[2,4],
                   coef(m2)[3], s2$coefficients[3,4], s2$r.squared))
  cat("      A coefficient that holds up with pct_new in the model is\n")
  cat("      evidence the relationship is not an artefact of uneven coverage.\n")
  invisible(m1)
}
m_cen <- baseline("cen_rate", "my_rate_res", "D")  # Census: Tier 1+2 residential
m_geo <- baseline("geo_rate", "my_rate_lt",  "E")  # GeoDirectory: Tier 1 only

## ======================= 10. LOCAL MORAN, BOTH KINDS =================== ##
uni <- function(v, nm, tag) {
  li <- localmoran(v, lw, zero.policy = TRUE)
  p  <- p.adjust(li[, ncol(li)], method = "fdr")
  z  <- as.numeric(scale(v)); lz <- lag.listw(lw, z, zero.policy = TRUE)
  q  <- ifelse(z>0 & lz>0,"High-High", ifelse(z<0 & lz<0,"Low-Low",
        ifelse(z>0 & lz<0,"High-Low","Low-High")))
  cl <- ifelse(p <= 0.05, q, "Not significant")
  saf[[paste0("uni_",nm)]] <<- cl
  cat("\n[",tag,"] univariate LISA:",nm,"\n",sep=""); print(table(cl)); invisible(cl)
}
uni(saf$my_rate_lt,"my_lt","I1a"); uni(saf$my_rate_res,"my_res","I1b"); uni(saf$cen_rate,"cen","I2"); uni(saf$geo_rate,"geo","I3")

bv_lisa <- function(x, y, lw, nsim = NSIM) {
  zx <- as.numeric(scale(x)); zy <- as.numeric(scale(y))
  ly <- lag.listw(lw, zy, zero.policy = TRUE); Ib <- zx * ly
  if ("localmoran_bv" %in% getNamespaceExports("spdep")) {
    r  <- spdep::localmoran_bv(x, y, lw, nsim = nsim, scale = TRUE)
    ## spdep 1.4 returns a matrix (class localmoran), not a list with $p_sim.
    ## r[["p_sim"]] therefore errors; p-values live in the Pr(...) column.
    pv <- NULL
    if (is.list(r) && !is.matrix(r) && !is.data.frame(r)) pv <- r[["p_sim"]]
    if (is.null(pv)) {
      cn <- colnames(r)
      pcol <- if (!is.null(cn)) grep("^Pr|^p", cn, ignore.case = TRUE) else integer(0)
      pv <- as.numeric(r[, if (length(pcol)) pcol[1] else ncol(r)])
    }
    src <- "spdep::localmoran_bv"
  } else {
    cnt <- integer(length(zx))
    for (s in seq_len(nsim))
      cnt <- cnt + (abs(zx * lag.listw(lw, sample(zy), zero.policy=TRUE)) >= abs(Ib))
    pv <- (cnt+1)/(nsim+1); src <- "manual permutation, spdep too old"
  }
  data.frame(Ib=Ib, p=as.numeric(pv), zx=zx, ly=ly, src=src, stringsAsFactors=FALSE)
}
run_bv <- function(xn, yn, field, tag) {
  r <- bv_lisa(saf[[xn]], saf[[yn]], lw)
  q <- ifelse(r$zx>0 & r$ly>0,"High-High", ifelse(r$zx<0 & r$ly<0,"Low-Low",
       ifelse(r$zx>0 & r$ly<0,"High-Low","Low-High")))
  cl <- ifelse(p.adjust(r$p, method="fdr") <= 0.05, q, "Not significant")
  saf[[field]] <<- cl
  cat("\n[",tag,"] bivariate LISA   x = ",xn,"   lagged y = ",yn,
      "   via ",r$src[1],"\n",sep="")
  print(table(factor(cl, levels=c("High-High","Low-Low","High-Low",
                                  "Low-High","Not significant"))))
  invisible(cl)
}
run_bv("my_rate_res","cen_rate","bv_my_cen","H1")   # Census: Tier 1+2 residential
run_bv("my_rate_lt","geo_rate","bv_my_geo","H2")    # GeoDirectory: Tier 1 only
run_bv("cen_rate","my_rate_res","bv_cen_my","H3")   # reverse, Census
run_bv("geo_rate","my_rate_lt","bv_geo_my","H4")    # reverse, GeoDirectory

## High-Low = visible dereliction the record misses.            RQ2
## Low-High = recorded vacancy with no visible dereliction.     RQ3

## ========================== 11. GETIS-ORD Gi* ========================== ##
lwG <- nb2listw(include.self(nb), style = "W", zero.policy = TRUE)
g   <- as.numeric(localG(saf$my_rate_res, lwG, zero.policy = TRUE))
pg  <- p.adjust(2*pnorm(-abs(g)), method = "fdr")
saf$gi_z  <- g
saf$gi_cl <- ifelse(pg > 0.05, "Not significant",
             ifelse(g > 0, ifelse(pg <= 0.01,"Hot 99%","Hot 95%"),
                           ifelse(pg <= 0.01,"Cold 99%","Cold 95%")))
cat("\n[I4] Getis-Ord Gi* on my_rate_res (Census currency)\n"); print(table(saf$gi_cl))

## ============================= 12. EXPORT ============================== ##
st_write(saf, GPKG, "sa_results", delete_layer = TRUE, quiet = TRUE)
write.csv(st_drop_geometry(saf), file.path(OUT,"SA_results_T1Geo.csv"), row.names = FALSE)
write.csv(b[, c("GUID","Linear_Score","Vacancy_Tier_NoOut","Is_Residential",
                "vintage","cap_date",
                "g22_any","g23_any","gd_matched","gd_persistent","gd_new","agreement")],
          file.path(OUT,"Building_results_T1Geo.csv"), row.names = FALSE)
cat("\nwritten\n  ", GPKG, "\n  ",
    file.path(OUT,"SA_results_T1Geo.csv"), "\n  ",
    file.path(OUT,"Building_results_T1Geo.csv"), "\n", sep = "")
cat("Add the GeoPackage to ArcGIS Pro for every map. Nothing here needs a shapefile.\n")

###############################################################################
##  13. STEP 9: GEOGRAPHICALLY WEIGHTED REGRESSION
###############################################################################
## Either run this, or do the same thing in ArcGIS Pro on the exported
## GeoPackage: Spatial Statistics, Modeling Spatial Relationships, GWR, with
## Neighborhood Type set to Number of neighbors, Golden search, minimum 30,
## bisquare. Pro is usually quicker if you know the dialog, and you will be
## in Pro for the maps anyway.
# install.packages("GWmodel"); library(GWmodel)
# sp <- as(saf, "Spatial")
#
# bw <- bw.gwr(cen_rate ~ my_rate_res, data = sp, approach = "AICc",
#              kernel = "bisquare", adaptive = TRUE)
# gw <- gwr.basic(cen_rate ~ my_rate_res, data = sp, bw = bw,
#                 kernel = "bisquare", adaptive = TRUE)
# print(gw)                          # [F1] bandwidth, [F2] AICc and R squared
# summary(gw$SDF$my_rate_res)        # [F3] local coefficient range
# summary(gw$SDF$Local_R2)           # [F4] local fit range
# sum(gw$SDF$my_rate_res < 0)        # [F5] areas where it runs the other way
# st_write(st_as_sf(gw$SDF), GPKG, "gwr_census", delete_layer = TRUE)
#
# bwg <- bw.gwr(geo_rate ~ my_rate_lt, data = sp, approach = "AICc",
#               kernel = "bisquare", adaptive = TRUE)
# gwg <- gwr.basic(geo_rate ~ my_rate_lt, data = sp, bw = bwg,
#                  kernel = "bisquare", adaptive = TRUE)
# print(gwg)                         # [G1] to [G3]
# st_write(st_as_sf(gwg$SDF), GPKG, "gwr_geodirectory", delete_layer = TRUE)
