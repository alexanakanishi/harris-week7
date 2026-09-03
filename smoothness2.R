# ============================================================
##  Smoothness Battery — Covariate Balance at Three Cutoffs
##  Running variable: composite (z-score underlying rank_composite)
##  Cutoffs: z-score values at rank boundaries 38, 52, 61
## ============================================================
##  install.packages(c("rdrobust", "dplyr"))
## ============================================================
 
library(rdrobust)
library(dplyr)
 
## ------------------------------------------------------------
## 0. SETUP
## ------------------------------------------------------------
 
DATA_PATH  <- "merged_covariates_with_rank.csv"
RV         <- "composite"          # continuous z-score running variable
RANK_CUTS  <- c(38, 52, 61)        # rank boundaries of interest
 
# Fixed bandwidth grid (in z-score units).
# h=2.0+ hits the mass-point floor at composite ≈ -0.21 (especially for
# ranks 52/61 whose left window reaches ~-0.5 at h=2.0). Cap at 1.5.
H_GRID <- c(0.5, 1.0, 1.5)
 
## ------------------------------------------------------------
## 1. LOAD DATA
## ------------------------------------------------------------
 
df <- read.csv(DATA_PATH)
cat("Observations:", nrow(df), "\n")
cat("composite range:", round(range(df[[RV]], na.rm = TRUE), 4), "\n\n")
 
## Derive cutoffs in z-score units from rank boundaries.
## For each rank boundary k, the cutoff is the composite value of the
## municipality AT rank k (the upper edge of the "below" group).
## If rank_composite is not in the data, adjust the sort column below.
CUTOFFS <- sapply(RANK_CUTS, function(k) {
  df_sorted <- df[order(df$rank_composite), ]
  round(df_sorted$composite[k], 6)
})
 
cat("Cutoffs in z-score units:\n")
for (i in seq_along(RANK_CUTS)) {
  cat("  rank", RANK_CUTS[i], "->", CUTOFFS[i], "\n")
}
cat("\n")
cat("NOTE: Check H_GRID scale against composite range above.\n")
cat("      Adjust H_GRID values in section 0 if needed.\n\n")
 
## ------------------------------------------------------------
## 2. DEFINE COVARIATES
##
##    Add or remove names freely.
##    Substantive covariates — pre-treatment municipal characteristics
##    no_ flags — zero-insertion indicators; a discontinuity here means
##                differential missingness at the threshold (validity threat)
##
##    Any name not found in the dataframe is silently dropped.
## ------------------------------------------------------------
 
all_covs <- c(
  # Socioeconomic
  "IM_2020", "IE", "TCVCU", "ICE", "POBTOT", "VIVPAR_HAB",
  # Housing quality
  "pct_VPH_SINTIC", "pct_VPH_C_ELEC", "pct_VPH_AGUADV",
  "pct_VPH_PISODT", "pct_VPH_PISOTI", "pct_VPH_1CUART",
  # Roads
  "km_carretera_fed", "km_carretera_est", "km_cuota",
  "km_camino", "km_vialidad_urbana", "km_total",
  # Transport & infrastructure
  "tdpa_avg", "punto_generador_count", "n_federal_offices",
  # Services
  "ag_servi", "serv_dre", "ag_dias", "ag_horas",
  # Schools & amenities
  "n_schools_avg", "pct_con_bebederos", "pct_con_cancha",
  # Other pre-treatment
  "avg_anio_creacion", "n_hospital", "n_turismo_total", "n_sites_total",
  # Zero-insertion flags (no_ prefix)
  "no_tdpa_avg", "no_punto_generador_count",
  "no_m_avg", "no_a_avg", "no_b_avg", "no_c2_avg", "no_c3_avg",
  "no_t3s2_avg", "no_t3s3_avg", "no_t3s2r4_avg", "no_otros_avg",
  "no_aa_avg", "no_bb_avg", "no_cc_avg",
  "no_IM_2020", "no_IE", "no_TCVCU", "no_ICE",
  "no_VIVPAR_HAB", "no_POBTOT",
  "no_pct_VPH_SINTIC", "no_pct_VPH_C_ELEC", "no_pct_VPH_AGUADV",
  "no_pct_VPH_PISODT", "no_pct_VPH_PISOTI", "no_pct_VPH_1CUART",
  "no_n_schools_avg", "no_pct_con_bebederos",
  "no_pct_con_proteccion_civil", "no_pct_con_cancha",
  "no_avg_anio_creacion", "no_n_federal_offices",
  "no_ag_servi", "no_pobl_prc", "no_ag_dias", "no_ag_horas",
  "no_serv_dre", "no_dren_cab", "no_dren_plu",
  "no_tot_conx", "no_prest_nu", "no_n_aerodromes",
  "no_n_heliports"
)
 
# Drop any names not present in the dataframe
all_covs <- intersect(all_covs, names(df))
 
cat("Total covariates to test:", length(all_covs), "\n\n")
 
## ------------------------------------------------------------
## 3. CORE FUNCTION — run RD for one covariate at one cutoff/h
## ------------------------------------------------------------
 
run_rd <- function(data, cov, rv, cutoff, h) {
  sub <- data[!is.na(data[[cov]]) & !is.na(data[[rv]]), ]
 
  tryCatch({
    rdd <- rdrobust(
      y      = sub[[cov]],
      x      = sub[[rv]],
      c      = cutoff,
      h      = h,
      kernel = "triangular"
      # masspoints not needed — composite is continuous
    )
    list(
      coef  = round(rdd$coef[1], 4),
      se    = round(rdd$se[3], 4),   # robust SE
      pval  = round(rdd$pv[3], 4),   # robust p-value
      n_l   = rdd$N_h[1],
      n_r   = rdd$N_h[2]
    )
  }, error = function(e) {
    list(coef = NA, se = NA, pval = NA, n_l = NA, n_r = NA)
  })
}
 
## ------------------------------------------------------------
## 4. BATTERY FUNCTION — all covariates × all h's for one cutoff
##
##    Output is a wide dataframe:
##      rows    = covariates
##      columns = one set per bandwidth: coef_h10, se_h10, pval_h10, ...
##                plus MSE-optimal: coef_opt, se_opt, pval_opt
## ------------------------------------------------------------
 
run_battery <- function(data, covs, rv, cutoff, h_grid) {
 
  cat("--- Cutoff:", cutoff, "---\n")
 
  ## 4a. Get MSE-optimal bandwidth (estimated from first substantive covariate
  ##     with enough variance; used as reference — not per-covariate optimal)
  bw_ref_cov <- covs[1]
  sub_ref    <- data[!is.na(data[[bw_ref_cov]]) & !is.na(data[[rv]]), ]
 
  h_opt <- tryCatch({
    round(rdbwselect(
      y      = sub_ref[[bw_ref_cov]],
      x      = sub_ref[[rv]],
      c      = cutoff,
      kernel = "triangular"
      # masspoints not needed — composite is continuous
    )$bws[1, 1], 4)
  }, error = function(e) NA)
 
  cat("  MSE-optimal h:", h_opt, "\n")
 
  # Full set of h values to run: fixed grid + optimal (deduplicated)
  h_all <- sort(unique(c(h_grid, h_opt)))
  h_labels <- ifelse(h_all == h_opt,
                     paste0("h", h_all, "_OPT"),
                     paste0("h", h_all))
 
  ## 4b. Loop over covariates
  results <- lapply(covs, function(cov) {
 
    row <- data.frame(covariate = cov, stringsAsFactors = FALSE)
 
    for (i in seq_along(h_all)) {
      h   <- h_all[i]
      lbl <- h_labels[i]
      res <- run_rd(data, cov, rv, cutoff, h)
 
      row[[paste0("coef_",  lbl)]] <- res$coef
      row[[paste0("se_",    lbl)]] <- res$se
      row[[paste0("pval_",  lbl)]] <- res$pval
      row[[paste0("n_l_",   lbl)]] <- res$n_l
      row[[paste0("n_r_",   lbl)]] <- res$n_r
    }
    row
  })
 
  tbl <- do.call(rbind, results)
 
  ## 4c. Summary: how many covariates significant at each h?
  cat("  Significant (p<0.05) per bandwidth:\n")
  pval_cols <- grep("^pval_", names(tbl), value = TRUE)
  for (pc in pval_cols) {
    n_sig <- sum(tbl[[pc]] < 0.05, na.rm = TRUE)
    cat("   ", pc, ":", n_sig, "/", sum(!is.na(tbl[[pc]])), "\n")
  }
  cat("\n")
 
  list(table = tbl, h_opt = h_opt)
}
 
## ------------------------------------------------------------
## 5. RUN BATTERY FOR EACH CUTOFF — produces 3 separate tables
## ------------------------------------------------------------
 
results_38 <- run_battery(df, all_covs, RV, cutoff = CUTOFFS[1], H_GRID)
results_52 <- run_battery(df, all_covs, RV, cutoff = CUTOFFS[2], H_GRID)
results_61 <- run_battery(df, all_covs, RV, cutoff = CUTOFFS[3], H_GRID)
 
## ------------------------------------------------------------
## 6. SAVE TABLES
## ------------------------------------------------------------
 
write.csv(results_38$table, "rd/smoothness_cutoff38-new2.csv", row.names = FALSE)
write.csv(results_52$table, "rd/smoothness_cutoff52-new2.csv", row.names = FALSE)
write.csv(results_61$table, "rd/smoothness_cutoff61.cs-new2v", row.names = FALSE)
 
cat("Saved:\n")
cat("  smoothness_cutoff38.csv\n")
cat("  smoothness_cutoff52.csv\n")
cat("  smoothness_cutoff61.csv\n\n")
 
## ------------------------------------------------------------
## 7. QUICK DIAGNOSTIC PRINT — p-values only, all three cutoffs
##    Useful for spotting problematic covariates at a glance
## ------------------------------------------------------------
 
print_pval_summary <- function(tbl, cutoff) {
  pval_cols <- grep("^pval_", names(tbl), value = TRUE)
  cat("=== Cutoff", cutoff, "— covariates with any p < 0.05 ===\n")
  flagged <- tbl[apply(tbl[, pval_cols, drop = FALSE], 1,
                       function(r) any(r < 0.05, na.rm = TRUE)), ]
  if (nrow(flagged) == 0) {
    cat("  None — all covariates smooth at this cutoff.\n")
  } else {
    print(flagged[, c("covariate", pval_cols)])
  }
  cat("\n")
}
 
print_pval_summary(results_38$table, 38)
print_pval_summary(results_52$table, 52)
print_pval_summary(results_61$table, 61)