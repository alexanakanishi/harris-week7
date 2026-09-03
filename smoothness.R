## ============================================================
##  Smoothness Battery — Covariate Balance at Three Cutoffs
##  Running variable: rank_composite
##  Cutoffs: 38, 52, 61
## ============================================================
 
library(rdrobust)
library(dplyr)
 
## ------------------------------------------------------------
## 0. SETUP
## ------------------------------------------------------------
 
DATA_PATH <- "merged_covariates_with_rank.csv"
#RV        <- "rank_composite"
RV <- "composite"
RANK_CUTS   <- c(38, 52, 61)
 


## ------------------------------------------------------------
## 1. LOAD DATA
## ------------------------------------------------------------
 
df <- read.csv(DATA_PATH)
cat("Observations:", nrow(df), "\n")
cat("rank_composite range:", range(df[[RV]]), "\n\n")

ncol(df) # 259 total
names(df) # 7 are not covariates, so 252 total covariates 
nrow(df)


# Fixed bandwidth grid (in z-score composite).
H_GRID <- c(0.5, 1.0, 1.5)

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



for (c in CUTOFFS) {
  cat("Cutoff", round(c, 3), "\n")
  for (h in H_GRID) {
    cat("  h =", h, ":", sum(abs(df$composite - c) <= h, na.rm=TRUE), "obs\n")
  }
  cat("\n")
}

## ------------------------------------------------------------
## 2. DEFINE COVARIATES
## ------------------------------------------------------------
 
all_covs <- c(
  # Traffic / vehicle types
  "tdpa_avg", "no_tdpa_avg",
  "punto_generador_count", "no_punto_generador_count",
  "m_avg", "no_m_avg",
  "a_avg", "no_a_avg",
  "b_avg", "no_b_avg",
  "c2_avg", "no_c2_avg",
  "c3_avg", "no_c3_avg",
  "t3s2_avg", "no_t3s2_avg",
  "t3s3_avg", "no_t3s3_avg",
  "t3s2r4_avg", "no_t3s2r4_avg",
  "otros_avg", "no_otros_avg",
  "aa_avg", "no_aa_avg",
  "bb_avg", "no_bb_avg",
  "cc_avg", "no_cc_avg",
  # Socioeconomic indices
  "IM_2020", "no_IM_2020",
  "IE", "no_IE",
  "TCVCU", "no_TCVCU",
  "ICE", "no_ICE",
  "VIVPAR_HAB", "no_VIVPAR_HAB",
  "POBTOT", "no_POBTOT",
  # Housing quality
  "pct_VPH_SINTIC", "no_pct_VPH_SINTIC",
  "pct_VPH_C_ELEC", "no_pct_VPH_C_ELEC",
  "pct_VPH_AGUADV", "no_pct_VPH_AGUADV",
  "pct_VPH_PISODT", "no_pct_VPH_PISODT",
  "pct_VPH_PISOTI", "no_pct_VPH_PISOTI",
  "pct_VPH_1CUART", "no_pct_VPH_1CUART",
  # Schools & amenities
  "n_schools_avg", "no_n_schools_avg",
  "pct_con_bebederos", "no_pct_con_bebederos",
  "pct_con_proteccion_civil", "no_pct_con_proteccion_civil",
  "pct_con_cancha", "no_pct_con_cancha",
  "avg_anio_creacion", "no_avg_anio_creacion",
  # Roads
  "km_carretera_fed", "km_carretera_est",
  "km_cuota", "km_camino",
  "km_vereda", "km_vialidad_urbana", "km_total",
  # Federal presence & aviation
  "n_federal_offices", "no_n_federal_offices",
  "n_aerodromes", "no_n_aerodromes",
  "n_heliports", "no_n_heliports",
  "has_aerodrome", "has_heliport",
  # Water & drainage services
  "ag_servi", "no_ag_servi",
  "pobl_prc", "no_pobl_prc",
  "ag_dias", "no_ag_dias",
  "ag_horas", "no_ag_horas",
  "serv_dre", "no_serv_dre",
  "dren_cab", "no_dren_cab",
  "dren_plu", "no_dren_plu",
  "tot_conx", "no_tot_conx",
  "prest_nu", "no_prest_nu",
  # Strategic infrastructure sites
  "n_sites_total",
  "n_aduana", "n_puerto", "n_puerto_fronterizo",
  "n_terminal_multimodal", "n_estacion_ferrocarril",
  "n_zona_industrial", "n_almacen", "n_zona_libre",
  "n_aeropuerto", "n_aeropuerto_internacional", "n_pista_aerea",
  "n_muelle_embarcadero", "n_terminal_transbordador",
  "n_central_camionera", "n_estacion_pesaje", "n_caseta_inspeccion",
  "n_estacion_combustible", "n_estacion_policia",
  "n_oficina_gobierno", "n_palacio_gobierno", "n_palacio_justicia",
  "n_estacion_bomberos",
  # Health & education
  "n_hospital", "n_emergencia_medica", "n_primeros_auxilios",
  "n_universidad", "n_centro_investigacion",
  # Tourism & culture
  "n_atractivo_turistico", "n_hotel_motel", "n_museo",
  "n_monumento_historico", "n_centro_cultural", "n_centro_convenciones",
  "n_lugar_esparcimiento", "n_mirador", "n_parque_diversiones",
  "n_parque_animal", "n_campo_golf", "n_restaurante", "n_balneario",
  "n_ex_hacienda", "n_presa_turistica", "n_area_natural_protegida",
  "n_zona_arqueologica", "n_lago_laguna", "n_cascada",
  "n_pueblo_magico", "n_cenote", "n_gruta", "n_ruinas",
  "n_teleferico_funicular",
  "n_museo_historia", "n_museo_arte", "n_museo_ciencia", "n_museo_ninos",
  "n_turismo_historico", "n_turismo_natural",
  "n_turismo_cultural", "n_turismo_recreativo", "n_turismo_total",
  # Binary infrastructure flags
  "has_aduana", "has_puerto", "has_puerto_fronterizo",
  "has_aeropuerto", "has_aeropuerto_internacional",
  "has_estacion_ferrocarril", "has_terminal_multimodal",
  "has_zona_industrial", "has_atractivo_turistico",
  "has_hotel", "has_tourism",
  # Distance to energy infrastructure (centroid)
  "dist_gasproc_cent", "nearest_gasproc_cent",
  "n_gasproc_25km_cent", "n_gasproc_50km_cent", "n_gasproc_100km_cent",
  "dist_maritime_cent", "nearest_maritime_cent",
  "n_maritime_25km_cent", "n_maritime_50km_cent", "n_maritime_100km_cent",
  "dist_petrochem_cent", "nearest_petrochem_cent",
  "n_petrochem_25km_cent", "n_petrochem_50km_cent", "n_petrochem_100km_cent",
  "dist_refinery_cent", "nearest_refinery_cent",
  "n_refinery_25km_cent", "n_refinery_50km_cent", "n_refinery_100km_cent",
  "dist_storage_cent", "nearest_storage_cent",
  "n_storage_25km_cent", "n_storage_50km_cent", "n_storage_100km_cent",
  "dist_any_cent", "n_any_25km_cent", "n_any_50km_cent", "n_any_100km_cent",
  # Distance to energy infrastructure (edge)
  "dist_gasproc_edge", "nearest_gasproc_edge",
  "n_gasproc_25km_edge", "n_gasproc_50km_edge", "n_gasproc_100km_edge",
  "dist_maritime_edge", "nearest_maritime_edge",
  "n_maritime_25km_edge", "n_maritime_50km_edge", "n_maritime_100km_edge",
  "dist_petrochem_edge", "nearest_petrochem_edge",
  "n_petrochem_25km_edge", "n_petrochem_50km_edge", "n_petrochem_100km_edge",
  "dist_refinery_edge", "nearest_refinery_edge",
  "n_refinery_25km_edge", "n_refinery_50km_edge", "n_refinery_100km_edge",
  "dist_storage_edge", "nearest_storage_edge",
  "n_storage_25km_edge", "n_storage_50km_edge", "n_storage_100km_edge",
  "dist_any_edge", "n_any_25km_edge", "n_any_50km_edge", "n_any_100km_edge",
  # Hosting
  "n_hosted", "hosts_any",
  # Railroad infrastructure
  "km_ferrocarril_total", "km_ferrocarril_operacion",
  "km_ferrocarril_sin_uso", "km_ferrocarril_carga",
  "km_ferrocarril_pasajeros", "km_ferrocarril_carga_pasajeros",
  "km_ferrocarril_inactivo",
  "km_ferromex", "km_kcs_mexico", "km_ferrosur", "km_tren_maya",
  "km_istmo_tehuantepec", "km_coahuila_durango", "km_ferrorvalle",
  "km_tijuana_tecate", "km_jalisco", "km_mexico_toluca",
  "km_ferrocarril_suburbano", "km_puebla", "km_sin_concesion",
  "has_ferrocarril", "has_ferrocarril_operacion",
  "has_ferrocarril_carga", "has_ferrocarril_pasajeros",
  "has_ferrocarril_carga_pasajeros"
)

 
cat("Total covariates to test:", length(all_covs), "\n")


## ------------------------------------------------------------
## 3. CORE FUNCTION — run RD for one covariate at one cutoff/h
## ------------------------------------------------------------
 
run_rd <- function(data, cov, rv, cutoff, h) {
  sub <- data[!is.na(data[[cov]]) & !is.na(data[[rv]]), ]
 
  tryCatch({
    rdd <- rdrobust(
      y          = sub[[cov]],
      x          = sub[[rv]],
      c          = cutoff,
      h          = h,
      kernel     = "triangular",
      #masspoints = "adjust"   # handles discrete/tied running variable (ranks are discrete)
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
      y          = sub_ref[[bw_ref_cov]],
      x          = sub_ref[[rv]],
      c          = cutoff,
      kernel     = "triangular",
      #masspoints = "adjust"
    )$bws[1, 1], 2)
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
 
write.csv(results_38$table, "rd/smoothness_cutoff38_uses_composite.csv", row.names = FALSE)
write.csv(results_52$table, "rd/smoothness_cutoff52_uses_composite.csv", row.names = FALSE)
write.csv(results_61$table, "rd/smoothness_cutoff61_uses_composite.csv", row.names = FALSE)
 
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
