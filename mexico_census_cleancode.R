# ============================================================
# Censo 2020 (INEGI) — Telecom & Infrastructure Covariates
# Standalone script: collapses municipal-level VPH_* variables
# into a clean, joinable covariate table.
# No dependency on any other script/file — self-contained.
# ============================================================

library(dplyr)
library(stringr)
library(stringi)

# ---- 1. Load raw INEGI census file ----
# Adjust filename and separator as needed (tab vs comma delimited)
censo_raw <- read.csv("municimex_2020_2021_03_06.csv", stringsAsFactors = FALSE,
                      encoding = "UTF-8", sep = ",")
nrow(censo_raw)
names(censo_raw)

# ---- 2. Build CVEGEO from entidad + municipio codes ----
censo_raw <- censo_raw %>%
  mutate(
    CVEGEO = paste0(
      formatC(ENTIDAD_2_DIGS, width = 2, format = "d", flag = "0"),
      formatC(MUN_3_DIGS, width = 3, format = "d", flag = "0")
    ),
    NOMGEO = stri_trans_general(toupper(trimws(NOM_MUN)), "Latin-ASCII")
  )

# ---- 3. Helper function: strip commas, convert to numeric ----
# INEGI formats large numbers as text with commas (e.g. "948,990")
clean_num <- function(x) {
  as.numeric(gsub(",", "", x))
}

# ---- 4. Define the columns to clean ----
vph_vars <- c(
  "VPH_SINTIC",  # without any ICT device at all
  "VPH_C_ELEC",  # with electricity
  "VPH_AGUADV",  # with piped water inside the dwelling
  "VPH_PISODT",  # finished (non-dirt) floor
  "VPH_PISOTI",  # dirt floor
  "VPH_1CUART"   # one-room dwellings
)

# ---- 5. Clean numeric columns (strip commas, convert to numeric) ----
censo_clean <- censo_raw %>%
  mutate(across(all_of(vph_vars), clean_num)) %>%
  mutate(
    VIVPAR_HAB = clean_num(VIVPAR_HAB),   # total inhabited private dwellings
    POBTOT     = clean_num(POBTOT)
  )

# ---- 6. Convert counts to percentages of total households ----
censo_pct <- censo_clean %>%
  mutate(across(all_of(vph_vars), ~ . / VIVPAR_HAB, .names = "pct_{.col}"))

# ---- 7. Build final table: CVEGEO, NOMGEO, + percentage columns ----
telecom_utility_covariates <- censo_pct %>%
  select(CVEGEO, NOMGEO, VIVPAR_HAB, POBTOT, starts_with("pct_VPH_"))

nrow(telecom_utility_covariates)
head(telecom_utility_covariates)

# ---- 8. Audit ----
sum(duplicated(telecom_utility_covariates$CVEGEO))   # should be 0
sum(is.na(telecom_utility_covariates$CVEGEO))        # should be 0

# Sanity check: percentages should fall between 0 and 1 (allow tiny float overshoot)
telecom_utility_covariates %>%
  select(starts_with("pct_VPH_")) %>%
  summarise(across(everything(), ~sum(. > 1.01 | . < 0, na.rm = TRUE)))

# ---- 9. Save ----
write.csv(telecom_utility_covariates, "mexico_census_infrastructure_data.csv", row.names = FALSE)
