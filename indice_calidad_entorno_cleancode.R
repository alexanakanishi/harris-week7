# ============================================================
# Índice de Calidad del Entorno (ICE) 2020 — CONAPO
# Standalone script: cleans municipal-level ICE variables
# into a clean, joinable covariate table.
# No dependency on any other script/file — self-contained.
# ============================================================

library(dplyr)
library(stringr)
library(stringi)

# ---- 1. Load raw ICE file ----
# Adjust filename and separator as needed (tab vs comma delimited)
ice_raw <- read.csv("indice_calidad_entorno.csv", stringsAsFactors = FALSE,
                    encoding = "UTF-8", sep = ",")

nrow(ice_raw)
names(ice_raw)

# ---- 2. Build CVEGEO from entidad + municipio codes ----
ice_raw <- ice_raw %>%
  mutate(
    CVEGEO = paste0(
      formatC(ENT, width = 2, format = "d", flag = "0"),
      formatC(MUN, width = 3, format = "d", flag = "0")
    ),
    NOMGEO = stri_trans_general(toupper(trimws(NOM_MUN)), "Latin-ASCII")
  )

# ---- 3. Select and clean relevant variables ----
ice_clean <- ice_raw %>%
  mutate(
    IM_2020 = as.numeric(IM_2020),
    IE      = as.numeric(IE),
    TCVCU   = as.numeric(TCVCU),
    ICE     = as.numeric(ICE),
    G_ICE   = factor(G_ICE,
                     levels = c("No hay", "Ligera", "Moderada", "Grave", "Completa"),
                     ordered = TRUE)
  )

# ---- 4. Build final table: CVEGEO, NOMGEO, + covariates ----
ice_covariates <- ice_clean %>%
  select(CVEGEO, NOMGEO, ENT, NOM_ENT, IM_2020, IE, TCVCU, ICE, G_ICE)

nrow(ice_covariates)
head(ice_covariates)

# ---- 5. Audit ----
sum(duplicated(ice_covariates$CVEGEO))   # should be 0
sum(is.na(ice_covariates$CVEGEO))        # should be 0
sum(is.na(ice_covariates$ICE))           # check how many ICE scores are missing

# Sanity check: G_ICE categories present
table(ice_covariates$G_ICE, useNA = "ifany")

# ---- 6. Save ----
write.csv(ice_covariates, "ice_covariates.csv", row.names = FALSE)
