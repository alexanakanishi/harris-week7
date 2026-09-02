library(dplyr)
library(stringr)
library(stringi)

# ---- Load raw school infrastructure data ----
schools_raw <- read.csv("infraestructura_fisica_educativa.csv", stringsAsFactors = FALSE, encoding = "UTF-8")

nrow(schools_raw)
names(schools_raw)

# ---- Check exact category labels first, for every field being recoded ----
unique(schools_raw$bebederos)
unique(schools_raw$cancha_deportiva)
unique(schools_raw$plan_proteccion_civil)

# ---- Standardize text ID fields ----
schools_raw <- schools_raw %>%
  mutate(
    entidad_clean = stri_trans_general(toupper(trimws(entidad_federativa)), "Latin-ASCII"),
    NOMGEO        = stri_trans_general(toupper(trimws(municipio)), "Latin-ASCII")
  )

# ---- Recode each infrastructure field to a binary flag ----
schools_raw <- schools_raw %>%
  mutate(
    has_bebederos   = case_when(str_detect(toupper(bebederos), "CON BEBEDERO") ~ 1,
                                str_detect(toupper(bebederos), "SIN BEBEDERO") ~ 0, TRUE ~ NA_real_),
    has_cancha      = case_when(str_detect(toupper(cancha_deportiva), "CON CANCHA") ~ 1,
                                str_detect(toupper(cancha_deportiva), "SIN CANCHA") ~ 0, TRUE ~ NA_real_),
    has_proteccion_civil = case_when(str_detect(toupper(plan_proteccion_civil), "CON PLAN") ~ 1,
                                     str_detect(toupper(plan_proteccion_civil), "SIN PLAN") ~ 0, TRUE ~ NA_real_)
  )

# ---- Check how many rows failed to classify per field (should be low/0) ----
schools_raw %>%
  summarise(across(starts_with("has_"), ~sum(is.na(.))))

# ---- Filter to a consistent year, if the file spans multiple years ----
table(schools_raw$anio)
schools_2020 <- schools_raw %>% filter(anio == 2020)

# ---- Collapse to municipality level: one row per NOMGEO ----
school_covariates <- schools_2020 %>%
  group_by(entidad_clean, NOMGEO) %>%
  summarise(
    n_schools                = n(),
    pct_con_bebederos        = mean(has_bebederos, na.rm = TRUE),
    pct_con_proteccion_civil = mean(has_proteccion_civil, na.rm = TRUE),
    pct_con_cancha           = mean(has_cancha, na.rm = TRUE),
    avg_anio_creacion        = mean(anio_creacion, na.rm = TRUE),
    .groups = "drop"
  )

nrow(school_covariates)
head(school_covariates)

# ---- Save as its own CSV ----
write.csv(school_covariates, "school_infrastructure_covariates.csv", row.names = FALSE)

