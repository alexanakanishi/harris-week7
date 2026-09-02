# ============================================================
# INDAABIN — Federal & Parastatal Real Estate Inventory
# Standalone script: cleans the property inventory, excludes
# religious properties (historical nationalization artifacts,
# not genuine "state presence"), and counts remaining federal
# properties ("offices") per municipality.
# No dependency on any other script/file — self-contained.
# ============================================================

library(dplyr)
library(stringr)
library(stringi)

# ---- 1. Load raw INDAABIN inventory file ----
# Adjust filename to match your actual downloaded file
indaabin_raw <- read.csv("Federal_and_Parastatal_Assets.csv", stringsAsFactors = FALSE,
                         encoding = "UTF-8", sep = ",")

nrow(indaabin_raw)
names(indaabin_raw)

# ---- IMPORTANT: check the actual column names before proceeding ----
# Confirmed actual columns via names(indaabin_raw):
#   X_id, rfi, seccion, institucion, nombre, estado, municipio_alcaldia, ubicacion
indaabin_raw <- indaabin_raw %>%
  rename(
    registro    = rfi,                  # Federal Real Estate Registry
    inmueble    = nombre,                # Property name
    municipio   = municipio_alcaldia     # Municipality or Mayor's Office
  )
# seccion, institucion, estado, and ubicacion already have the right names

# ---- 2. Standardize municipality/state names ----
indaabin_raw <- indaabin_raw %>%
  mutate(
    NOMGEO    = stri_trans_general(toupper(trimws(municipio)), "Latin-ASCII"),
    estado_clean = stri_trans_general(toupper(trimws(estado)), "Latin-ASCII")
  )

# ---- 3. Flag and exclude religious properties ----
# Church buildings were historically nationalized after the Mexican
# Revolution and remain technically federal property even though used
# by religious congregations — these reflect that legal history, not
# genuine government office presence, so they're excluded from the count.
religious_pattern <- "TEMPLO|TEMPLE|CAPILLA|CHAPEL|IGLESIA|CHURCH|PARROQUIA|SANTUARIO|CONVENTO|ERMITA"

indaabin_raw <- indaabin_raw %>%
  mutate(
    is_religious = str_detect(toupper(stri_trans_general(inmueble, "Latin-ASCII")), religious_pattern)
  )

# ---- Audit: how many properties are being excluded as religious? ----
sum(indaabin_raw$is_religious)
mean(indaabin_raw$is_religious)

# Sanity check: eyeball a sample of what's being excluded, to confirm the pattern is catching the right things
indaabin_raw %>% filter(is_religious) %>% select(inmueble) %>% distinct() %>% head(20)

# Sanity check: eyeball a sample of what's being KEPT, to confirm genuine offices aren't being excluded
indaabin_raw %>% filter(!is_religious) %>% select(inmueble) %>% distinct() %>% head(20)

# ---- 4. Filter to non-religious properties only ----
indaabin_offices <- indaabin_raw %>%
  filter(!is_religious)

nrow(indaabin_offices)   # remaining "genuine" federal properties

# ---- 5. Count offices per municipality ----
office_counts <- indaabin_offices %>%
  group_by(estado_clean, NOMGEO) %>%
  summarise(
    n_federal_offices = n(),
    .groups = "drop"
  )

nrow(office_counts)
head(office_counts)

# Sanity extremes
office_counts %>% arrange(desc(n_federal_offices)) %>% head(10)

# ---- 6. Save (pre zero-insertion) ----
write.csv(office_counts, "indaabin_office_counts.csv", row.names = FALSE)

# ============================================================
# 7. Zero insertion against the FULL municipal universe
# ============================================================

# ---- Load the full list of municipalities (from Day 1 key_frame) ----
key_frame_names <- read.csv("key_frame.csv", stringsAsFactors = FALSE) %>%
  select(CVEGEO, NOMGEO) %>%
  mutate(NOMGEO = stri_trans_general(toupper(trimws(NOMGEO)), "Latin-ASCII"))

nrow(key_frame_names)   # expect 2478

# ---- Left join: every municipality in key_frame is kept, even with no match ----
office_counts_full <- key_frame_names %>%
  left_join(office_counts %>% select(NOMGEO, n_federal_offices), by = "NOMGEO")

# ---- Create the no_ indicator BEFORE filling ----
office_counts_full <- office_counts_full %>%
  mutate(no_n_federal_offices = if_else(is.na(n_federal_offices), 1, 0))

# ---- Zero-fill: no matching properties means 0 federal offices ----
office_counts_full <- office_counts_full %>%
  mutate(n_federal_offices = if_else(is.na(n_federal_offices), 0, n_federal_offices))

# ---- Confirm no NAs remain ----
sum(is.na(office_counts_full$n_federal_offices))

# ---- Audit ----
nrow(office_counts_full)                       # should be 2478
sum(office_counts_full$no_n_federal_offices)    # how many municipalities have zero federal offices
mean(office_counts_full$no_n_federal_offices)   # what share of all municipalities

# ---- Save ----
write.csv(office_counts_full, "Federal_and_Parastatal_Real_Estate_Assets_Final.csv", row.names = FALSE)

