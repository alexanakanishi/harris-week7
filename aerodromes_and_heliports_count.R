# ============================================================
# DGAC Aeródromos y Helipuertos — Airport/Aerodrome Infrastructure
# Standalone script: filters to active (VIGENTE) facilities,
# converts lat/long from DMS to decimal degrees, spatially joins
# each facility to its municipality (point-in-polygon) using the
# Marco Geoestadístico shapefile, and counts per municipality.
# No dependency on any other script/file.
# ============================================================

library(dplyr)
library(stringr)
library(stringi)
library(readxl)
library(sf)

# ---- 1. Load raw DGAC registry ----
# Note: the raw file has two columns both named "NOMBRE" (aerodrome name
# in column D, owner name in column K) — readxl auto-renames the second
# occurrence. Check names(aero_raw) and adjust below if the suffix differs.
aero_raw <- read_excel("aerodromo-helipuertos-pub-290726.xlsx", skip = 1)

nrow(aero_raw)
names(aero_raw)

# ---- 2. Standardize column names we need ----
# Lat/long columns are renamed BY POSITION (columns 14-19), since their
# header text contains embedded line breaks (\r\n) that are unreliable to
# match exactly. Confirm these are still the right columns by checking
# names(aero_raw)[14:19] before trusting this — it should show the six
# LATITUD/LONGITUD degree/minute/second columns in order.
names(aero_raw)[14:19]   # sanity check: should print the 6 lat/long columns in order

aero_raw <- aero_raw %>%
  rename(
    tipo_aerodromo   = `TIPO AERÓDROMO`,
    nombre_aerodromo = `NOMBRE...4`,     # adjust suffix if readxl renamed differently
    estado    = ESTADO,
    municipio = MUNICIPIO,
    vigente   = `¿VIGENTE?`,
    situacion = SITUACIÓN
  )

names(aero_raw)[14:19] <- c("lat_deg", "lat_min", "lat_sec", "lon_deg", "lon_min", "lon_sec")

# ---- 3. Standardize text fields for filtering ----
aero_raw <- aero_raw %>%
  mutate(
    tipo_aerodromo_clean = str_trim(toupper(tipo_aerodromo)),
    vigente_clean         = str_trim(toupper(vigente)),
    situacion_clean       = str_trim(toupper(situacion))
  )

table(aero_raw$tipo_aerodromo_clean, useNA = "ifany")
table(aero_raw$vigente_clean, useNA = "ifany")
table(aero_raw$situacion_clean, useNA = "ifany")

# ---- 4. Filter to active facilities only ----
aero_active <- aero_raw %>%
  filter(
    vigente_clean == "SI",
    situacion_clean == "VIGENTE"
  )

nrow(aero_active)

# ---- 5. Convert lat/long from DMS (degrees/minutes/seconds) to decimal ----
# Mexico is west of the prime meridian, so longitude must be negative even
# though the raw file stores it as a positive number of degrees west.

# ---- Coerce DMS columns to numeric first — readxl may have read them as ----
# ---- character if any cell in the column had non-numeric content.       ----
aero_active <- aero_active %>%
  mutate(
    lat_deg = as.numeric(lat_deg),
    lat_min = as.numeric(lat_min),
    lat_sec = as.numeric(lat_sec),
    lon_deg = as.numeric(lon_deg),
    lon_min = as.numeric(lon_min),
    lon_sec = as.numeric(lon_sec)
  )

# ---- Check how many rows got NA'd out by the coercion (non-numeric entries) ----
sapply(aero_active[c("lat_deg", "lat_min", "lat_sec", "lon_deg", "lon_min", "lon_sec")],
       function(x) sum(is.na(x)))

aero_active <- aero_active %>%
  mutate(
    lat_decimal = lat_deg + lat_min / 60 + lat_sec / 3600,
    lon_decimal = -(lon_deg + lon_min / 60 + lon_sec / 3600)
  )

# ---- Audit: check for missing/implausible coordinates before the spatial join ----
sum(is.na(aero_active$lat_decimal))
sum(is.na(aero_active$lon_decimal))
range(aero_active$lat_decimal, na.rm = TRUE)   # Mexico's latitude spans roughly 14 to 33
range(aero_active$lon_decimal, na.rm = TRUE)   # Mexico's longitude spans roughly -118 to -86

# ---- Drop rows with missing or clearly invalid coordinates — can't spatially join these ----
aero_active_geo <- aero_active %>%
  filter(
    !is.na(lat_decimal), !is.na(lon_decimal),
    lat_decimal > 10, lat_decimal < 35,
    lon_decimal > -120, lon_decimal < -85
  )

nrow(aero_active_geo)                       # how many active facilities have usable coordinates
nrow(aero_active) - nrow(aero_active_geo)   # how many were dropped for bad/missing coordinates

# ---- 6. Split into aerodromes and heliports ----
aero_active_geo <- aero_active_geo %>%
  mutate(
    is_aerodrome = tipo_aerodromo_clean == "AERÓDROMO",
    is_heliport         = tipo_aerodromo_clean == "HELIPUERTO"
  )

# ---- 7. Build spatial points and read the municipal shapefile ----
aero_points <- st_as_sf(
  aero_active_geo,
  coords = c("lon_decimal", "lat_decimal"),
  crs = 4326   # WGS-84, the standard lat/long coordinate system
)

gdf <- st_read("Mexico_mun_layer_simplified/Mexico_mun_layer_simplified.shp",
               options = "ENCODING=LATIN1")

gdf$CVEGEO <- formatC(as.numeric(gdf$CVEGEO), width = 5, format = "d", flag = "0")

# ---- Reproject points to match the shapefile's CRS before joining ----
aero_points <- st_transform(aero_points, st_crs(gdf))

# ---- 8. Spatial join: assign each aerodrome point to its containing municipality ----
aero_joined <- st_join(aero_points, gdf %>% select(CVEGEO, NOMGEO), join = st_within)

# ---- Audit: how many facilities did NOT fall inside any municipality polygon? ----
# (can happen for coastal/offshore points, or coordinate errors)
sum(is.na(aero_joined$CVEGEO))
nrow(aero_joined) - sum(is.na(aero_joined$CVEGEO))   # successfully matched

aero_joined <- aero_joined %>% st_drop_geometry()

# ---- 9. Count per municipality (by CVEGEO now, not by name) ----
aerodrome_counts <- aero_joined %>%
  filter(!is.na(CVEGEO)) %>%
  group_by(CVEGEO) %>%
  summarise(
    n_aerodromes = sum(is_aerodrome),
    n_heliports         = sum(is_heliport),
    .groups = "drop"
  )

nrow(aerodrome_counts)
head(aerodrome_counts)

aerodrome_counts %>% arrange(desc(n_aerodromes)) %>% head(10)

write.csv(aerodrome_counts, "aerodrome_counts.csv", row.names = FALSE)

# ============================================================
# 10. Zero insertion against the FULL municipal universe
# ============================================================

key_frame_names <- gdf %>%
  st_drop_geometry() %>%
  select(CVEGEO, NOMGEO)

nrow(key_frame_names)   # expect 2478

aerodrome_counts_full <- key_frame_names %>%
  left_join(aerodrome_counts, by = "CVEGEO")

# ---- Confirm the join did NOT duplicate any municipality ----
# (a spatial join on CVEGEO keys can't collide the way name-matching could)
nrow(aerodrome_counts_full)                          # should still be 2478
sum(duplicated(aerodrome_counts_full$CVEGEO))         # should be 0

fill_cols <- c("n_aerodromes", "n_heliports")

for (col in fill_cols) {
  no_col <- paste0("no_", col)
  aerodrome_counts_full[[no_col]] <- if_else(is.na(aerodrome_counts_full[[col]]), 1, 0)
}

aerodrome_counts_full <- aerodrome_counts_full %>%
  mutate(across(all_of(fill_cols), ~ if_else(is.na(.), 0, .)))

# ---- Add binary "has an active facility" flags ----
aerodrome_counts_full <- aerodrome_counts_full %>%
  mutate(
    has_aerodrome = if_else(n_aerodromes > 0, 1, 0),
    has_heliport         = if_else(n_heliports > 0, 1, 0)
  )

# ---- Reorder columns ----
ordered_cols <- c("CVEGEO", "NOMGEO")
for (col in fill_cols) {
  ordered_cols <- c(ordered_cols, col, paste0("no_", col))
}
ordered_cols <- c(ordered_cols, "has_aerodrome", "has_heliport")

aerodrome_counts_full <- aerodrome_counts_full %>%
  select(all_of(ordered_cols))

sum(is.na(aerodrome_counts_full[fill_cols]))

nrow(aerodrome_counts_full)                       # should be 2478
sapply(paste0("no_", fill_cols), function(c) sum(aerodrome_counts_full[[c]]))
sum(aerodrome_counts_full$has_aerodrome)
sum(aerodrome_counts_full$has_heliport)

write.csv(aerodrome_counts_full, "aerodrome_counts_full.csv", row.names = FALSE)
