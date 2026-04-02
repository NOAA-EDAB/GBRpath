# =============================================================================
# Title:  GB_rsim_biomass_pull.R
# Purpose: Pull and format biomass time series for Georges Bank Rpath fitting
#          using the survdat package. Outputs data-raw/biomass_fit.csv in
#          t/km² with uncertainty estimates, formatted for direct use by
#          GB_01_build_scenarios.R via read.fitting.biomass().
#          Run this script in the GBRpath repo, then copy the output CSV to
#          Rpath_fitting/GB/biomass_fit.csv.
#
# Script location: GBRpath/data-raw/R/GB_biomass_pull.R
#
# Data sources:
#   - NEFSC Fall Bottom Trawl Survey (stratified mean) via survdat
#   - NEFSC Scallop Dredge Survey via survdat
#   - NEFSC Clam Survey via survdat
#
# Key decisions:
#   - NO pre-application of catchability (q) corrections. q is estimated
#     internally by rsim.fit.obj() during Rpath fitting for all index series.
#     Pre-applying q caused AtlHerring and AtlMackerel to be inflated by
#     factors of 91x and 62x respectively in the previous data pull.
#   - Stdev is derived from the stratified variance output of
#     calc_stratified_mean(), propagated through unit conversion.
#   - All series output with Type = "index" and Scale = 1.
#   - Groups not representatively sampled by trawl are excluded from output
#     and handled via environmental forcing in GB_01_build_scenarios.R.
#
# Output: GBRpath/data-raw/biomass_fit.csv
#         Copy to: Rpath_fitting/GB/biomass_fit.csv
#
# Requires: NEFSC network access, Oracle instant client, ROracle
# Packages: survdat, tidyverse, sf, here, DBI, dbutils
#
# Author: M.T. Grezlik
# Date:   2026
# =============================================================================

library(survdat)
library(tidyverse)
library(sf)
library(here)
library(DBI)

pull_date  <- Sys.Date()
start_year <- 1985    # first year of GB Rpath hindcast
# end year is dynamic — pulls all available data from start_year onward

# =============================================================================
# 0. Database connection
#    Requires Oracle instant client and ROracle installed.
# =============================================================================

channel <- dbutils::connect_to_database("NEFSC_pw_oraprod", "MGREZLIK")
# Replace "username" with your NEFSC database username.

# =============================================================================
# 1. Species lookup table
#    Maps SVSPP codes to Rpath group names.
#    This file should live in data-raw/ and be version controlled.
#    Expected columns: SVSPP (integer), RPATH (character)
# =============================================================================

spp <- read_csv(here("data-raw", "Species_codes.csv"),
                show_col_types = FALSE) |>
  # Rpath group name corrections
  mutate(RPATH = case_when(
    RPATH == "OtherFlatfish" ~ "OtherDemersals",
    RPATH == "OffHake"       ~ "SilverHake",
    RPATH == "AtlHalibut"   ~ "OtherDemersals",
    RPATH == "Weakfish"      ~ "SouthernDemersals",
    RPATH == "AmShad"        ~ "RiverHerring",
    RPATH == "StripedBass"   ~ "OtherDemersals",
    RPATH == "Tilefish"      ~ "SouthernDemersals",
    RPATH == "RedCrab"       ~ "Megabenthos",
    RPATH == "NShrimp"       ~ "OtherShrimps",
    TRUE                     ~ RPATH
  )) |>
  select(SVSPP, RPATH) |>
  distinct()

# =============================================================================
# 2. NEFSC Fall Bottom Trawl Survey
#    Pull tow-level data then compute stratified mean biomass and variance
#    for Georges Bank using the EPU shapefile as the area definition.
# =============================================================================

message("Pulling bottom trawl survey data...")

# Pull tow-level data with vessel/gear conversion factors applied.
# conversion.factor = TRUE creates a continuous time series across
# vessel and gear changes (Delaware II -> Albatross IV -> Henry Bigelow).
trawl_list <- survdat::get_survdat_data(
  channel,
  getLengths        = FALSE,
  conversion.factor = TRUE
)
trawl_raw <- trawl_list$survdat

message(sprintf("Trawl data pulled: %d rows, years %d-%d",
                nrow(trawl_raw),
                min(trawl_raw$YEAR, na.rm = TRUE),
                max(trawl_raw$YEAR, na.rm = TRUE)))

# Load EPU shapefile for post-stratification to Georges Bank
epu <- sf::st_read(here("data-raw", "gis", "EPU_extended.shp"), quiet = TRUE)

# Calculate stratified mean biomass and variance.
# filterBySeason = "FALL" uses fall survey only.
# filterByArea = "GB" restricts to Georges Bank EPU strata.
# tidy = TRUE returns long format with separate rows for each variable.
# Output units from calc_stratified_mean are kg/tow.
message("Calculating stratified mean biomass for Georges Bank (Fall)...")

trawl_strat <- survdat::calc_stratified_mean(
  trawl_raw,
  areaPolygon     = epu,
  areaDescription = "EPU",
  filterByArea    = "GB",
  filterBySeason  = "FALL",
  tidy            = TRUE
)

# Check what variables are returned — useful on first run
message("Variables in calc_stratified_mean output: ",
        paste(unique(trawl_strat$variable), collapse = ", "))

# Extract biomass (kg/tow) and variance (kg/tow)²
trawl_B <- trawl_strat |>
  filter(variable == "strat.biomass") |>
  select(SVSPP, YEAR, biomass_kgtow = value)

trawl_V <- trawl_strat |>
  filter(variable == "biomass.var") |>
  select(SVSPP, YEAR, var_kgtow = value)

trawl_index <- trawl_B |>
  left_join(trawl_V, by = c("SVSPP", "YEAR")) |>
  mutate(
    # Replace missing or non-positive variance with 10% CV placeholder
    var_kgtow = if_else(
      is.na(var_kgtow) | var_kgtow <= 0,
      (biomass_kgtow * 0.10)^2,
      var_kgtow
    )
  )

# Join Rpath group names
trawl_index <- trawl_index |>
  left_join(spp, by = "SVSPP") |>
  filter(!is.na(RPATH))

# Convert kg/tow to t/km²
# Standardized tow sweeps 0.0384 km² (41.4m headrope at 3.5 knots for 30 min)
# Factor = 0.001 (kg -> t) / 0.0384 (km²/tow) = 0.02604
conversion_trawl <- 0.001 / 0.0384

trawl_index <- trawl_index |>
  mutate(
    B_tkm2   = biomass_kgtow * conversion_trawl,
    Bsd_tkm2 = sqrt(var_kgtow) * conversion_trawl   # SD scales linearly
  ) |>
  select(RPATH, YEAR, B_tkm2, Bsd_tkm2)

# Aggregate reassigned groups (e.g. Weakfish + Tilefish -> SouthernDemersals).
# Sum biomasses; sum variances before taking sqrt (not sum of SDs).
trawl_index <- trawl_index |>
  group_by(RPATH, YEAR) |>
  summarise(
    B_tkm2   = sum(B_tkm2,        na.rm = TRUE),
    Bsd_tkm2 = sqrt(sum(Bsd_tkm2^2, na.rm = TRUE)),
    .groups  = "drop"
  ) |>
  filter(YEAR >= start_year)

message(sprintf("Trawl index: %d species-year records, %d species, years %d-%d",
                nrow(trawl_index),
                n_distinct(trawl_index$RPATH),
                min(trawl_index$YEAR),
                max(trawl_index$YEAR)))

# =============================================================================
# 3. Scallop Dredge Survey
#    Dedicated survey for Atlantic scallop; more accurate than trawl.
#    Restricted to Georges Bank, Summer season.
# =============================================================================

message("Pulling scallop survey data...")

scallop_list <- survdat::get_survdat_scallop_data(
  channel,
  getWeightLength = TRUE
)
scallop_raw <- scallop_list$survdat

scallop_strat <- survdat::calc_stratified_mean(
  scallop_raw,
  areaPolygon     = epu,
  areaDescription = "EPU",
  filterByArea    = "GB",
  filterBySeason  = "SUMMER",
  tidy            = TRUE
)

scallop_B <- scallop_strat |>
  filter(variable == "strat.biomass") |>
  select(YEAR, biomass_kgtow = value)

scallop_V <- scallop_strat |>
  filter(variable == "biomass.var") |>
  select(YEAR, var_kgtow = value)

scallop_index <- scallop_B |>
  left_join(scallop_V, by = "YEAR") |>
  mutate(
    var_kgtow = if_else(
      is.na(var_kgtow) | var_kgtow <= 0,
      (biomass_kgtow * 0.15)^2,
      var_kgtow
    )
  )

# Convert kg/tow to t/km²
# Dredge width : 0.001317 nmi * 1.852 km/nmi = 0.002439 km
# Tow length   : 1.0 nmi   * 1.852 km/nmi = 1.852 km
# Tow area     : 0.002439 * 1.852 = 0.004517 km²
conversion_scallop <- 0.001 / (0.001317 * 1.852 * 1.0 * 1.852)

scallop_index <- scallop_index |>
  mutate(
    RPATH    = "AtlScallop",
    B_tkm2   = biomass_kgtow * conversion_scallop,
    Bsd_tkm2 = sqrt(var_kgtow) * conversion_scallop
  ) |>
  filter(YEAR >= start_year) |>
  select(RPATH, YEAR, B_tkm2, Bsd_tkm2)

message(sprintf("Scallop index: %d records, years %d-%d",
                nrow(scallop_index),
                min(scallop_index$YEAR),
                max(scallop_index$YEAR)))

# =============================================================================
# 4. Clam Survey
#    Dedicated survey for ocean quahog and surf clam on Georges Bank.
# =============================================================================

message("Pulling clam survey data...")

clam_list <- survdat::get_survdat_clam_data(channel)
clam_raw  <- clam_list$data

# Check available columns on first run in case column names differ
message("Clam survey columns: ", paste(names(clam_raw), collapse = ", "))

# Filter to Georges Bank clam region; compute mean and SE across tows.
# BIOMASS.MW = meat weight biomass per tow in kg.
# clam.region is a post-stratification column added by get_survdat_clam_data.
clam_index <- clam_raw |>
  as_tibble() |>
  filter(!is.na(SVSPP), clam.region == "GB") |>
  mutate(YEAR = as.integer(YEAR)) |>
  group_by(YEAR, SVSPP) |>
  summarise(
    biomass_kgtow = mean(BIOMASS.MW, na.rm = TRUE),
    var_kgtow     = var(BIOMASS.MW,  na.rm = TRUE),
    n_tows        = n(),
    .groups       = "drop"
  ) |>
  mutate(
    # SE of the mean = sqrt(var / n); use as per-year uncertainty
    se_kgtow = sqrt(var_kgtow / n_tows),
    # Replace NA SE (single-tow years) with 20% CV placeholder
    se_kgtow = if_else(
      is.na(se_kgtow) | se_kgtow <= 0,
      biomass_kgtow * 0.20,
      se_kgtow
    )
  )

# Convert kg/tow to t/km²
# Dredge width : 0.0039624 km
# Tow length   : 0.374 km
# Tow area     : 0.0039624 * 0.374 = 0.001482 km²
conversion_clam <- 0.001 / (0.0039624 * 0.374)

clam_index <- clam_index |>
  mutate(
    RPATH    = if_else(SVSPP == 409, "OceanQuahog", "SurfClam"),
    B_tkm2   = biomass_kgtow * conversion_clam,
    Bsd_tkm2 = se_kgtow      * conversion_clam
  ) |>
  filter(YEAR >= start_year) |>
  select(RPATH, YEAR, B_tkm2, Bsd_tkm2)

message(sprintf("Clam index: %d records, years %d-%d",
                nrow(clam_index),
                min(clam_index$YEAR),
                max(clam_index$YEAR)))

# =============================================================================
# 5. Combine surveys
#    Replace trawl estimates for shellfish species with dedicated surveys.
# =============================================================================

bio_combined <- bind_rows(
  trawl_index |>
    filter(!RPATH %in% c("AtlScallop", "OceanQuahog", "SurfClam")) |>
    mutate(Source = "nefsc_trawl"),
  scallop_index |>
    mutate(Source = "nefsc_scallop"),
  clam_index |>
    mutate(Source = "nefsc_clam")
)

# =============================================================================
# 6. Exclude groups not suitable for biomass fitting
#    Groups listed here either have near-zero trawl catchability or are
#    better constrained through environmental forcing in GB_01.
#    Rationale documented inline.
# =============================================================================

exclude_groups <- c(
  # Marine mammals and seabirds: not sampled by bottom trawl or dredge
  "SeaBirds", "Pinnipeds", "BaleenWhales", "Odontocetes",
  # Lower trophic groups: plankton net surveys only; handled via
  # ForcedBio environmental forcing in GB_01_build_scenarios.R
  "Phytoplankton", "LgCopepods", "SmCopepods",
  "Microzooplankton", "GelZooplankton", "Krill", "Micronekton", "Bacteria",
  # Macrobenthos: obs/model ratio ~1e-5; trawl samples essentially none of
  # true biomass; forced via benthic time series in GB_01
  "Macrobenthos",
  # Megabenthos: obs/model ratio ~0.011; same issue; forced in GB_01
  "Megabenthos",
  # OtherCephalopods: obs/model ratio ~0.005; poorly sampled
  "OtherCephalopods",
  # SmPelagics: obs/model ratio ~0.010; not well sampled by bottom trawl
  "SmPelagics",
  # Groups not in GB Rpath model — catch-all or misassigned survey codes
  "Fauna",        # unidentified organisms catch-all code
  "Freshwater",   # freshwater species, not a GB Rpath group
  "LargePelagics",# likely maps to HMS which is already included
  "Rays",         # not modelled separately in GB Rpath
  "OtherShrimp"   # name mismatch with OtherShrimps — single spurious record
)

bio_fit <- bio_combined |>
  filter(!RPATH %in% exclude_groups)

# =============================================================================
# 7. Format output CSV
#    Required columns for read.fitting.biomass() in merge_ecofitting.R:
#    Group, Year, Value, Stdev, Scale, Source, Type
# =============================================================================

bio_out <- bio_fit |>
  transmute(
    Group  = RPATH,
    Year   = YEAR,
    Value  = round(B_tkm2,   8),
    Stdev  = round(Bsd_tkm2, 8),
    Scale  = 1,          # values already in t/km²; no further scaling needed
    Source = Source,
    Type   = "index"     # all series relative indices; q estimated by fitting
  ) |>
  arrange(Group, Year)

# =============================================================================
# 8. Diagnostic checks before saving
# =============================================================================

message("\n--- Output diagnostics ---")
message(sprintf("Species in output : %d", n_distinct(bio_out$Group)))
message(sprintf("Total records     : %d", nrow(bio_out)))
message(sprintf("Year range        : %d - %d",
                min(bio_out$Year), max(bio_out$Year)))

# Flag zero, negative, or NA values — read.fitting.biomass() drops these
# silently so better to catch them here
bad_rows <- bio_out |>
  filter(Value <= 0 | Stdev <= 0 | is.na(Value) | is.na(Stdev))

if (nrow(bad_rows) > 0) {
  warning(sprintf(
    "%d rows have zero/negative/NA Value or Stdev — these will be dropped by read.fitting.biomass():",
    nrow(bad_rows)
  ))
  print(bad_rows)
} else {
  message("All Value and Stdev entries are positive and non-NA.")
}

# Year coverage and source per species
coverage <- bio_out |>
  group_by(Group, Source) |>
  summarise(
    first_year = min(Year),
    last_year  = max(Year),
    n_years    = n(),
    .groups    = "drop"
  ) |>
  arrange(Group)

message("\nYear coverage per species:")
print(coverage, n = Inf)

# =============================================================================
# 9. Save output and metadata
# =============================================================================

out_path <- here("data-raw", "biomass_fit.csv")
write_csv(bio_out, out_path)
message(sprintf("\nSaved %d records to %s", nrow(bio_out), out_path))
message("Copy this file to Rpath_fitting/GB/biomass_fit.csv before running GB_01.")

tibble(
  pull_date       = as.character(pull_date),
  start_year      = start_year,
  end_year        = max(bio_out$Year),
  n_species       = n_distinct(bio_out$Group),
  n_records       = nrow(bio_out),
  survdat_version = as.character(packageVersion("survdat"))
) |>
  write_csv(here("data-raw", "biomass_fit_metadata.csv"))

message("Pull metadata saved to data-raw/biomass_fit_metadata.csv")

dbDisconnect(channel)
message("Database connection closed.")
