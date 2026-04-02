# =============================================================================
# Title:  GB_rsim_take_pull.R
# Purpose: Pull and format commercial landings and discard time series for
#          Georges Bank Rpath fitting. Combines landings + discards into a
#          single total take (t/km²/year) per Rpath group. Outputs
#          data-raw/take_fit.csv formatted for direct use by
#          GB_01_build_scenarios.R via read.fitting.catch().
#          Run this script in the GBRpath repo, then copy the output CSV to
#          Rpath_fitting/GB/take_fit.csv.
#
# Script location: GBRpath/data-raw/R/GB_rsim_take_pull.R
#
# Data sources:
#   - NEFSC Commercial Landings (STOCKEFF/CFDBS) via comlandr on sole
#   - NEFSC Commercial Discards (observer database) via comlandr on nova
#
# Key decisions:
#   - Take = landings + discards combined (total fishing mortality)
#   - Stdev set to 10% CV of take (commercial data has no formal
#     variance estimate from the database; 10% is a conservative placeholder
#     consistent with well-monitored fisheries)
#   - Scale = 1 (values already in t/km²)
#   - Species that crashed the model under forced catch in preliminary runs
#     are flagged but retained in the output — the updated biomass data
#     may resolve the instability. Remove from scene in GB_01 if needed.
#
# Output: GBRpath/data-raw/take_fit.csv
#         Copy to: Rpath_fitting/GB/take_fit.csv
#
# Requires: NEFSC network access (sole + nova), Oracle instant client, ROracle
# Packages: comlandr, survdat, tidyverse, sf, here, DBI, dbutils
#
# Author: M.T. Grezlik (updated from S. Lucey original)
# Date:   2026
# =============================================================================

library(comlandr)
library(survdat)
library(tidyverse)
library(sf)
library(here)
library(DBI)

pull_date  <- Sys.Date()
start_year <- 1985    # first year of GB Rpath hindcast
# end year is dynamic — pulls all available data from start_year onward

# GB statistical areas (GARFO/NEFSC area codes covering Georges Bank)
GB_stat_areas <- c(521, 522, 525, 526, 537, 551, 552, 561, 562)

# =============================================================================
# 0. Species lookup
#    Same lookup used in GB_biomass_pull.R — maps NESPP3 to Rpath groups.
#    Expected columns: NESPP3 (integer), RPATH (character)
# =============================================================================

spp <- read_csv(here("data-raw", "Species_codes.csv"),
                show_col_types = FALSE) |>
  mutate(RPATH = case_when(
    RPATH == "OtherFlatfish" ~ "OtherDemersals",
    RPATH == "OffHake"       ~ "SilverHake",
    RPATH == "AtlHalibut"   ~ "OtherDemersals",
    RPATH == "AtlCroaker"   ~ "SouthernDemersals",
    RPATH == "Weakfish"      ~ "SouthernDemersals",
    RPATH == "AmShad"        ~ "RiverHerring",
    RPATH == "StripedBass"   ~ "OtherDemersals",
    RPATH == "Tilefish"      ~ "SouthernDemersals",
    RPATH == "RedCrab"       ~ "Megabenthos",
    RPATH == "NShrimp"       ~ "OtherShrimps",
    # Resolve duplicate NESPP3 codes (single code covers multiple common names)
    NESPP3 == 117 ~ "OtherDemersals",    # Eel/Moray Uncl -> OtherDemersals
    NESPP3 == 333 ~ "OtherDemersals",    # Sea Bass NK / Rock Sea Bass -> OtherDemersals
    NESPP3 == 363 ~ "SmPelagics",        # Silversides (all) -> SmPelagics
    NESPP3 == 431 ~ "SouthernDemersals", # Puffer (all) -> SouthernDemersals
    NESPP3 == 456 ~ "SouthernDemersals", # Triggerfish (all) -> SouthernDemersals
    TRUE          ~ RPATH
  )) |>
  filter(!is.na(NESPP3)) |>
  select(NESPP3, RPATH) |>
  distinct() |>
  group_by(NESPP3) |>
  slice(1) |>        # enforce one Rpath group per NESPP3; case_when takes priority
  ungroup()

# =============================================================================
# 1. Georges Bank area in km²
#    Used to convert metric tons to t/km².
#    Calculated from the EPU shapefile using survdat::get_area().
# =============================================================================

epu      <- sf::st_read(here("data-raw", "gis", "EPU_extended.shp"),
                        quiet = TRUE)
epu_area <- survdat::get_area(epu, "EPU")
gb_area  <- as.numeric(epu_area$Area[epu_area$STRATUM == "GB"])

message(sprintf("Georges Bank area: %.1f km²", gb_area))

# =============================================================================
# 2. Commercial Landings (sole database)
# =============================================================================

message("Connecting to sole for landings...")
channel_sole <- dbutils::connect_to_database("NEFSC_pw_oraprod", "MGREZLIK")

message("Pulling commercial landings in decade chunks to manage memory...")

# Split into decade chunks to avoid memory allocation errors during
# area aggregation of the full time series
year_chunks <- list(
  1985:1989,
  1990:1994,
  1995:1999,
  2000:2004,
  2005:2009,
  2010:2014,
  2015:2019,
  2020:as.integer(format(Sys.Date(), "%Y"))
)

landings_chunks <- vector("list", length(year_chunks))

for (i in seq_along(year_chunks)) {
  chunk_years <- year_chunks[[i]]
  message(sprintf("  Pulling years %d-%d...",
                  min(chunk_years), max(chunk_years)))
  
  landings_chunks[[i]] <- comlandr::get_comland_data(
    channel_sole,
    filterByYear = chunk_years,
    filterByArea = GB_stat_areas,
    aggGear      = FALSE,
    aggArea      = TRUE,
    unkVar       = c("MONTH", "NEGEAR"),
    knStrata     = c("HY", "QY", "MONTH", "NEGEAR", "TONCL2")
  )
  message(sprintf("  Chunk %d complete: %d rows",
                  i, nrow(landings_chunks[[i]]$comland)))
}

# Bind chunks — keep list structure for get_comdisc_data compatibility
# Use first chunk as the template and replace $comland with full series
landings_list        <- landings_chunks[[1]]
landings_list$comland <- bind_rows(
  lapply(landings_chunks, function(x) as_tibble(x$comland))
)

# comlandr returns a list; data is in $comland
landings_raw <- landings_list$comland

message(sprintf("Landings pulled: %d rows, years %d-%d",
                nrow(landings_raw),
                min(landings_raw$YEAR, na.rm = TRUE),
                max(landings_raw$YEAR, na.rm = TRUE)))

# Filter to GB EPU, join Rpath groups, aggregate to RPATH x YEAR
landings_index <- landings_raw |>
  as_tibble() |>
  filter(EPU == "GB") |>
  left_join(spp, by = "NESPP3") |>
  filter(!is.na(RPATH)) |>
  group_by(RPATH, YEAR) |>
  summarise(
    landings_mt = sum(SPPLIVMT, na.rm = TRUE),
    .groups     = "drop"
  ) |>
  mutate(landings_tkm2 = landings_mt / gb_area) |>
  filter(YEAR >= start_year)

message(sprintf("Landings index: %d species-year records, %d species",
                nrow(landings_index),
                n_distinct(landings_index$RPATH)))

# =============================================================================
# 3. Commercial Discards
#    Uses the same channel_sole connection — dbutils routes to the correct
#    database internally. Pulled in decade chunks to avoid memory errors.
#    get_comdisc_data() requires the corresponding landings chunk as input
#    to compute discard ratios correctly.
# =============================================================================

message("Pulling commercial discards in decade chunks to manage memory...")

discards_chunks <- vector("list", length(year_chunks))

for (i in seq_along(year_chunks)) {
  chunk_years <- year_chunks[[i]]
  message(sprintf("  Pulling discard years %d-%d...",
                  min(chunk_years), max(chunk_years)))
  
  discards_chunks[[i]] <- comlandr::get_comdisc_data(
    channel_sole,
    landings_chunks[[i]],   # pass matching landings chunk for ratio estimation
    aggArea = TRUE,
    aggGear = FALSE
  )
  message(sprintf("  Discard chunk %d complete: %d rows",
                  i, nrow(discards_chunks[[i]]$comdisc)))
}

# Bind all discard chunks
discards_raw <- bind_rows(
  lapply(discards_chunks, function(x) as_tibble(x$comdisc))
)

message(sprintf("Discards pulled: %d rows, years %d-%d",
                nrow(discards_raw),
                min(discards_raw$YEAR, na.rm = TRUE),
                max(discards_raw$YEAR, na.rm = TRUE)))

# Filter to GB EPU, join Rpath groups, aggregate to RPATH x YEAR
discards_index <- discards_raw |>
  as_tibble() |>
  filter(EPU == "GB") |>
  left_join(spp, by = "NESPP3") |>
  filter(!is.na(RPATH)) |>
  group_by(RPATH, YEAR) |>
  summarise(
    discards_mt = sum(DISMT, na.rm = TRUE),
    .groups     = "drop"
  ) |>
  filter(!is.na(discards_mt)) |>
  mutate(discards_tkm2 = discards_mt / gb_area) |>
  filter(YEAR >= start_year)

message(sprintf("Discards index: %d species-year records, %d species",
                nrow(discards_index),
                n_distinct(discards_index$RPATH)))

dbDisconnect(channel_sole)
message("Disconnected from database.")

# =============================================================================
# 4. Combine landings and discards into total take
#    Take = landings + discards per species per year.
#    Species with landings but no discard record get discards = 0.
#    Species with discards but no landing record are included (bycatch only).
# =============================================================================

take_index <- landings_index |>
  full_join(discards_index |> select(RPATH, YEAR, discards_tkm2),
            by = c("RPATH", "YEAR")) |>
  mutate(
    landings_tkm2  = replace_na(landings_tkm2,  0),
    discards_tkm2  = replace_na(discards_tkm2,  0),
    take_tkm2      = landings_tkm2 + discards_tkm2
  ) |>
  filter(take_tkm2 > 0)   # drop species-years with zero total take

# =============================================================================
# 5. Rpath group name reassignments
#    Applied after aggregation so that reassigned species are summed
#    into their target group correctly.
# =============================================================================

# Re-aggregate after any remaining name collisions from case_when above
take_index <- take_index |>
  group_by(RPATH, YEAR) |>
  summarise(
    take_tkm2 = sum(take_tkm2, na.rm = TRUE),
    .groups   = "drop"
  )

# =============================================================================
# 6. Uncertainty
#    Commercial landings and discards have no formal variance estimate
#    from the database. We assign a 10% CV as a conservative placeholder
#    consistent with well-monitored commercial fisheries.
#    This can be updated per-species if better uncertainty estimates
#    become available (e.g. from stock assessments).
# =============================================================================

take_index <- take_index |>
  mutate(take_sd_tkm2 = take_tkm2 * 0.10)

# =============================================================================
# 7. Format output CSV
#    Required columns for read.fitting.catch() in merge_ecofitting.R:
#    Group, Year, Value, Stdev, Scale
# =============================================================================

take_out <- take_index |>
  transmute(
    Group = RPATH,
    Year  = YEAR,
    Value = round(take_tkm2,    8),
    Stdev = round(take_sd_tkm2, 8),
    Scale = 1     # values already in t/km²
  ) |>
  arrange(Group, Year)

# =============================================================================
# 8. Diagnostic checks
# =============================================================================

message("\n--- Output diagnostics ---")
message(sprintf("Species in output : %d", n_distinct(take_out$Group)))
message(sprintf("Total records     : %d", nrow(take_out)))
message(sprintf("Year range        : %d - %d",
                min(take_out$Year), max(take_out$Year)))

# Flag zero, negative, or NA values
bad_rows <- take_out |>
  filter(Value <= 0 | Stdev <= 0 | is.na(Value) | is.na(Stdev))
if (nrow(bad_rows) > 0) {
  warning(sprintf(
    "%d rows with zero/negative/NA Value or Stdev:",
    nrow(bad_rows)
  ))
  print(bad_rows)
} else {
  message("All Value and Stdev entries are positive and non-NA.")
}

# Year coverage per species
coverage <- take_out |>
  group_by(Group) |>
  summarise(
    first_year = min(Year),
    last_year  = max(Year),
    n_years    = n(),
    mean_take  = round(mean(Value), 6),
    .groups    = "drop"
  ) |>
  arrange(Group)

message("\nYear coverage and mean take per species:")
print(coverage, n = Inf)

# Flag species from preliminary runs known to cause biomass crashes
# under forced catch — review these after running GB_01
unstable_flag <- c(
  "Redfish", "WitchFlounder", "SummerFlounder",
  "Goosefish", "Fourspot", "SouthernDemersals", "AtlMackerel"
)
flagged <- take_out |>
  filter(Group %in% unstable_flag) |>
  group_by(Group) |>
  summarise(n_years = n(), mean_take = round(mean(Value), 6), .groups = "drop")

if (nrow(flagged) > 0) {
  message("\nPreviously unstable species present in output (review after GB_01):")
  print(flagged)
}

# =============================================================================
# 9. Save output and metadata
# =============================================================================

out_path <- here("data-raw", "take_fit.csv")
write_csv(take_out, out_path)
message(sprintf("\nSaved %d records to %s", nrow(take_out), out_path))
message("Copy this file to Rpath_fitting/GB/take_fit.csv before running GB_01.")

tibble(
  pull_date        = as.character(pull_date),
  start_year       = start_year,
  end_year         = max(take_out$Year),
  n_species        = n_distinct(take_out$Group),
  n_records        = nrow(take_out),
  gb_area_km2      = gb_area,
  cv_assumption    = 0.10,
  comlandr_version = as.character(packageVersion("comlandr"))
) |>
  write_csv(here("data-raw", "take_fit_metadata.csv"))

message("Pull metadata saved to data-raw/take_fit_metadata.csv")