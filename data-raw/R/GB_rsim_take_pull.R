# =============================================================================
# Title:  GB_rsim_take_pull.R
# Purpose: Pull and format commercial landings and discard time series for
#          Georges Bank Rpath fitting. Combines landings + discards into a
#          single total take (t/km²/year) per Rpath group. Outputs
#          data-raw/take_fit.csv formatted for direct use by
#          GB_01_build_scenarios.R via read.fitting.catch().
#          Run this script on the RStudio container, then copy the output
#          CSV to Rpath_fitting/GB/take_fit.csv.
#
# Script location: GBRpath/data-raw/R/GB_rsim_take_pull.R
#
# !! RUN THIS SCRIPT ON THE RSTUDIO CONTAINER, NOT LOCALLY !!
#    The discard pull requires ~10 GB RAM during area aggregation.
#    The container has ~740 GB available; a standard laptop does not.
#    Connect via VPN before opening this script.
#
# HOW TO RE-RUN ANNUALLY:
#    1. Connect to VPN and open this script in the RStudio container
#    2. Update your NEFSC username on the connect_to_database() line if needed
#    3. Source the full script — no other changes required
#       The end year updates automatically to the previous calendar year
#    4. Copy data-raw/take_fit.csv to Rpath_fitting/GB/take_fit.csv
#    5. Re-run GB_01_build_scenarios.R in Rpath_fitting to update the scene
#
# Data sources:
#   - NEFSC Commercial Landings (STOCKEFF/CFDBS) via comlandr
#   - NEFSC Commercial Discards (observer database) via comlandr
#   Both pulled through a single dbutils connection to NEFSC_pw_oraprod.
#
# Key decisions:
#   - userAreas (full EPU mapping table) used instead of filterByArea.
#     This follows the intended comlandr usage pattern and avoids the
#     indefinite run times caused by sparse strata imputation when only
#     GB statistical areas are passed.
#   - aggGear = TRUE used for both landings and discards. Aggregating gears
#     upfront reduces the number of strata processed during discard ratio
#     estimation, which is why the discard pull completes in ~1 hour rather
#     than running indefinitely.
#   - Take = landings + discards (total fishing mortality per species per year)
#   - Stdev = 10% CV of take (no formal variance from database; conservative
#     placeholder consistent with well-monitored commercial fisheries)
#   - Scale = 1 (values already in t/km²)
#   - End year capped at previous calendar year — NAFO zip files for the
#     current year are typically not yet published
#
# Output: GBRpath/data-raw/take_fit.csv
#         GBRpath/data-raw/take_fit_metadata.csv
#         Copy take_fit.csv to: Rpath_fitting/GB/take_fit.csv
#
# Requires: comlandr >= v1.1.1, NEFSC network access via VPN,
#           Oracle instant client, ROracle
# Packages: comlandr, survdat, tidyverse, sf, here, data.table, DBI, dbutils
#
# Author: M.T. Grezlik (updated from S. Lucey original)
# Date:   2026
# =============================================================================

library(comlandr)
library(survdat)
library(tidyverse)
library(sf)
library(here)
library(data.table)
library(DBI)

pull_date  <- Sys.Date()
start_year <- 1985
# Capped at previous year — NAFO zip files for current year typically not
# published yet. Requires comlandr >= v1.1.1 (v1.1.0 errored on missing
# NAFO files with "object 'Country' not found").
end_year   <- as.integer(format(Sys.Date(), "%Y")) - 1

message(sprintf("Starting GB take pull: %d-%d", start_year, end_year))

# =============================================================================
# 0. Environment checks
# =============================================================================

# Confirm comlandr version
cv <- packageVersion("comlandr")
message(sprintf("comlandr version: %s", cv))
if (cv < "1.1.1") {
  stop("comlandr >= v1.1.1 required. Update with: ",
       "remotes::install_github('NOAA-EDAB/comlandr')")
}

# Confirm sufficient RAM (discard aggregation requires ~10 GB)
ram_available_gb <- as.numeric(
  system("awk '/MemAvailable/ {print $2}' /proc/meminfo", intern = TRUE)
) / 1024 / 1024
message(sprintf("Available RAM: %.1f GB", ram_available_gb))
if (ram_available_gb < 12) {
  stop(sprintf(
    "Insufficient RAM (%.1f GB available, ~12 GB required). ",
    ram_available_gb
  ), "Run this script on the RStudio container, not locally.")
}

# =============================================================================
# 1. Species lookup
#    Maps NESPP3 codes to Rpath group names.
#    Duplicate NESPP3 codes (one code covering multiple common names) are
#    resolved explicitly so each code maps to exactly one Rpath group.
# =============================================================================

spp <- read_csv(here("data-raw", "Species_codes.csv"),
                show_col_types = FALSE) |>
  mutate(RPATH = case_when(
    # Standard group reassignments
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
    # Duplicate NESPP3 resolutions (one code, multiple common names)
    # Identified by: spp |> group_by(NESPP3) |> filter(n() > 1)
    NESPP3 == 117            ~ "OtherDemersals",    # Eel Uncl / Moray Uncl
    NESPP3 == 333            ~ "OtherDemersals",    # Sea Bass NK / Rock Sea Bass
    NESPP3 == 363            ~ "SmPelagics",        # Silversides (all forms)
    NESPP3 == 431            ~ "SouthernDemersals", # Puffer (all forms)
    NESPP3 == 456            ~ "SouthernDemersals", # Triggerfish/Filefish (all)
    TRUE                     ~ RPATH
  )) |>
  filter(!is.na(NESPP3)) |>
  select(NESPP3, RPATH) |>
  distinct() |>
  group_by(NESPP3) |>
  slice(1) |>   # enforce one Rpath group per NESPP3 after case_when
  ungroup()

# Verify no duplicates remain
dupes <- spp |> group_by(NESPP3) |> filter(n() > 1)
if (nrow(dupes) > 0) {
  warning("Duplicate NESPP3 codes found — review species lookup:")
  print(dupes)
} else {
  message(sprintf("Species lookup: %d unique NESPP3 codes loaded.", nrow(spp)))
}

# =============================================================================
# 2. Georges Bank area in km²
#    Used to convert metric tons to t/km².
# =============================================================================

epu      <- sf::st_read(here("data-raw", "gis", "EPU_extended.shp"),
                        quiet = TRUE)
epu_area <- survdat::get_area(epu, "EPU")
gb_area  <- as.numeric(epu_area$Area[epu_area$STRATUM == "GB"])
message(sprintf("Georges Bank area: %.1f km²", gb_area))

# =============================================================================
# 3. EPU area mapping table
#    Covers all four EPUs — this is the intended input for comlandr's
#    userAreas argument. Passing the full shelf context (not just GB) allows
#    comlandr to resolve unknown catch strata correctly before filtering to GB.
# =============================================================================

gom <- data.table(AREA = c(500, 510, 512:515),                        EPU = "GOM")
gb  <- data.table(AREA = c(521:526, 551, 552, 561, 562),               EPU = "GB")
mab <- data.table(AREA = c(537, 539, 600, 612:616,
                           621, 622, 625, 626, 631, 632),              EPU = "MAB")
ss  <- data.table(AREA = c(463:467, 511),                              EPU = "SS")

epu_areas           <- rbindlist(list(gom, gb, mab, ss))
epu_areas[, NESPP3   := 1]
epu_areas[, MeanProp := 1]

# =============================================================================
# 4. Database connection
#    Single connection handles both landings and discards.
# =============================================================================

channel <- dbutils::connect_to_database("NEFSC_pw_oraprod", "MGREZLIK")

# =============================================================================
# 5. Commercial Landings
#    Pulled as a single call using the full EPU area mapping table.
#    aggGear = TRUE aggregates across gear types upfront, which is required
#    for the discard pull to complete in a reasonable time.
# =============================================================================

message(sprintf("[%s] Pulling landings %d-%d...", Sys.time(), start_year, end_year))

landings <- comlandr::get_comland_data(
  channel,
  filterByYear = start_year:end_year,
  userAreas    = epu_areas,
  aggGear      = TRUE,
  aggArea      = TRUE
)

message(sprintf("[%s] Landings complete: %d rows",
                Sys.time(), nrow(landings$comland)))

# =============================================================================
# 6. Commercial Discards
#    Passed the full landings object (not just GB) so discard ratios are
#    computed with the correct regional context.
#    aggGear = TRUE must match the landings pull.
# =============================================================================

message(sprintf("[%s] Pulling discards...", Sys.time()))

discards <- comlandr::get_comdisc_data(
  channel,
  landings,
  aggArea = TRUE,
  aggGear = TRUE
)

message(sprintf("[%s] Discards complete: %d rows",
                Sys.time(), nrow(discards$comdisc)))

dbDisconnect(channel)
message(sprintf("[%s] Database connection closed.", Sys.time()))

# =============================================================================
# 7. Aggregate landings to Rpath group x year in t/km²
# =============================================================================

landings_index <- landings$comland |>
  as_tibble() |>
  filter(EPU == "GB") |>
  left_join(spp, by = "NESPP3") |>
  filter(!is.na(RPATH)) |>
  group_by(RPATH, YEAR) |>
  summarise(landings_mt = sum(SPPLIVMT, na.rm = TRUE), .groups = "drop") |>
  mutate(landings_tkm2 = landings_mt / gb_area) |>
  filter(YEAR >= start_year)

message(sprintf("Landings index: %d records, %d species",
                nrow(landings_index), n_distinct(landings_index$RPATH)))

# =============================================================================
# 8. Aggregate discards to Rpath group x year in t/km²
# =============================================================================

discards_index <- discards$comdisc |>
  as_tibble() |>
  filter(EPU == "GB") |>
  left_join(spp, by = "NESPP3") |>
  filter(!is.na(RPATH), !is.na(DISMT)) |>
  group_by(RPATH, YEAR) |>
  summarise(discards_mt = sum(DISMT, na.rm = TRUE), .groups = "drop") |>
  mutate(discards_tkm2 = discards_mt / gb_area) |>
  filter(YEAR >= start_year)

message(sprintf("Discards index: %d records, %d species",
                nrow(discards_index), n_distinct(discards_index$RPATH)))

# =============================================================================
# 9. Combine into total take
#    Take = landings + discards per species per year.
#    Species with landings but no discard record get discards = 0.
#    Species with discards but no landing record are included (bycatch only).
# =============================================================================

# Groups present in commercial data that are not GB Rpath model groups,
# are duplicates from lookup name variants, or have too few years to use.
# OffHake and OtherFlatfish are lookup gaps — NESPP3 codes in the landings
# database not caught by the case_when block; retained here as a safety net.
groups_to_remove <- c(
  "Fauna",         # not a GB Rpath group — catch-all survey code
  "Freshwater",    # not a GB Rpath group
  "LargePelagics", # maps to HMS which is already included
  "Rays",          # not modelled separately in GB Rpath
  "RedDrum",       # southern species, too few years
  "OtherShrimp",   # duplicate of OtherShrimps — lookup name mismatch
  "OffHake",       # lookup gap — should map to SilverHake
  "OtherFlatfish", # lookup gap — should map to OtherDemersals
  "SmFlatfishes",  # 1 year only, not reliable
  "SmallPelagics", # duplicate of SmPelagics — lookup name variant
  "Sturgeon",      # not a GB Rpath model group
  "Macrobenthos"   # not a commercial fishery target
)

take_index <- landings_index |>
  full_join(
    discards_index |> select(RPATH, YEAR, discards_tkm2),
    by = c("RPATH", "YEAR")
  ) |>
  mutate(
    landings_tkm2 = replace_na(landings_tkm2, 0),
    discards_tkm2 = replace_na(discards_tkm2, 0),
    take_tkm2     = landings_tkm2 + discards_tkm2
  ) |>
  filter(take_tkm2 > 0) |>
  group_by(RPATH, YEAR) |>
  summarise(take_tkm2 = sum(take_tkm2, na.rm = TRUE), .groups = "drop") |>
  mutate(take_sd_tkm2 = take_tkm2 * 0.10) |>   # 10% CV uncertainty
  filter(!RPATH %in% groups_to_remove)

# =============================================================================
# 10. Format output
#     Required columns for read.fitting.catch() in merge_ecofitting.R:
#     Group, Year, Value, Stdev, Scale
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
# 11. Diagnostics
# =============================================================================

message("\n--- Output diagnostics ---")
message(sprintf("Species in output : %d", n_distinct(take_out$Group)))
message(sprintf("Total records     : %d", nrow(take_out)))
message(sprintf("Year range        : %d - %d",
                min(take_out$Year), max(take_out$Year)))

bad_rows <- take_out |>
  filter(Value <= 0 | Stdev <= 0 | is.na(Value) | is.na(Stdev))
if (nrow(bad_rows) > 0) {
  warning(sprintf("%d rows with zero/negative/NA Value or Stdev:", nrow(bad_rows)))
  print(bad_rows)
} else {
  message("All Value and Stdev entries are positive and non-NA.")
}

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

# Flag species known to cause biomass crashes under forced catch in GB_01
unstable_flag <- c(
  "Redfish", "WitchFlounder", "SummerFlounder",
  "Goosefish", "Fourspot", "SouthernDemersals", "AtlMackerel"
)
flagged <- take_out |>
  filter(Group %in% unstable_flag) |>
  group_by(Group) |>
  summarise(n_years = n(), mean_take = round(mean(Value), 6), .groups = "drop")
if (nrow(flagged) > 0) {
  message("\nPreviously unstable species present (review in GB_01_build_scenarios.R):")
  print(flagged)
}

# =============================================================================
# 12. Save output and metadata
# =============================================================================

write_csv(take_out, here("data-raw", "take_fit.csv"))
message(sprintf("\nSaved %d records to data-raw/take_fit.csv", nrow(take_out)))
message("Next step: copy data-raw/take_fit.csv to Rpath_fitting/GB/take_fit.csv")

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

message("Metadata saved to data-raw/take_fit_metadata.csv")
