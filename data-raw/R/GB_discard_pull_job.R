# Pulling discards takes a long time and connection to the database
# fails if the laptop goes to sleep. Pulling discards as a job
# to work around this issue. -MTG 04/03/2026

# Submit this as a background job from RStudio:
# Tools -> Background Jobs -> Start Background Job -> select this file

library(comlandr)
library(survdat)
library(tidyverse)
library(sf)
library(here)
library(DBI)
library(dbutils)

# Load the landings objects saved from the completed landings pull
landings_list    <- readRDS(here("data-raw", "landings_list.rds"))
landings_list_gb <- readRDS(here("data-raw", "landings_list_gb.rds"))
gb_area          <- readRDS(here("data-raw", "gb_area.rds"))

spp <- read_csv(here("data-raw", "Species_codes.csv"), show_col_types = FALSE) |>
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
    NESPP3 == 117 ~ "OtherDemersals",
    NESPP3 == 333 ~ "OtherDemersals",
    NESPP3 == 363 ~ "SmPelagics",
    NESPP3 == 431 ~ "SouthernDemersals",
    NESPP3 == 456 ~ "SouthernDemersals",
    TRUE          ~ RPATH
  )) |>
  filter(!is.na(NESPP3)) |>
  select(NESPP3, RPATH) |>
  distinct() |>
  group_by(NESPP3) |>
  slice(1) |>
  ungroup()

channel <- dbutils::connect_to_database("NEFSC_pw_oraprod", "MGREZLIK")

message(sprintf("[%s] Starting discard pull...", Sys.time()))

discards_list <- comlandr::get_comdisc_data(
  channel,
  landings_list_gb,
  aggArea = TRUE,
  aggGear = FALSE
)

message(sprintf("[%s] Discard pull complete.", Sys.time()))

dbDisconnect(channel)

discards_raw <- discards_list$comdisc

discards_index <- discards_raw |>
  as_tibble() |>
  filter(EPU == "GB") |>
  left_join(spp, by = "NESPP3") |>
  filter(!is.na(RPATH), !is.na(DISMT)) |>
  group_by(RPATH, YEAR) |>
  summarise(discards_mt = sum(DISMT, na.rm = TRUE), .groups = "drop") |>
  mutate(discards_tkm2 = discards_mt / gb_area) |>
  filter(YEAR >= 1985)

saveRDS(discards_index, here("data-raw", "discards_index.rds"))
message(sprintf("[%s] discards_index saved to data-raw/discards_index.rds", 
                Sys.time()))