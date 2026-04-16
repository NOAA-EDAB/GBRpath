# =============================================================================
# Title: Compare Raw survdat Pulls (Old vs. New)
# Purpose: Identify differences in raw tow-level data between the 2021 .RData 
#          pull and the fresh 2026 database pull.
# =============================================================================

library(tidyverse)
library(here)

# 1. Load the Data
# -----------------------------------------------------------------------------
# Load old data (creates an object in your environment, likely called 'survey')
load(here('data', 'NEFSC_BTS_2021_all_seasons.RData'))

# Extract the raw dataframe
old_raw <- as_tibble(survey$survdat)

# pull new data as is done in GB_rsim_biomass_pull.R
channel <- dbutils::connect_to_database("NEFSC_pw_oraprod", "MGREZLIK")

trawl_list <- survdat::get_survdat_data(
  channel,
  getLengths        = FALSE,
  conversion.factor = TRUE
)
trawl_raw <- trawl_list$survdat
new_raw <- as_tibble(trawl_raw) 

# 2. High-Level Summary Checks
# -----------------------------------------------------------------------------
message("--- High Level Summaries ---")
message("Old Data Rows: ", nrow(old_raw), " | Cols: ", ncol(old_raw))
message("New Data Rows: ", nrow(new_raw), " | Cols: ", ncol(new_raw))
message("Old Year Range: ", min(old_raw$YEAR, na.rm = TRUE), "-", max(old_raw$YEAR, na.rm = TRUE))
message("New Year Range: ", min(new_raw$YEAR, na.rm = TRUE), "-", max(new_raw$YEAR, na.rm = TRUE))

# Check if column names match
missing_in_new <- setdiff(names(old_raw), names(new_raw))
missing_in_old <- setdiff(names(new_raw), names(old_raw))

if(length(missing_in_new) > 0) message("Columns in Old but missing in New: ", paste(missing_in_new, collapse = ", "))
if(length(missing_in_old) > 0) message("Columns in New but missing in Old: ", paste(missing_in_old, collapse = ", "))

# 3. Aggregate for Direct Comparison
# -----------------------------------------------------------------------------
# To compare raw data, we should aggregate the total biomass caught per year, 
# season, and species. This smooths out tow-level ID mismatches but catches
# differences in vessel calibrations or missing cruises.

# Correctly aggregate the old length-expanded data by isolating unique tows first
old_agg <- old_raw |>
  filter(YEAR >= 1985 & YEAR <= 2021) |>
  # This drops the length columns and keeps only 1 row per tow/species
  distinct(YEAR, SEASON, CRUISE6, STATION, SVSPP, BIOMASS) |> 
  group_by(YEAR, SEASON, SVSPP) |>
  summarise(Old_Total_B = sum(BIOMASS, na.rm = TRUE),
            Old_Tows = n(), 
            .groups = "drop")

new_agg <- new_raw |>
  filter(YEAR >= 1985 & YEAR <= 2021) |>
  group_by(YEAR, SEASON, SVSPP) |>
  summarise(New_Total_B = sum(BIOMASS, na.rm = TRUE),
            New_Tows = n_distinct(CRUISE6, STATION), 
            .groups = "drop")

# Join them together
compare_raw <- full_join(old_agg, new_agg, by = c("YEAR", "SEASON", "SVSPP")) |>
  mutate(
    Old_Total_B = replace_na(Old_Total_B, 0),
    New_Total_B = replace_na(New_Total_B, 0),
    B_Diff_Absolute = New_Total_B - Old_Total_B,
    B_Diff_Percent = if_else(Old_Total_B > 0, (B_Diff_Absolute / Old_Total_B) * 100, NA_real_),
    Tow_Diff = replace_na(New_Tows, 0) - replace_na(Old_Tows, 0)
  )

# 4. Investigate Discrepancies
# -----------------------------------------------------------------------------
# Flag major discrepancies (e.g., more than a 5% difference in raw biomass)
major_diffs <- compare_raw |>
  filter(abs(B_Diff_Percent) > 5 & Old_Total_B > 10) |> # Filter out tiny catches where small diffs = huge %
  arrange(desc(abs(B_Diff_Percent)))

message("\n--- Major Discrepancies Detected ---")
message("Found ", nrow(major_diffs), " instances where aggregated biomass differs by > 5%.")

# View the worst offenders
head(major_diffs, 10)

# 5. Visualizing the Raw Differences (Example: Atlantic Cod = SVSPP 73)
# -----------------------------------------------------------------------------
# Let's plot Fall Cod to see if the differences are structural (calibration changes)
# or just random noise.

plot_cod <- compare_raw |>
  filter(SVSPP == 73, SEASON == "FALL") |>
  select(YEAR, Old_Total_B, New_Total_B) |>
  pivot_longer(cols = c(Old_Total_B, New_Total_B), names_to = "Source", values_to = "Raw_Biomass")

ggplot(plot_cod, aes(x = YEAR, y = Raw_Biomass, color = Source)) +
  geom_line(size = 1) +
  geom_point() +
  theme_minimal() +
  labs(title = "Raw Fall Catch Comparison: Atlantic Cod (SVSPP 73)",
       subtitle = "Total BIOMASS across all tows (Not stratified, no area expansions)",
       y = "Total Raw Biomass Caught (kg)",
       x = "Year") +
  scale_color_manual(values = c("New_Total_B" = "blue", "Old_Total_B" = "red"),
                     labels = c("New (2026 Pull)", "Old (2021 File)"))




# 6. Join Species Names (RPATH groups)
# -----------------------------------------------------------------------------
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


compare_mapped <- compare_raw |>
  inner_join(spp |> select(SVSPP, RPATH) |> distinct(), by = "SVSPP") |>
  # Pivot for ggplot
  pivot_longer(cols = c(Old_Total_B, New_Total_B), 
               names_to = "Source", 
               values_to = "Raw_Biomass") |>
  # Clean up labels for the plot legend
  mutate(Source = if_else(Source == "New_Total_B", "New (2026 Pull)", "Old (2021 Pull)"))

# Identify all unique RPATH groups that have data
plot_groups <- sort(unique(compare_mapped$RPATH))
message("Generating plots for ", length(plot_groups), " RPATH groups...")

# 4. Generate Multi-page PDF
# -----------------------------------------------------------------------------
pdf_path <- file.path(out_dir, "Raw_Fall_Biomass_Comparison.pdf")
pdf(pdf_path, width = 11, height = 8.5) # Standard landscape letter size

# Loop through species in chunks of 4 (for a 2x2 grid per page)
for(i in seq(1, length(plot_groups), by = 4)) {
  
  # Get the 4 species for this specific page
  current_spp <- plot_groups[i:min(i + 3, length(plot_groups))]
  
  # Filter data for current chunk
  plot_data <- compare_mapped |>
    filter(RPATH %in% current_spp)
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = YEAR, y = Raw_Biomass, color = Source)) +
    geom_line(linewidth = 1) +
    geom_point(alpha = 0.7) +
    facet_wrap(~ RPATH, scales = "free_y", ncol = 2, nrow = 2) +
    scale_color_manual(values = c("New (2026 Pull)" = "blue", "Old (2021 Pull)" = "red")) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold")
    ) +
    labs(
      title = paste("Raw Fall Catch Biomass Comparison (Page", ceiling(i/4), "of", ceiling(length(plot_groups)/4), ")"),
      subtitle = "Comparing unexpanded sum of BIOMASS (kg) across all tows",
      x = "Year",
      y = "Raw Catch Weight (kg)",
      color = NULL
    )
  
  # Print the plot to the active PDF device
  print(p)
}

# Close the PDF device to save the file
dev.off()

message("PDF successfully saved to: ", pdf_path)
