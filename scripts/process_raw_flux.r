library("tidyverse")
library("readr")
library("dplyr")
library("purrr")
library("janitor")

# Path to .csv files exported from LI-COR software
data_path <- "sorghum-rye/data/processed/"

# List all .csv files in the data_path directory
file_list <- list.files(path = data_path, pattern = "*.csv", full.names = TRUE)
file_list

# Read in all .csv files, skipping the first row and merging the second and third rows as the header
custom_read_csv <- function(file) {
  # Read first three rows
  header_rows <- readr::read_csv(file, n_max = 3, col_names = FALSE, show_col_types = FALSE)
  # Skip first row, merge row 2 and 3 with _
  merged_names <- paste(header_rows[2, ], header_rows[3, ], sep = "_")
  # Read the rest of the data, skipping first 3 rows, and set col_names
  readr::read_csv(file, skip = 3, col_names = merged_names, show_col_types = FALSE)
}

flux_data <- map(file_list, custom_read_csv)

head(flux_data[[1]])

data <- bind_rows(flux_data) %>%
  clean_names()

# Separate label_number into plot and location on "_" or "-"
data <- data %>%
  separate(label_number, into = c("plot", "location"), sep = "[-_]") %>%
  mutate(date = as.Date(date_time_initial_value_yyyy_mm_dd_hh_mm_ss,
                        format = "%Y/%m/%d"),
         plot = as.factor(plot))

# Here is the processed data, before proceding, check if any dates already exist in processed data
#sorghum-rye/data/processed/processed_flux_data.csv

already_done <- read_csv("sorghum-rye/data/processed_flux_data.csv",
                        show_col_types = FALSE) %>%
                        mutate(plot = as.factor(plot))

# Remove already processed dates from current data
data <- data %>%
  filter(!(plot %in% already_done$plot & date %in% already_done$date))

# Remove leading zeros from plot
data <- data %>%
    mutate(plot = as.factor(sub("^0+", "", as.character(plot))))

# metadata
metadata <- read_csv("sorghum-rye/data/metadata/plot_treatments.csv",
                     show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(plot = as.factor(plot))

compare_df_cols_same(data, metadata)
compare_df_cols(data, metadata)

# Join metadata to flux data
data <- left_join(data, metadata, by = c("plot", "date"))

# Select the best flux column based on r2 values
data <- data %>%
  mutate(selected_flux = if_else(
    fn2o_dry_r2_number >= fn2o_dry_lin_r2_number,
    fn2o_dry_nmol_1m_2s_1,
    fn2o_dry_lin_nmol_1m_2s_1)
    )

# Convert flux to g n ha per day
nmols_to_grams_hectare_day <- function(nmols) {
  mols <- nmols / 1e9
  grams_n <- mols * 28.0134
  grams_per_m2_per_day <- grams_n * 86400
  grams_per_hectare_per_day <- grams_per_m2_per_day * 10000
  return(grams_per_hectare_per_day)
}

data <- data %>%
  mutate(flux_g_n_ha_day = nmols_to_grams_hectare_day(selected_flux))

# append to processed data and save as new processed data file
updated <- bind_rows(already_done, data)

# Save processed data, add date to filename
write_csv(updated, paste0("sorghum-rye/data/processed_flux_data_",
                           Sys.Date(), ".csv"))
write_csv(updated, "sorghum-rye/data/processed_flux_data.csv")

# Move input files to archive
file.rename(from = file_list, to = file.path("sorghum-rye/data/archive", basename(file_list)))
