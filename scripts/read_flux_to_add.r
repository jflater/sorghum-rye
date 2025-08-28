library(readr)
library(dplyr)
library(purrr)
library(tidyverse)
library(janitor)

source("scripts/myfunctions.r")

# These were found to be missing in seasonal_flux_combined.
# I processed them separately.
flux_folder <- "data/flux_to_add_to_analysis"

file_list <- list.files(flux_folder, pattern = "*.csv", full.names = TRUE)

read_flux_file <- function(file_path) {
  # Read the header from row 2 (skip first row, read headers from row 2,
  # then skip row 3 with units)
  col_names <- read_csv(
    file_path,
    skip = 1,
    n_max = 1,
    col_names = FALSE,
    show_col_types = FALSE
  )
  col_names <- as.character(col_names[1, ])

  # Read the actual data starting from row 4 (skip first 3 rows)
  # with proper column names
  data <- read_csv(
    file_path,
    skip = 3,
    col_names = col_names,
    show_col_types = FALSE
  )
  data$source_file <- basename(file_path)
  data
}

flux_data <- map_dfr(file_list, read_flux_file)

print(paste("Read", nrow(flux_data), "rows from", length(file_list), "files"))
print("Files processed:")
print(basename(file_list))

# Remove rows that contain 'f' or 'F' in the LABEL column
if ("LABEL" %in% colnames(flux_data)) {
  flux_data <- flux_data %>% filter(!grepl("[fF]", LABEL, ignore.case = FALSE))
}

# separate the LABEL column on _ or - into plot and location using separate

flux_data <- flux_data %>%
  separate(LABEL, into = c("plot", "location"), sep = "[-_]", extra = "warn")

colnames(flux_data)

# We want leading zeros on plot if it is a single digit
flux_data <- flux_data %>%
  mutate(plot = sprintf("%02d", as.numeric(plot))) %>%
  clean_names() %>%
  select(-longitude, -latitude, -remark, -source_file)

unique(flux_data$plot)

colnames(flux_data)

# get best flux and convert to gnha
flux_data <- flux_data %>%
  mutate(
    year_month_day = as.Date(date_time_initial_value),
    # Calculate best flux
    best_flux_nmol_1m_2s_1 = ifelse(
      fn2o_dry_lin_r2 > fn2o_dry_r2,
      fn2o_dry_lin, fn2o_dry
    ),
    gnha_day = nmols_to_grams_hectare_day(as.numeric(best_flux_nmol_1m_2s_1)),
    gnha_day_no_negative = ifelse(gnha_day < 0, 0, gnha_day)
  ) %>%
  # Classify location into categories more robustly
  mutate(RowvsInterrow = case_when(
    str_detect(location, "^[iIeE]$|^Interrow$") ~ "Interrow",
    str_detect(location, "^[rRwW]+?$|^Row$") ~ "Row",
    str_detect(location, "[fF]$|^Fertilizer_band$") ~ "Fertilizer_band",
    location == "" ~ "No Location",
    TRUE ~ "Unclassified"
  )) %>%
  select(
    year_month_day,
    plot,
    RowvsInterrow,
    best_flux_nmol_1m_2s_1,
    ts_2_mean,
    swc_2_mean,
    gnha_day,
    gnha_day_no_negative
  )

# read in seasonal flux
old_flux <- read_csv(
  "data/seasonal_flux_combined.csv",
  show_col_types = FALSE
)

seasonal_flux <- old_flux %>%
  select(colnames(flux_data)) %>%
  filter(RowvsInterrow != "Fertilizer_band")

compare_df_cols(seasonal_flux, flux_data)

# combine the two dataframes
combined_flux <- bind_rows(seasonal_flux, flux_data)

# how many observations for each combination of year, plot, and row
observation_counts <- combined_flux %>%
  group_by(year_month_day, plot, RowvsInterrow) %>%
  summarise(observations = n(), .groups = "drop") %>%
  # if more than 1 observation, note that row
  mutate(row_note = ifelse(observations > 1, "Multiple observations", "Single observation")) %>%
  filter(row_note == "Multiple observations")

matched_rows <- merge(old_flux, observation_counts[, 1:3], by = names(observation_counts)[1:3]) %>%
  # add unique row marker
  mutate(row_id = row_number())

unique(matched_rows$year_month_day)

# I looked at the raw .json files to choose which duplicate readings are to be kept,
# the following list are those rows to REMOVE, i removed the row with the lower
# rsquared value
remove_rows <- c(2, 5, 7, 10, 11, 13, 16, 19, 20, 23)
# change row 3 to plot 10
matched_rows$plot[matched_rows$row_id == 3] <- 10
# change row 18 to RowvsInterrow row
matched_rows$RowvsInterrow[matched_rows$row_id == 18] <- "Row"
