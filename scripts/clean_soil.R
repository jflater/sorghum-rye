library(readxl)
library(tidyverse)
library(janitor)
# Read in the two soil data files
# pH and GWC might be hidden in other sheets
soil_2023 <- read_xlsx("data/soils/2023_SABR_MASTER_soil.xlsx")
soil_2024 <- read_xlsx("data/soils/2024_SABR_MASTER_soil.xlsx")

soil_2023 <- soil_2023 %>%
  select(-contains("...")) %>%
  filter(
    str_detect(`Project/sampling`, str_c("12-2023|Spring 2023")) &
      !str_detect(`Plot ID`, str_c("NE|SE|SW|NW"))
  ) %>%
  rename(
    plot = `Plot ID`,
    nitrate_ppm = `Nitrate-ppm`,
    ammonia_ppm = `Ammonia-ppm`
  ) %>%
  mutate(plot = str_pad(plot, width = 2, side = "left", pad = "0"))

soil_2024 <- soil_2024 %>%
  filter(
    str_detect(lab_id, "2024") &
      !str_detect(lab_id, str_c("NE|SE|SW|NW"))
  ) %>%
  separate(
    lab_id,
    into = c("date", "plot"),
    sep = "_",
    extra = "merge"
  ) %>%
  mutate(
    plot = str_pad(plot, width = 2, side = "left", pad = "0"),
    date = lubridate::ymd(date), # Use mdy() for MM/DD/YYYY or similar formats
    ammonia_ppm = as.numeric(ammonia_ppm),
    nitrate_ppm = as.numeric(nitrate_ppm)
  ) %>%
  select(date, plot, ammonia_ppm, nitrate_ppm)

# Soil collection dates
# There is a September date that need processed
date_12_2023 <- as.Date("2023-12-14")
date_spring_2023 <- as.Date("2023-05-18")

# add date column with Soil collection dates
soil_2023 <- soil_2023 %>%
  mutate(
    date = case_when(
      str_detect(`Project/sampling`, "12-2023") ~ date_12_2023,
      str_detect(`Project/sampling`, "Spring 2023") ~ date_spring_2023
    ),
    ammonia_ppm = as.numeric(ammonia_ppm),
    nitrate_ppm = as.numeric(nitrate_ppm)
  ) %>%
  select(date, plot, ammonia_ppm, nitrate_ppm)

# Compare df columns
compare_df_cols(soil_2023, soil_2024)

# Join the two data frames
soil_combined <- bind_rows(soil_2023, soil_2024) %>%
  arrange(date, plot)

# Add treatment information
trt <- read_csv("data/metadata/plot_treatments.csv") %>%
  clean_names() %>%
  # add leading zero to plot numbers
  mutate(plot = str_pad(plot, width = 2, side = "left", pad = "0")) %>%
  select(!growing_season)

trt_2023 <- trt %>%
  filter(date == "2023-07-01")

trt_2024 <- trt %>%
  filter(date == "2024-07-01")

trt_year <- bind_rows(
  trt_2023 %>% mutate(year = 2023),
  trt_2024 %>% mutate(year = 2024)
) %>%
  select(plot, year, treatment)

df <- soil_combined %>%
  mutate(
    year = year(date)
  ) %>%
  left_join(trt_year, by = c("plot", "year"))

# Write cleaned soil data to CSV
write_csv(df, "data/soils/cleaned_soil_data.csv")
