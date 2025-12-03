library(readxl)
library(tidyverse)
library(janitor)
# Read in the two soil data files
# pH and GWC might be hidden in other sheets
soil_2023 <- read_xlsx("data/soils/2023_SABR_MASTER_soil.xlsx") %>%
  clean_names()
soil_2024 <- read_xlsx("data/soils/2024_SABR_MASTER_soil.xlsx") %>%
  clean_names()

# 23 pH and GWC
gwc <- read_xlsx("data/soils/2023_SABR_MASTER_soil.xlsx", sheet = "GWC") %>%
  clean_names()

# Join gwc to soil_2023
soil_2023 <- soil_2023 %>%
  left_join(
    gwc,
    by = c("project_sampling", "plot_id")
  )

soil_2023 <- soil_2023 %>%
  select(-contains("...")) %>%
  filter(
    str_detect(project_sampling, str_c("12-2023|Spring 2023")) &
      !str_detect(plot_id, str_c("NE|SE|SW|NW"))
  ) %>%
  mutate(plot = str_pad(plot_id, width = 2, side = "left", pad = "0"))

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
    nitrate_ppm = as.numeric(nitrate_ppm),
    gwc_g_g = as.numeric(gwc_g_g),
    extraction_wt_g = as.numeric(extraction_wght)
  ) %>%
  select(date, plot, ammonia_ppm, nitrate_ppm, gwc_g_g, extraction_wt_g)

# Soil collection dates
# There is a September date that need processed
date_12_2023 <- as.Date("2023-12-14")
date_spring_2023 <- as.Date("2023-05-18")

# add date column with Soil collection dates
soil_2023 <- soil_2023 %>%
  mutate(
    date = case_when(
      str_detect(project_sampling, "12-2023") ~ date_12_2023,
      str_detect(project_sampling, "Spring 2023") ~ date_spring_2023
    ),
    ammonia_ppm = as.numeric(ammonia_ppm),
    nitrate_ppm = as.numeric(nitrate_ppm),
    gwc_g_g = as.numeric(gwc_g_g),
    extraction_wt_g = as.numeric(extraction_wt_g)
  ) %>%
  select(date, plot, ammonia_ppm, nitrate_ppm, gwc_g_g, extraction_wt_g)

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

df <- soil_combined %>%
  mutate(
    year = year(date),
    gwc_g_g = as.numeric(gwc_g_g) # Add this line
  ) %>%
  left_join(trt_year, by = c("plot", "year"))
glimpse(df)

# replace missing gwc values with mean
df <- df %>%
  group_by(plot) %>%
  mutate(
    gwc_g_g = ifelse(
      is.na(gwc_g_g),
      mean(gwc_g_g, na.rm = TRUE),
      gwc_g_g
    )
  ) %>%
  ungroup()

# a function to calculate conc in mg/kg of dry soil
df <- df %>%
  mutate(
    ammonia_mg_per_kg = (ammonia_ppm * .025) / ((extraction_wt_g - (extraction_wt_g * gwc_g_g)) / 1000),
    nitrate_mg_per_kg = (nitrate_ppm * .025) / ((extraction_wt_g - (extraction_wt_g * gwc_g_g)) / 1000)
  )

# Write cleaned soil data to CSV
write_csv(df, "data/soils/cleaned_soil_data.csv")

# A boxplot of gwc_g_g by plot (combining all dates)
ggplot(df, aes(x = as.factor(plot), y = gwc_g_g)) +
  geom_boxplot() +
  labs(
    x = "Plot",
    y = "Gravimetric Water Content (g/g)"
  ) +
  theme_minimal()

ggplot(df, aes(x = as.factor(plot), y = gwc_g_g)) +
  geom_boxplot() +
  labs(
    x = "Plot",
    y = "Gravimetric Water Content (g/g)"
  ) +
  theme_minimal() +
  facet_wrap(~year)
