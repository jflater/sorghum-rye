library(tidyverse)
library(janitor)


leaching_data <- read_csv("data/2023_2024_SABR_tile_cumulative_n.csv") %>%
  clean_names() %>%
  mutate(
    date = as.Date(date),
    year = year(date)
  ) %>%
  filter(year %in% c(2023, 2024)) %>% # Filter out bad years
  filter(!is.na(year))

str(leaching_data)

# Remove columns treatment and growing_season
leaching_data <- leaching_data %>%
  select(-treatment, -growing_season, -year_date, -cumulative_n_loss_mg) %>%
  mutate(plot = str_pad(plot, width = 2, side = "left", pad = "0"))

str(leaching_data)
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

df <- leaching_data %>%
  mutate(
    year = year(date)
  ) %>%
  left_join(trt_year, by = c("plot", "year"))

# Recalculate total N loss using existing nitrate and ammonia loss columns
df <- df %>%
  mutate(
    total_n_loss_mg = nitrate_loss_mg + ammonia_loss_mg
  )

# Calculate cumulative flow and N loss per plot per year
leaching_data <- df %>%
  group_by(plot, treatment, year) %>%
  arrange(date) %>%
  mutate(
    cumulative_flow_l = cumsum(flow_l),
    cumulative_n_loss_mg = cumsum(replace_na(total_n_loss_mg, 0))
  ) %>%
  ungroup()

# Save cleaned data
write_csv(leaching_data, "data/clean_leaching.csv")
