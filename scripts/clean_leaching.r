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

## Weekly heatmap: two rows per plot (Sample, Flow) without overlap
wk <- leaching_data %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(plot, treatment, year, week) %>%
  summarise(flow = sum(flow_l, na.rm = TRUE), sample = any(sample_y_n == "Y", na.rm = TRUE), .groups = "drop") %>%
  pivot_longer(c(sample, flow), names_to = "measure", values_to = "value") %>%
  mutate(plot_row = paste(plot, if_else(measure == "sample", "Sample", "Flow")))

ggplot() +
  geom_tile(
    data = ~ dplyr::filter(wk, measure == "flow"),
    aes(x = week, y = plot_row, fill = value), color = NA
  ) +
  geom_tile(
    data = ~ dplyr::filter(wk, measure == "sample" & value),
    aes(x = week, y = plot_row), fill = "#1100ff", color = NA
  ) +
  facet_grid(treatment ~ year, scales = "free") +
  # 0 flow = red; any positive flow transitions from light to dark green
  scale_fill_gradientn(
    colors = c("red", "#BDECB6", "#006400"),
    values = c(0, 1e-6, 1),
    name = "Weekly flow (L)"
  ) +
  theme_minimal() +
  labs(x = "Week", y = "Plot") +
  theme_publication()
ggsave("figures/supp2_weekly_sample_flow_heatmap.png", width = 12, height = 8, dpi = 300)
