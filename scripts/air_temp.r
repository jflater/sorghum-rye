library(readxl)
library(tidyverse)
library(lubridate)

gdd <- read_xlsx("data/rain/historical_rain.xlsx")

gdd <- gdd %>%
  select(day, doy, gdd_50_86) %>%
  mutate(
    date = as.Date(day),
    year = year(date),
  ) %>%
  group_by(year) %>%
  arrange(date) %>%
  mutate(cumulative_gdd = cumsum(gdd_50_86))

gdd <- gdd %>%
  mutate(doy_date = as.Date(doy - 1, origin = "2000-01-01"))

gdd %>%
  ggplot(aes(x = doy_date, y = cumulative_gdd, group = year)) +
  # All years gray
  geom_line(
    data = filter(gdd, !(year %in% c(2023, 2024))),
    color = "gray",
    size = 0.8,
    alpha = .5
  ) +
  # 2023 and 2024 colored
  geom_line(data = filter(gdd, year == 2023), color = "#E41A1C", size = 1.2) +
  geom_line(data = filter(gdd, year == 2024), color = "#377EB8", size = 1.2) +
  scale_x_date(
    date_breaks = "1 month", # Breaks at each month
    date_labels = "%b" # Label as abbreviated month names
  ) +
  labs(
    x = "Month",
    y = "Growing Degree Days (GDD)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

mean_cum <- gdd %>%
  group_by(doy_date) %>%
  summarize(cumulative_gdd = mean(cumulative_gdd, na.rm = TRUE)) %>%
  mutate(year = "Mean") %>%
  filter(doy_date != "2000-12-31")

# Combine all into one data frame for plotting
gdd_long <- gdd %>%
  filter(year %in% c(2023, 2024)) %>% # Only plot 2023 and 2024
  mutate(year = as.character(year)) %>%
  bind_rows(mean_cum) # add the mean as its own 'year'

# For all OTHER years, add a gray background
background <- gdd %>%
  filter(!(year %in% c(2023, 2024)))

gdd_plot <- ggplot() +
  # Gray lines for all other years
  geom_line(
    data = background,
    aes(x = doy_date, y = cumulative_gdd, group = year),
    color = "gray", size = 0.8, alpha = 0.6
  ) +
  # Colored lines for 2023, 2024, and mean
  geom_line(
    data = gdd_long,
    aes(x = doy_date, y = cumulative_gdd, color = year, group = year),
    size = 1.2
  ) +
  scale_color_manual(
    name = "Year",
    values = c("2023" = "#E41A1C", "2024" = "#377EB8", "Mean" = "black"),
    labels = c("2023", "2024", "Mean (All Years)")
  ) +
  scale_x_date(
    date_breaks = "1 month", # Breaks at each month
    date_labels = "%b" # Label as abbreviated month names
  ) +
  labs(
    x = "Day of Year",
    y = "Cumulative Growing Degree Days (GDD)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

gdd_plot

#### Version 2
# Exclude current years from the envelope
hist_env <- gdd %>%
  filter(!(year %in% c(2023, 2024))) %>%
  group_by(doy_date) %>%
  summarize(
    min_cum = min(cumulative_gdd, na.rm = TRUE),
    max_cum = max(cumulative_gdd, na.rm = TRUE)
  )

gdd_2023 <- filter(gdd, year == 2023)
gdd_2024 <- filter(gdd, year == 2024)

ggplot() +
  # Shaded region for historical envelope
  geom_ribbon(
    data = hist_env,
    aes(x = doy_date, ymin = min_cum, ymax = max_cum),
    fill = "gray80", alpha = 0.6
  ) +
  # 2023 solid line
  geom_line(
    data = gdd_2023,
    aes(x = doy_date, y = cumulative_gdd),
    color = "#E41A1C", size = 1.2, linetype = "solid"
  ) +
  # 2024 dashed line
  geom_line(
    data = gdd_2024,
    aes(x = doy_date, y = cumulative_gdd),
    color = "#377EB8", size = 1.2, linetype = "dashed"
  ) +
  scale_x_date(
    date_breaks = "1 month", # Breaks at each month
    date_labels = "%b" # Label as abbreviated month names
  ) +
  labs(
    title = "Cumulative Rainfall: Historical Range vs. Recent Years",
    x = "Day of Year",
    y = "Cumulative Precipitation (mm)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Custom legend via manual color/linetype mapping:
  scale_color_manual(
    name = "Year",
    values = c("2023" = "#E41A1C", "2024" = "#377EB8")
  ) +
  scale_linetype_manual(
    name = "Year",
    values = c("2023" = "solid", "2024" = "dashed")
  )

gdd_highlight <- gdd %>%
  filter(year %in% c(2023, 2024)) %>%
  mutate(year = as.character(year))

ggplot() +
  geom_ribbon(
    data = hist_env,
    aes(x = doy_date, ymin = min_cum, ymax = max_cum),
    fill = "gray80", alpha = 0.6
  ) +
  geom_line(
    data = gdd_highlight,
    aes(x = doy_date, y = cumulative_gdd, color = year, linetype = year),
    size = 1.2
  ) +
  scale_color_manual(
    name = "Year",
    values = c("2023" = "#E41A1C", "2024" = "#377EB8"),
    labels = c("2023", "2024")
  ) +
  scale_linetype_manual(
    name = "Year",
    values = c("2023" = "solid", "2024" = "dashed"),
    labels = c("2023", "2024")
  ) +
  scale_x_date(
    date_breaks = "1 month", # Breaks at each month
    date_labels = "%b" # Label as abbreviated month names
  ) +
  labs(
    title = "Cumulative Rainfall: Historical Range vs. Recent Years",
    x = "Day of Year",
    y = "Cumulative Growing Degree Days (GDD)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

mean_cum <- gdd %>%
  filter(!(year %in% c(2023, 2024))) %>% # use only historical for mean
  group_by(doy_date) %>%
  summarize(cumulative_gdd = mean(cumulative_gdd, na.rm = TRUE)) %>%
  mutate(year = "Mean") %>%
  filter(doy_date != "2000-12-31")

gdd_highlight <- gdd %>%
  filter(year %in% c(2023, 2024)) %>%
  mutate(year = as.character(year)) %>%
  bind_rows(mean_cum)

gdd_plot_v2 <- ggplot() +
  geom_ribbon(
    data = hist_env,
    aes(x = doy_date, ymin = min_cum, ymax = max_cum),
    fill = "gray80", alpha = 0.6
  ) +
  geom_line(
    data = gdd_highlight,
    aes(x = doy_date, y = cumulative_gdd, color = year, linetype = year),
    size = 1.2
  ) +
  scale_color_manual(
    name = "Year",
    values = c(
      "2023" = "#E41A1C",
      "2024" = "#377EB8",
      "Mean" = "black"
    ),
    labels = c(
      "2023" = "2023",
      "2024" = "2024",
      "Mean" = "Mean (Historical)"
    )
  ) +
  scale_linetype_manual(
    name = "Year",
    values = c(
      "2023" = "solid",
      "2024" = "dashed",
      "Mean" = "dotdash"
    ),
    labels = c(
      "2023" = "2023",
      "2024" = "2024",
      "Mean" = "Mean (Historical)"
    )
  ) +
  scale_x_date(
    date_breaks = "1 month", # Breaks at each month
    date_labels = "%b" # Label as abbreviated month names
  ) +
  labs(
    x = "Day of Year",
    y = "Cumulative Growing Degree Days (GDD)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

gdd_plot_v2 <- gdd_plot_v2 + guides(color = "none", fill = "none", linetype = "none", shape = "none")

pdplot <- rain_plot_v2 | gdd_plot_v2 +
  plot_layout(guides = "collect") +
  plot_annotation(
    # <- this theme applies to the *patchwork container* (incl. collected legend)
    theme = theme_sabr() + theme(
      legend.position = "bottom",
      legend.justification = "center",
      legend.direction = "horizontal",
      legend.box.just = "center",
      legend.box.margin = margin(t = 6)
    )
  )

ggsave(
  "figures/gdd_plot_v2.png",
  pdplot,
  width = 8.5, height = 11, dpi = 350
)
