# Precipitation and GDD plots with historical envelopes

# Custom theme for consistency
theme_publication <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.background = element_rect(fill = "#ffffffff", color = NA),
      plot.background = element_rect(fill = "#ffffffff", color = NA),
      panel.grid.major = element_line(color = alpha("#B7B5B3", 0.2)),
      panel.grid.minor = element_blank(),
      text = element_text(color = "#000000ff"),
      axis.text = element_text(color = "#000000ff"),
      strip.background = element_rect(fill = alpha("#A5CAD2", 0.2)),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.spacing = unit(2, "lines")
    )
}
# Import historical rain and gdd data
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

rain <- read_xlsx("data/rain/historical_rain.xlsx")

rain <- rain %>%
  select(day, doy, precipmm) %>%
  mutate(
    date = as.Date(day),
    year = year(date),
  ) %>%
  group_by(year) %>%
  arrange(date) %>%
  mutate(cumulative_precip = cumsum(precipmm))

rain <- rain %>%
  mutate(doy_date = as.Date(doy - 1, origin = "2000-01-01"))

# Calculate historical envelope for RAIN (excluding 2023 and 2024)
hist_env_rain <- rain |>
  filter(!(year %in% c(2023, 2024))) |>
  group_by(doy_date) |>
  summarize(
    min_cum = min(cumulative_precip, na.rm = TRUE),
    max_cum = max(cumulative_precip, na.rm = TRUE)
  )

# Calculate historical envelope for GDD (excluding 2023 and 2024)
hist_env_gdd <- gdd |>
  filter(!(year %in% c(2023, 2024))) |>
  group_by(doy_date) |>
  summarize(
    min_cum = min(cumulative_gdd, na.rm = TRUE),
    max_cum = max(cumulative_gdd, na.rm = TRUE)
  )

# Calculate historical mean for both rain and GDD
mean_cum_rain <- rain |>
  filter(!(year %in% c(2023, 2024))) |>
  group_by(doy_date) |>
  summarize(cumulative_precip = mean(cumulative_precip, na.rm = TRUE)) |>
  mutate(year = "Mean") |>
  filter(doy_date != "2000-12-31")

mean_cum_gdd <- gdd |>
  filter(!(year %in% c(2023, 2024))) |>
  group_by(doy_date) |>
  summarize(cumulative_gdd = mean(cumulative_gdd, na.rm = TRUE)) |>
  mutate(year = "Mean") |>
  filter(doy_date != "2000-12-31")

# Combine highlighted years with mean
rain_highlight <- rain |>
  filter(year %in% c(2023, 2024)) |>
  mutate(year = as.character(year)) |>
  bind_rows(mean_cum_rain)

gdd_highlight <- gdd |>
  filter(year %in% c(2023, 2024)) |>
  mutate(year = as.character(year)) |>
  bind_rows(mean_cum_gdd)

# Rain plot
rain_plot_v2 <- ggplot() +
  geom_ribbon(
    data = hist_env_rain,
    aes(x = doy_date, ymin = min_cum, ymax = max_cum),
    fill = "gray80", alpha = 0.6
  ) +
  geom_line(
    data = rain_highlight,
    aes(x = doy_date, y = cumulative_precip, color = year, linetype = year),
    linewidth = 0.6
  ) +
  scale_color_manual(
    name = "Year",
    values = c("2023" = "#E41A1C", "2024" = "#377EB8", "Mean" = "black"),
    labels = c("2023" = "2023", "2024" = "2024", "Mean" = "Mean (Historical)")
  ) +
  scale_linetype_manual(
    name = "Year",
    values = c("2023" = "solid", "2024" = "dashed", "Mean" = "dotdash"),
    labels = c("2023" = "2023", "2024" = "2024", "Mean" = "Mean (Historical)")
  ) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  labs(x = "", y = "Cumulative Precipitation (mm)") +
  theme_publication() +
  theme(
    legend.position = "top",
    text = element_text(family = "Arial", size = 10),
    axis.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    legend.key.width = unit(0.8, "cm"),
    legend.key.height = unit(0.3, "cm")
  )

# GDD plot
gdd_plot_v2 <- ggplot() +
  geom_ribbon(
    data = hist_env_gdd,
    aes(x = doy_date, ymin = min_cum, ymax = max_cum),
    fill = "gray80", alpha = 0.6
  ) +
  geom_line(
    data = gdd_highlight,
    aes(x = doy_date, y = cumulative_gdd, color = year, linetype = year),
    linewidth = 0.6
  ) +
  scale_color_manual(
    name = "Year",
    values = c("2023" = "#E41A1C", "2024" = "#377EB8", "Mean" = "black"),
    labels = c("2023" = "2023", "2024" = "2024", "Mean" = "Mean (Historical)")
  ) +
  scale_linetype_manual(
    name = "Year",
    values = c("2023" = "solid", "2024" = "dashed", "Mean" = "dotdash"),
    labels = c("2023" = "2023", "2024" = "2024", "Mean" = "Mean (Historical)")
  ) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
  labs(x = "", y = "Cumulative Growing Degree Days (GDD)") +
  theme_publication() +
  theme(
    legend.position = "top",
    text = element_text(family = "Arial", size = 10),
    axis.text = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    legend.key.width = unit(0.8, "cm"),
    legend.key.height = unit(0.3, "cm")
  )

# Combine into pdplot with shared legend
six_panel_plot <- (rain_plot_v2 | gdd_plot_v2) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "top",
    legend.justification = "center",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.margin = margin(b = 1, unit = "pt"),
    legend.box.margin = margin(b = 1, unit = "pt"),
    plot.margin = margin(t = 3, r = 4, b = 2, l = 4, unit = "pt")
  ) & 
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE),
    linetype = guide_legend(nrow = 2, byrow = TRUE)
  )

ggsave("figures/figure1_weather.png", six_panel_plot, 
       width = 84, height = 105, dpi = 600, bg = "white", units = "mm")
six_panel_plot

# Report date ranges used for historical means (excludes 2023, 2024)
hist_range_rain <- rain |>
  filter(!(year %in% c(2023, 2024))) |>
  ungroup() |>
  summarize(
    start_date = min(year, na.rm = TRUE),
    end_date   = max(year, na.rm = TRUE)
  )

hist_range_gdd <- gdd |>
  filter(!(year %in% c(2023, 2024))) |>
  ungroup() |>
  summarize(
    start_date = min(year, na.rm = TRUE),
    end_date   = max(year, na.rm = TRUE)
  )

cat("Historical mean period (rain):", hist_range_rain$start_date, "to", hist_range_rain$end_date, "\n")
cat("Historical mean period (GDD):",  hist_range_gdd$start_date, "to", hist_range_gdd$end_date, "\n")
