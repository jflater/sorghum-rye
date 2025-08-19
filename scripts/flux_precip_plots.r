library(tidyverse)
library(janitor)

daily_precip <- read_csv("data/rain/daily_precip_2023_2024.csv") %>%
  select(day, precipmm)

seasonal_flux <- read_csv("data/seasonal_flux_combined.csv") %>%
  rename(day = year_month_day) %>%
  group_by(Treatment, day) %>%
  summarise(
    mean_flux = mean(gnha_day_no_negative, na.rm = TRUE),
    se_flux = sd(gnha_day_no_negative, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  filter(Treatment %in% c("Sorghum", "Sorghum + Rye") | is.na(Treatment)) %>%
  pivot_wider(names_from = Treatment, values_from = mean_flux)

compare_df_cols(
  daily_precip,
  seasonal_flux
)

# join by day
daily <- daily_precip %>%
  left_join(seasonal_flux, by = "day")

write_csv(daily, "data/daily_precip_flux_combined.csv")

library(tidyverse)
library(lubridate)

treatment_colors <- c(
  "Corn" = "#fda500",
  "Soy" = "#E9967A",
  "Sorghum" = "#D2B48C",
  "Sorghum + Rye" = "#8B4513"
)

theme_sabr <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      axis.title = element_text(size = base_size + 2),
      axis.text = element_text(size = base_size),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_text(size = base_size),
      legend.text = element_text(size = base_size - 1),
      strip.text = element_text(size = base_size + 2),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_blank()
    )
}
fert_events <- data.frame(
  growing_season = c(2023, 2024),
  fertilizer_date = as.Date(c("2023-05-05", "2024-07-17"))
)

df <- daily %>%
  pivot_longer(c(Sorghum, `Sorghum + Rye`), names_to = "treatment", values_to = "value") %>%
  mutate(treatment = factor(treatment, levels = c("Sorghum", "Sorghum + Rye"))) %>%
  select(day, treatment, value, se_flux) %>%
  filter(!is.na(value) & year(day) %in% c(2023))


year_min <- year(min(df$day, na.rm = TRUE))
year_max <- year(max(df$day, na.rm = TRUE))

# For a single year axis: pick the year you want to show (e.g. 2023)
plot_year <- year_min # or hard-code: plot_year <- 2023
start <- as.Date(paste0(plot_year, "-01-01"))
end <- as.Date(paste0(plot_year, "-12-31"))

p4 <- ggplot(df, aes(x = day, y = value, color = treatment)) +
  geom_line(size = 1.2, alpha = 0.8, na.rm = TRUE) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_errorbar(
    aes(ymin = value - se_flux, ymax = value + se_flux),
    width = 0.2,
    alpha = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  geom_segment(
    data = fert_events[1, ],
    aes(
      x = fertilizer_date, xend = fertilizer_date,
      y = 0, yend = 80, color = "Fertilizer Date"
    ),
    linetype = "dashed",
    alpha = 0.7,
    inherit.aes = FALSE
  ) +
  geom_label(
    data = fert_events[1, ],
    inherit.aes = FALSE,
    aes(
      x = fertilizer_date,
      y = 60,
      label = format(fertilizer_date, "%d-%b-%Y")
    ),
    size = 4,
    hjust = 0,
    vjust = 0.5,
    show.legend = FALSE
  ) +
  scale_x_date(
    limits = c(start, end),
    breaks = seq(start, end, by = "1 week"),
    date_labels = "%b %d" # e.g. Jan 01, Jan 08, ...
  ) +
  labs(
    x = "Date",
    y = expression("Daily N"[2] * "O Flux (g N ha"^"-1" * ")"),
    color = "Legend"
  ) +
  theme_sabr() +
  scale_color_manual(values = c(treatment_colors, "Fertilizer Date" = "red")) +
  # place legend in the inside the plot area
  theme(
    legend.position = c(0.1, 0.9),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(color = "black")
  )
p4
diff_thresh <- 5 # change as needed

# daily absolute difference and flag rows
flag_df <- read_csv("data/seasonal_flux_combined.csv") %>%
  rename(day = year_month_day) %>%
  group_by(Treatment, day) %>%
  summarise(
    mean_flux = mean(gnha_day_no_negative, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = Treatment, values_from = mean_flux) %>%
  select(day, Sorghum, `Sorghum + Rye`) %>%
  mutate(
    diff = abs(Sorghum - `Sorghum + Rye`),
    flag = ifelse(diff > diff_thresh, "Flagged", "Normal")
  ) %>%
  filter(flag == "Flagged")

p4 <- p4 + geom_segment(
  data = flag_df,
  aes(x = day - .5, xend = day + 0.5, y = 85, yend = 85),
  inherit.aes = FALSE,
  linewidth = 1.2, # thickness
  lineend = "butt",
  color = "red" # square ends
)

p4

y_top <- max(daily_precip$precipmm, na.rm = TRUE) * 1.1

p5 <- ggplot(daily_precip, aes(day, precipmm)) +
  geom_col(fill = "#0080b3", color = "black", width = 1) +
  scale_x_date(
    limits = c(start, end),
    breaks = seq(start, end, by = "1 week"),
    date_labels = "%b %d"
  ) +
  scale_y_reverse(limits = c(y_top, 0), name = "Daily Precipitation (mm)") +
  theme_sabr() +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )
p5
# Combine plots
library(patchwork)
combined_plot <- ((p5 + theme(plot.margin = margin(0, 0, 0, 0))) /
  (p4 + theme(plot.margin = margin(0, 0, 0, 0)))) +
  plot_annotation(theme = theme_sabr())

combined_plot


###############################################################################
########### 2024###############################################################
df <- daily %>%
  pivot_longer(c(Sorghum, `Sorghum + Rye`), names_to = "treatment", values_to = "value") %>%
  mutate(treatment = factor(treatment, levels = c("Sorghum", "Sorghum + Rye"))) %>%
  select(day, treatment, value, se_flux) %>%
  filter(!is.na(value) & year(day) %in% c(2024))


year_min <- year(min(df$day, na.rm = TRUE))
year_max <- year(max(df$day, na.rm = TRUE))

# For a single year axis: pick the year you want to show (e.g. 2023)
plot_year <- year_min # or hard-code: plot_year <- 2023
start <- as.Date(paste0(plot_year, "-01-01"))
end <- as.Date(paste0(plot_year, "-12-31"))

p6 <- ggplot(df, aes(x = day, y = value, color = treatment)) +
  geom_line(size = 1.2, alpha = 0.8, na.rm = TRUE) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_errorbar(
    aes(ymin = value - se_flux, ymax = value + se_flux),
    width = 0.2,
    alpha = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  geom_segment(
    data = fert_events[2, ],
    aes(
      x = fertilizer_date, xend = fertilizer_date,
      y = 0, yend = 80, color = "Fertilizer Date"
    ),
    linetype = "dashed",
    alpha = 0.7,
    inherit.aes = FALSE
  ) +
  geom_label(
    data = fert_events[2, ],
    inherit.aes = FALSE,
    aes(
      x = fertilizer_date,
      y = 60,
      label = format(fertilizer_date, "%d-%b-%Y")
    ),
    size = 4,
    hjust = 0,
    vjust = 0.5,
    show.legend = FALSE
  ) +
  scale_x_date(
    limits = c(start, end),
    breaks = seq(start, end, by = "1 week"),
    date_labels = "%b %d" # e.g. Jan 01, Jan 08, ...
  ) +
  labs(
    x = "Date",
    y = expression("Daily N"[2] * "O Flux (g N ha"^"-1" * ")"),
    color = "Legend"
  ) +
  theme_sabr() +
  scale_color_manual(values = c(treatment_colors, "Fertilizer Date" = "red")) +
  # place legend in the inside the plot area
  theme(
    legend.position = c(0.1, 0.9),
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(color = "black")
  )
p6
diff_thresh <- 5 # change as needed

# daily absolute difference and flag rows
flag_df <- read_csv("data/seasonal_flux_combined.csv") %>%
  rename(day = year_month_day) %>%
  group_by(Treatment, day) %>%
  summarise(
    mean_flux = mean(gnha_day_no_negative, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = Treatment, values_from = mean_flux) %>%
  select(day, Sorghum, `Sorghum + Rye`) %>%
  mutate(
    diff = abs(Sorghum - `Sorghum + Rye`),
    flag = ifelse(diff > diff_thresh, "Flagged", "Normal")
  ) %>%
  filter(flag == "Flagged")

p6 <- p6 + geom_segment(
  data = flag_df,
  aes(x = day - .5, xend = day + 0.5, y = 85, yend = 85),
  inherit.aes = FALSE,
  linewidth = 1.2, # thickness
  lineend = "butt",
  color = "red" # square ends
)

p6
y_top <- max(daily_precip$precipmm, na.rm = TRUE) * 1.1

p7 <- ggplot(daily_precip, aes(day, precipmm)) +
  geom_col(fill = "#0080b3", color = "black", width = 1) +
  scale_x_date(
    limits = c(start, end),
    breaks = seq(start, end, by = "1 week"),
    date_labels = "%b %d"
  ) +
  scale_y_reverse(limits = c(y_top, 0), name = "Daily Precipitation (mm)") +
  theme_sabr() +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )
p7

# Combine plots
library(patchwork)
combined_plot2 <- ((p7 + theme(plot.margin = margin(0, 0, 0, 0))) /
  (p6 + theme(plot.margin = margin(0, 0, 0, 0)))) +
  plot_annotation(theme = theme_sabr())
combined_plot2 + combined_plot
