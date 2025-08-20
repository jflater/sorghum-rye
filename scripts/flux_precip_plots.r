library(tidyverse)
library(ggplot2)
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
  pivot_longer(
    c(Sorghum, `Sorghum + Rye`),
    names_to = "treatment",
    values_to = "value"
  ) %>%
  mutate(
    treatment = factor(treatment, levels = c("Sorghum", "Sorghum + Rye"))
  ) %>%
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
    breaks = seq(start, end, by = "1 month"),
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
  scale_y_continuous(limits = c(0, 100)) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal"
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
    breaks = seq(start, end, by = "1 month"),
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
combined_plot <- (
  (p5 + theme(plot.margin = margin(0, 0, 0, 0))) /
    (p4 + theme(plot.margin = margin(0, 0, 0, 0)))
) +
  plot_annotation(theme = theme_sabr())

combined_plot


###############################################################################
########### 2024###############################################################
df <- daily %>%
  pivot_longer(
    c(Sorghum, `Sorghum + Rye`),
    names_to = "treatment",
    values_to = "value"
  ) %>%
  mutate(
    treatment = factor(treatment, levels = c("Sorghum", "Sorghum + Rye"))
  ) %>%
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
    breaks = seq(start, end, by = "1 month"),
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
    legend.box.background = element_rect(color = "black"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 100))
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
    breaks = seq(start, end, by = "1 month"),
    date_labels = "%b %d"
  ) +
  scale_y_reverse(limits = c(y_top, 0), name = "Daily Precipitation (mm)") +
  theme_sabr() +
  theme(
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  )


p7

# Combine plots
combined_plot2 <- (
  (p7 + theme(plot.margin = margin(0, 0, 0, 0))) /
    (p6 + theme(plot.margin = margin(0, 0, 0, 0), legend.position = "none"))) +
  plot_annotation(theme = theme_sabr())
combined_plot2
combined_plot | combined_plot2


# ---- load daily means + SE for 2023 ----
daily_se <- read_csv("data/seasonal_flux_combined.csv") %>%
  rename(day = year_month_day) %>%
  filter(Treatment %in% c("Sorghum", "Sorghum + Rye")) %>%
  mutate(day = as.Date(day)) %>%
  filter(year(day) == 2023) %>%
  group_by(Treatment, day) %>%
  summarise(
    mean_flux = mean(gnha_day_no_negative, na.rm = TRUE),
    se_flux = sd(gnha_day_no_negative, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# ---- full daily grid for the year ----
start <- as.Date("2023-01-01")
end <- as.Date("2023-12-31")
grid <- expand_grid(
  Treatment = c("Sorghum", "Sorghum + Rye"),
  day = seq.Date(start, end, by = "day")
)

# ---- join to grid and linearly interpolate within observed ranges ----
interp <- grid %>%
  left_join(daily_se, by = c("Treatment", "day")) %>%
  arrange(Treatment, day) %>%
  group_by(Treatment) %>%
  mutate(
    # interpolate interior gaps; outside the observed range stays NA (rule = 1)
    mean_flux_lin = approx(
      x = day[!is.na(mean_flux)],
      y = mean_flux[!is.na(mean_flux)],
      xout = day, rule = 1, ties = "ordered"
    )$y,
    se_flux_lin = approx(
      x = day[!is.na(se_flux)],
      y = se_flux[!is.na(se_flux)],
      xout = day, rule = 1, ties = "ordered"
    )$y
  ) %>%
  # treat days outside measurement range as 0 increment
  mutate(
    mean_flux_lin = replace_na(mean_flux_lin, 0),
    se_flux_lin   = replace_na(se_flux_lin, 0)
  ) %>%
  # cumulative sums: SE combined assuming independence day-to-day
  mutate(
    cum_flux = cumsum(mean_flux_lin),
    cum_se   = sqrt(cumsum(se_flux_lin^2))
  ) %>%
  ungroup()

# ---- plot cumulative with ribbon ----
treatment_colors <- c("Sorghum" = "#D2B48C", "Sorghum + Rye" = "#8B4513")

p_cum <- ggplot(interp, aes(day, cum_flux, color = Treatment, fill = Treatment)) +
  geom_ribbon(aes(ymin = cum_flux - cum_se, ymax = cum_flux + cum_se),
    alpha = 0.2, linewidth = 0
  ) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = treatment_colors) +
  scale_fill_manual(values = treatment_colors) +
  scale_x_date(
    limits = c(start, end),
    breaks = seq(start, end, by = "1 month"),
    date_labels = "%b %d"
  ) +
  labs(
    x = "2023",
    y = expression("Cumulative N"[2] * "O Flux (g N ha"^{
      -1
    } * ")"),
    color = NULL, fill = NULL
  ) +
  theme_sabr() +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(0, 2100))

p_cum

# ---- load daily means + SE for 2024 ----
daily_se <- read_csv("data/seasonal_flux_combined.csv") %>%
  rename(day = year_month_day) %>%
  filter(Treatment %in% c("Sorghum", "Sorghum + Rye")) %>%
  mutate(day = as.Date(day)) %>%
  filter(year(day) == 2024) %>%
  group_by(Treatment, day) %>%
  summarise(
    mean_flux = mean(gnha_day_no_negative, na.rm = TRUE),
    se_flux = sd(gnha_day_no_negative, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

# ---- full daily grid for the year ----
start <- as.Date("2024-01-01")
end <- as.Date("2024-12-30")
grid <- expand_grid(
  Treatment = c("Sorghum", "Sorghum + Rye"),
  day = seq.Date(start, end, by = "day")
)

# ---- join to grid and linearly interpolate within observed ranges ----
interp <- grid %>%
  left_join(daily_se, by = c("Treatment", "day")) %>%
  arrange(Treatment, day) %>%
  group_by(Treatment) %>%
  mutate(
    # interpolate interior gaps; outside the observed range stays NA (rule = 1)
    mean_flux_lin = approx(
      x = day[!is.na(mean_flux)],
      y = mean_flux[!is.na(mean_flux)],
      xout = day, rule = 1, ties = "ordered"
    )$y,
    se_flux_lin = approx(
      x = day[!is.na(se_flux)],
      y = se_flux[!is.na(se_flux)],
      xout = day, rule = 1, ties = "ordered"
    )$y
  ) %>%
  # treat days outside measurement range as 0 increment
  mutate(
    mean_flux_lin = replace_na(mean_flux_lin, 0),
    se_flux_lin   = replace_na(se_flux_lin, 0)
  ) %>%
  # cumulative sums: SE combined assuming independence day-to-day
  mutate(
    cum_flux = cumsum(mean_flux_lin),
    cum_se   = sqrt(cumsum(se_flux_lin^2))
  ) %>%
  ungroup()

# ---- plot cumulative with ribbon ----
treatment_colors <- c("Sorghum" = "#D2B48C", "Sorghum + Rye" = "#8B4513")

p_cum2 <- ggplot(interp, aes(day, cum_flux, color = Treatment, fill = Treatment)) +
  geom_ribbon(aes(ymin = cum_flux - cum_se, ymax = cum_flux + cum_se),
    alpha = 0.2, linewidth = 0
  ) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = treatment_colors) +
  scale_fill_manual(values = treatment_colors) +
  scale_x_date(
    limits = c(start, end),
    breaks = seq(start, end, by = "1 month"),
    date_labels = "%b %d"
  ) +
  labs(
    x = "2024",
    y = expression("Cumulative N"[2] * "O Flux (g N ha"^{
      -1
    } * ")"),
    color = NULL, fill = NULL
  ) +
  theme_sabr() +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  scale_y_continuous(limits = c(0, 2100))

p_cum2
library(patchwork)

# 1) who contributes a legend?
p4_leg <- p4 + theme(legend.position = "bottom") # keep
p5_nl <- p5 + guides(color = "none", fill = "none", linetype = "none", shape = "none")
p6_nl <- p6 + guides(color = "none", fill = "none", linetype = "none", shape = "none")
p7_nl <- p7 + guides(color = "none", fill = "none", linetype = "none", shape = "none")
pcum_nl <- p_cum + guides(color = "none", fill = "none", linetype = "none")
pcum2_nl <- p_cum2 + guides(color = "none", fill = "none", linetype = "none")

# 2–3) compose, collect, and place legend at bottom-center
final_plot <-
  ((p5_nl | p7_nl) /
    (p4_leg | p6_nl) /
    (pcum_nl | pcum2_nl)) +
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

final_plot


ggsave("figures/flux_precip_combined_2023_2024.png", final_plot,
  width = 8.5, height = 11, units = "in", dpi = 350
)
