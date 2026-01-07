# =============================================================================
# Figure 3: Dual-Axis Leaching + Precipitation Plot (2023-2024)
# =============================================================================
# This script generates figure3_dual_axis_leaching_precip_2023_2024v2.png
# Extracted from Final_Figures_Tables.qmd
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Load Required Packages
# -----------------------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(janitor)
library(patchwork)
library(readxl)

# Re-establish dplyr preferences after loading packages
if (requireNamespace("conflicted", quietly = TRUE)) {
  library(conflicted)
  conflicts_prefer(dplyr::select)
  conflicts_prefer(dplyr::filter)
  conflicts_prefer(dplyr::summarise)
}

# -----------------------------------------------------------------------------
# 2. Custom Theme and Color Scales
# -----------------------------------------------------------------------------
theme_publication <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.background = element_rect(fill = "#ffffffff", color = NA),
      plot.background = element_rect(fill = "#ffffffff", color = NA),
      panel.grid.major = element_line(color = alpha("#B7B5B3", 0.2)),
      panel.grid.minor = element_blank(),
      text = element_text(color = "#000000ff"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      axis.line = element_blank(),
      axis.text = element_text(color = "#000000ff"),
      strip.background = element_rect(fill = alpha("#A5CAD2", 0.2)),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.spacing = unit(2, "lines")
    )
}

# Treatment colors
treatment_colors <- c(
  "Sorghum" = "#D8D97AFF",
  "Sorghum + WCC" = "#95C36EFF",
  "Corn" = "#74C8C3FF",
  "Soy" = "#0A2E57FF"
)

scale_color_treatments <- function() scale_color_manual(values = treatment_colors)
scale_fill_treatments <- function() scale_fill_manual(values = treatment_colors)

# -----------------------------------------------------------------------------
# 3. Load Data
# -----------------------------------------------------------------------------
# Import nitrogen leaching data
leaching_data <- read_csv("data/clean_leaching.csv") %>%
  clean_names() %>%
  mutate(
    date = as.Date(date),
    year = year(date)
  ) %>%
  filter(year %in% c(2023, 2024)) %>%
  filter(!is.na(year))

# Import historical rain data
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

# -----------------------------------------------------------------------------
# 4. Data Processing for Leaching Figure
# -----------------------------------------------------------------------------
# Define plot area
plot_area_ft2 <- 120 * 160
plot_area_ha <- plot_area_ft2 / 107639.1041671

# Build daily treatment means and SE for leaching (plot-level daily loss)
daily_leaching <- leaching_data %>%
  filter(
    treatment %in% c("Sorghum", "Sorghum + Rye"),
    !is.na(total_n_loss_mg) # keep only days with actual N loss data
  ) %>%
  mutate(treatment = if_else(treatment == "Sorghum + Rye", "Sorghum + WCC", treatment)) %>%
  mutate(
    daily_n_loss_kgha = (total_n_loss_mg / 1e6) / plot_area_ha
  ) %>%
  group_by(plot, treatment, year, date) %>%
  summarise(
    daily_loss = mean(daily_n_loss_kgha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(plot, treatment, year, date)

# Treatment means and SE for *daily* losses (for top panel)
daily_leaching_summary <- daily_leaching %>%
  group_by(treatment, year, date) %>%
  summarise(
    mean_loss = mean(daily_loss, na.rm = TRUE),
    se_loss   = sd(daily_loss, na.rm = TRUE) / sqrt(sum(!is.na(daily_loss))),
    .groups   = "drop"
  )
# -----------------------------------------------------------------------------
# 4b. Plot-level cumulative leaching and treatment-level SE for ribbons
# -----------------------------------------------------------------------------

# 1) Cumulative N loss per plot, using the existing cumulative_n_loss_mg
cum_leaching_plot <- leaching_data %>%
  filter(
    treatment %in% c("Sorghum", "Sorghum + Rye"),
    !is.na(cumulative_n_loss_mg)
  ) %>%
  mutate(treatment = if_else(treatment == "Sorghum + Rye", "Sorghum + WCC", treatment)) %>%
  mutate(
    cum_loss_kgha = (cumulative_n_loss_mg / 1e6) / plot_area_ha # mg -> kg N ha^-1
  ) %>%
  group_by(plot, treatment, year, date) %>%
  summarise(
    cum_loss_kgha = mean(cum_loss_kgha, na.rm = TRUE), # just in case of duplicates
    .groups = "drop"
  )
# 2) Treatment means and SE of cumulative loss at each date
cum_leaching_combined <- cum_leaching_plot %>%
  group_by(treatment, year, date) %>%
  summarise(
    vals = list(cum_loss_kgha[!is.na(cum_loss_kgha)]),
    n_plots = length(vals[[1]]),
    cum_loss = mean(vals[[1]]),
    cum_se = if (n_plots > 1) sd(vals[[1]]) / sqrt(n_plots) else 0,
    .groups = "drop"
  ) %>%
  select(-vals)

# 3) "Observed" cumulative points for plotting (just the mean curve)
obs_points <- cum_leaching_combined %>%
  transmute(
    treatment, year, date,
    cum_obs = cum_loss
  )

# Calculate daily significance tests for leaching between treatments
daily_leaching_significance <- leaching_data %>%
  filter(treatment %in% c("Sorghum", "Sorghum + Rye")) %>%
  mutate(treatment = if_else(treatment == "Sorghum + Rye", "Sorghum + WCC", treatment)) %>%
  mutate(
    year = lubridate::year(date),
    daily_n_loss_kgha = (total_n_loss_mg / 1e6) / plot_area_ha
  ) %>%
  filter(year %in% c(2023, 2024)) %>%
  group_by(year, date) %>%
  summarise(
    p_value = {
      # Get leaching values for each treatment
      sorghum_loss <- daily_n_loss_kgha[treatment == "Sorghum"]
      sorghum_rye_loss <- daily_n_loss_kgha[treatment == "Sorghum + WCC"]

      # Remove NA values
      sorghum_loss <- sorghum_loss[!is.na(sorghum_loss)]
      sorghum_rye_loss <- sorghum_rye_loss[!is.na(sorghum_rye_loss)]

      # Perform t-test if we have enough data
      if (length(sorghum_loss) >= 3 && length(sorghum_rye_loss) >= 3) {
        tryCatch(wilcox.test(sorghum_loss, sorghum_rye_loss, exact = FALSE)$p.value,
          error = function(e) NA_real_
        )
      } else {
        NA_real_
      }
    },
    .groups = "drop"
  ) %>%
  mutate(
    significant = p_value < 0.05,
    star_label = ifelse(significant & !is.na(p_value), "*", "")
  )

# Get max leaching values for star positioning
max_leaching_by_date <- daily_leaching_summary %>%
  group_by(year, date) %>%
  summarise(max_loss = max(mean_loss + se_loss, na.rm = TRUE), .groups = "drop")

# Combine significance data with position data for leaching
leaching_star_data <- daily_leaching_significance %>%
  left_join(max_leaching_by_date, by = c("year", "date")) %>%
  filter(star_label == "*") %>%
  mutate(star_y = max_loss * 1.1)

# Get actual measurement date ranges from RAW leaching data
actual_leaching_date_ranges <- leaching_data %>%
  filter(treatment %in% c("Sorghum", "Sorghum + Rye")) %>%
  mutate(treatment = if_else(treatment == "Sorghum + Rye", "Sorghum + WCC", treatment)) %>%
  mutate(year = lubridate::year(date)) %>%
  filter(year %in% c(2023, 2024), !is.na(total_n_loss_mg)) %>%
  group_by(year) %>%
  summarise(
    min_date = min(date, na.rm = TRUE),
    max_date = max(date, na.rm = TRUE),
    .groups = "drop"
  )

cat("Actual leaching measurement date ranges:\n")
print(actual_leaching_date_ranges)

# Filter precipitation to match ACTUAL leaching measurement periods
daily_precip_leaching <- rain %>%
  transmute(date, year, precipmm) %>%
  filter(year %in% c(2023, 2024))

# Subset cumulative data to the observed cumulative date range per year
cum_leaching_trimmed <- cum_leaching_combined %>%
  group_by(year) %>%
  mutate(
    min_date = min(date, na.rm = TRUE),
    max_date = max(date, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(date >= min_date, date <= max_date) %>%
  select(-min_date, -max_date)

# Also trim observed points to match
obs_points_trimmed <- obs_points %>%
  group_by(year) %>%
  mutate(
    min_date = min(date, na.rm = TRUE),
    max_date = max(date, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(date >= min_date, date <= max_date) %>%
  select(-min_date, -max_date)

# -----------------------------------------------------------------------------
# 5. Create Figure 3 (Dual-Axis Leaching + Precipitation)
# -----------------------------------------------------------------------------
# Global limits and scaling
global_max_loss <- max(daily_leaching_summary$mean_loss + daily_leaching_summary$se_loss, na.rm = TRUE)
global_max_precip_leaching <- max(daily_precip_leaching$precipmm, na.rm = TRUE)
global_max_cum_loss <- max(cum_leaching_trimmed$cum_loss + cum_leaching_trimmed$cum_se, na.rm = TRUE)

global_precip_scale_factor_leaching <- global_max_loss * 0.4 / global_max_precip_leaching
global_plot_top_leaching <- global_max_loss * 1.1

fert_events <- tibble(
  year = c(2023, 2024),
  fertilizer_date = as.Date(c("2023-05-05", "2024-07-17"))
)
# Updated dual-axis function (matches Figure 2 structure)
create_leaching_dual_axis_plot <- function(year_val, is_right_panel = FALSE) {
  leaching_year <- daily_leaching_summary %>% filter(year == year_val)
  precip_year <- daily_precip_leaching %>% filter(year == year_val)
  star_year <- leaching_star_data %>% filter(year == year_val)
  cum_year <- cum_leaching_trimmed %>% filter(year == year_val)
  obs_year <- obs_points_trimmed %>% filter(year == year_val)
  fert_year <- fert_events %>% filter(year == year_val)
  # axis limits: actual leaching measurement window for this year
  year_start <- as.Date(paste0(year_val, "-01-01"))
  year_end <- as.Date(paste0(year_val, "-12-31"))

  # ---- DAILY PANEL ----
  daily_plot <- ggplot() +
    geom_rect(
      data = precip_year,
      aes(
        xmin = date - 0.5, xmax = date + 0.5,
        ymin = global_plot_top_leaching - precipmm * global_precip_scale_factor_leaching,
        ymax = global_plot_top_leaching
      ),
      fill = "#377EB8", alpha = 0.7
    ) +
    geom_point(
      data = leaching_year,
      aes(x = date, y = mean_loss, color = treatment),
      size = 2
    ) +
    geom_errorbar(
      data = leaching_year,
      aes(x = date, ymin = mean_loss - se_loss, ymax = mean_loss + se_loss, color = treatment),
      width = 0.2, alpha = 0.5
    ) +
    # geom_vline(
    #   data = fert_year,
    #   aes(xintercept = as.numeric(fertilizer_date)),
    #   linetype = "dashed", color = "red", alpha = 0.7
    # ) +
    geom_text(
      data = star_year,
      aes(x = date, y = star_y, label = star_label),
      color = "black", size = 5
    ) +
    scale_color_treatments() +
    guides(color = "none") +
    scale_x_date(
      date_breaks = "2 month", date_labels = "%b",
      limits = c(year_start, year_end)
    ) +
    scale_y_continuous(
      name = if (!is_right_panel) expression("Daily N Loss (kg N ha"^-1 * " d"^-1 * ")") else "",
      limits = c(0, global_plot_top_leaching),
      sec.axis = sec_axis(~ (global_plot_top_leaching - .) / global_precip_scale_factor_leaching,
        name = if (is_right_panel) "Daily Precipitation (mm)" else ""
      )
    ) +
    theme_publication() +
    theme(
      legend.position = "none",
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank(),
      axis.text.y = if (is_right_panel) element_blank() else element_text(),
      axis.ticks.y = if (is_right_panel) element_blank() else element_line(),
      axis.title.y = if (is_right_panel) element_blank() else element_text(),
      axis.text.y.right = if (is_right_panel) element_text(color = "#377EB8") else element_blank(),
      axis.ticks.y.right = if (is_right_panel) element_line() else element_blank(),
      axis.title.y.right = if (is_right_panel) element_text(color = "#377EB8") else element_blank()
    ) +
    geom_hline(yintercept = 0, linetype = "solid", alpha = 0.3)

  # ---- CUMULATIVE PANEL (now identical to Figure 2) ----
  cumulative_plot <- ggplot(cum_year, aes(x = date, y = cum_loss, color = treatment)) +
    geom_ribbon(
      aes(ymin = pmax(cum_loss - cum_se, 0), ymax = cum_loss + cum_se, fill = treatment),
      alpha = 0.2, linewidth = 0
    ) +
    geom_line(linewidth = 1.1) +
    # geom_point(
    #   data = obs_year,
    #   aes(x = date, y = cum_obs, color = treatment),
    #   size = 1.8
    # ) +
    scale_color_treatments() +
    scale_fill_treatments() +
    scale_x_date(
      date_breaks = "2 month", date_labels = "%b",
      limits = c(year_start, year_end)
    ) +
    scale_y_continuous(
      name = if (!is_right_panel) expression("Cumulative N Loss (kg N ha"^-1 * ")") else "",
      limits = c(0, global_max_cum_loss)
    ) +
    labs(x = "Month") +
    theme_publication() +
    theme(
      axis.text.y = if (is_right_panel) element_blank() else element_text(),
      axis.ticks.y = if (is_right_panel) element_blank() else element_line(),
      axis.title.y = if (is_right_panel) element_blank() else element_text()
    ) +
    labs(color = "Treatment", fill = "Treatment")

  # ---- Combine top + bottom panels ----
  (daily_plot / cumulative_plot) +
    patchwork::plot_layout(heights = c(1.2, 1))
}

# --- Build panels and assemble full figure ---
leach_2023 <- create_leaching_dual_axis_plot(2023, is_right_panel = FALSE)
leach_2024 <- create_leaching_dual_axis_plot(2024, is_right_panel = TRUE)

figure3_dual_axis <- (leach_2023 | leach_2024) +
  patchwork::plot_layout(guides = "collect") &
  theme(
    legend.position = "top",
    legend.justification = "center",
    legend.box.just = "center"
  )

figure3_dual_axis

ggsave(
  filename = "figures/figure3_dual_axis_leaching_precip_2023_2024.png",
  plot     = figure3_dual_axis,
  width    = 174,
  height   = 174,
  units    = "mm",
  dpi      = 600,
  bg       = "white"
)


# find what percent nitrate is of total leaching
leaching_data %>%
  filter(treatment %in% c("Sorghum", "Sorghum + Rye")) %>%
  mutate(treatment = if_else(treatment == "Sorghum + Rye", "Sorghum + WCC", treatment)) %>%
  mutate(
    year = lubridate::year(date),
    daily_n_loss_kgha = (total_n_loss_mg / 1e6) / plot_area_ha,
    daily_no3_loss_kgha = (nitrate_loss_mg / 1e6) / plot_area_ha
  ) %>%
  filter(year %in% c(2023, 2024)) %>%
  group_by(treatment, year) %>%
  summarise(
    total_n_loss = sum(daily_n_loss_kgha, na.rm = TRUE),
    total_no3_loss = sum(daily_no3_loss_kgha, na.rm = TRUE),
    percent_no3 = total_no3_loss / total_n_loss * 100,
    .groups = "drop"
  ) 
