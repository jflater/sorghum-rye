library(readxl)
library(tidyverse)

# Custom theme for consistency
theme_publication <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      # Backgrounds
      panel.background = element_rect(fill = "#ffffffff", color = NA),
      plot.background = element_rect(fill = "#ffffffff", color = NA),
      strip.background = element_rect(fill = alpha("#A5CAD2", 0.2)),
      
      # Grid
      panel.grid.major = element_line(color = alpha("#B7B5B3", 0.2)),
      panel.grid.minor = element_blank(),
      panel.spacing = unit(2, "lines"),
      
      # Text & Axis
      text = element_text(color = "#000000ff"),
      axis.text = element_text(color = "#000000ff", size = base_size * 0.8),
      axis.title = element_text(size = base_size),
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      axis.line = element_line(color = "black", linewidth = 0.5),
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      
      # Legend
      legend.position = "top",
      legend.justification = "center",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.margin = margin(b = 1, unit = "pt"),
      legend.box.margin = margin(b = 1, unit = "pt"),
      legend.text = element_text(size = base_size * 0.9),
      legend.title = element_text(size = base_size),
      legend.key.width = unit(0.8, "cm"),
      legend.key.height = unit(0.3, "cm")
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
sorg_biomass_23 <- read_excel("/Users/jaredflater/Library/CloudStorage/Box-Box/CABBI/Data/MeasuredData/SABR/SmallDrainagePlots/Biomass/2023GrowingSeason/DerivedDatasets/chopper_load_weights_yields.xlsx")
#######
sorg_biomass_23 <- sorg_biomass_23 |>
  group_by(Plot) |>
  mutate(total_biomass = sum(`Net Weight`)) |>
  select(`Harvest Date`, Plot, Treatment, total_biomass) |>
  distinct()

sorg_biomass_23

# Bar plot with total_biomass by treatment and with error bars showing sd
library(ggplot2)
library(dplyr)
plot_size_ha <- 0.145

sorg_biomass_23 |>
  group_by(Treatment) |>
  summarise(mean_biomass = mean(total_biomass / plot_size_ha), sd_biomass = sd(total_biomass / plot_size_ha)) |>
  ggplot(aes(x = Treatment, y = mean_biomass)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_biomass - sd_biomass, ymax = mean_biomass + sd_biomass), width = 0.2) +
  labs(x = "Treatment", y = "Average Biomass (tons/ha)")

sorg_biomass_23 |>
  group_by(Treatment) |>
  summarise(
    mean_biomass = mean(total_biomass / plot_size_ha),
    sd_biomass = sd(total_biomass / plot_size_ha),
    se_biomass = sd_biomass / sqrt(n())
  ) |>
  ggplot(aes(x = Treatment, y = mean_biomass)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_biomass - se_biomass, ymax = mean_biomass + se_biomass),
    width = 0.2
  ) +
  labs(x = "Treatment", y = "Average Biomass (tons/ha) (SE)")

sorg_biomass_23 |>
  group_by(Treatment) |>
  summarise(
    mean_biomass = mean(total_biomass / plot_size_ha),
    sd_biomass = sd(total_biomass / plot_size_ha),
    n = n(),
    se_biomass = sd_biomass / sqrt(n()),
    ci = qt(0.975, df = n - 1) * se_biomass
  ) |>
  ggplot(aes(x = Treatment, y = mean_biomass)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_biomass - ci, ymax = mean_biomass + ci),
    width = 0.2
  ) +
  labs(x = "Treatment", y = "Average Biomass (tons/ha) (95% CI)") +
  theme_minimal()

plot_size_ha <- 0.145 # make sure to use same value as final figure qmd
conversion_factor <- 0.907185 # 1 US ton = 0.907185 metric tons (or megagrams)

biomass_summary <- sorg_biomass_23 |>
  group_by(Treatment) |>
  summarise(
    # Convert US tons to metric tons and divide by plot size (ha)
    mean_biomass = mean((total_biomass * conversion_factor) / plot_size_ha),
    sd_biomass = sd((total_biomass * conversion_factor) / plot_size_ha),
    n = n(),
    se_biomass = sd_biomass / sqrt(n()),
    ci = qt(0.975, df = n - 1) * se_biomass,
    .groups = "drop"
  )
# Custom color palette (matching your function earlier)
treatment_colors <- c(
  "No cover" = "#D2B48C",
  "Cover" = "#8B4513"
)

# Plot with 95% CI error bars, using custom colors
ggplot(biomass_summary, aes(x = Treatment, y = mean_biomass, fill = Treatment)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_biomass - ci, ymax = mean_biomass + ci),
    width = 0.2, color = "black"
  ) +
  labs(x = "Treatment", y = expression("Average Biomass 2023 (Mg ha"^-1 * ")")) +
  scale_fill_manual(values = treatment_colors) +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )

ggsave("figures/sorghum_biomass_2023.png", width = 8, height = 6, dpi = 300)
moisture_df <- data.frame(
  Plot = c(2, 7, 9, 15),
  Moisture = c(0.749262537, 0.766325316, 0.731502423, 0.767241379)
)

# Calculate average moisture
avg_moisture <- mean(moisture_df$Moisture)

# Adjust biomass_summary by dividing mean_biomass and ci by avg_moisture
biomass_summary <- biomass_summary |>
  mutate(
    mean_biomass_corr = mean_biomass * (1 - avg_moisture),
    ci_corr = ci * (1 - avg_moisture)
  )
# Plot corrected biomass with error bars
ggplot(biomass_summary, aes(x = Treatment, y = mean_biomass_corr, fill = Treatment)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_biomass_corr - ci_corr, ymax = mean_biomass_corr + ci_corr),
    width = 0.2, color = "black"
  ) +
  labs(x = "Treatment", y = expression("Average Biomass 2024 (Mg ha"^-1 * ")")) +
  scale_fill_manual(values = treatment_colors) +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )

# Save the updated plot
ggsave("figures/sorghum_biomass_2023_corrected.png", width = 8, height = 6, dpi = 300)
###########
##### 2024
library(readxl)
sorg_biomass_24 <- read_excel("/Users/jaredflater/Library/CloudStorage/Box-Box/CABBI/Data/MeasuredData/SABR/SmallDrainagePlots/Biomass/2024GrowingSeason/RecordingDataSheets/sorg_chopper_harvest_load_weights.xlsx")
sorg_biomass_24 <- sorg_biomass_24 |>
  dplyr::filter(Study == "Drainage") |>
  group_by(Plot) |>
  mutate(total_biomass = sum(`Net Weight (t)`)) |>
  select(Date, Plot, total_biomass) |>
  distinct() |>
  mutate(treatment = case_when(
    Plot %in% c("2", "3", "5", "15") ~ "No cover",
    TRUE ~ "Cover",
  ))

# Figure of total_biomass by Plot
sorg_biomass_24 |>
  ggplot(aes(x = Plot, y = total_biomass, fill = treatment)) +
  geom_bar(stat = "identity") +
  labs(x = "Plot", y = "Total Biomass (tons)")

plot_size_ha <- 0.145
conversion_factor <- 0.907185 # 1 US ton = 0.907185 metric tons (or megagrams)

biomass_summary <- sorg_biomass_24 |>
  group_by(treatment) |>
  summarise(
    # Convert US tons to metric tons and divide by plot size (ha)
    mean_biomass = mean((total_biomass * conversion_factor) / plot_size_ha),
    sd_biomass = sd((total_biomass * conversion_factor) / plot_size_ha),
    n = n(),
    se_biomass = sd_biomass / sqrt(n()),
    ci = qt(0.975, df = n - 1) * se_biomass,
    .groups = "drop"
  )
# Custom color palette (matching your function earlier)
treatment_colors <- c(
  "No cover" = "#D2B48C",
  "Cover" = "#8B4513"
)

# Plot with 95% CI error bars, using custom colors
ggplot(biomass_summary, aes(x = treatment, y = mean_biomass, fill = treatment)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_biomass - ci, ymax = mean_biomass + ci),
    width = 0.2, color = "black"
  ) +
  labs(x = "Treatment", y = expression("Average Biomass 2024 (Mg ha"^-1 * ")")) +
  scale_fill_manual(values = treatment_colors) +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )

ggsave("figures/sorghum_biomass_2024.png", width = 8, height = 6, dpi = 300)

moisture_df <- data.frame(
  Plot = c(2, 7, 9, 15),
  Moisture = c(0.749262537, 0.766325316, 0.731502423, 0.767241379)
)
# Calculate average moisture
avg_moisture <- mean(moisture_df$Moisture)

biomass_summary <- biomass_summary |>
  mutate(
    mean_biomass_corr = mean_biomass * (1 - avg_moisture),
    ci_corr = ci * (1 - avg_moisture)
  )

# Plot corrected biomass with error bars
ggplot(biomass_summary, aes(x = treatment, y = mean_biomass_corr, fill = treatment)) +
  geom_bar(stat = "identity", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_biomass_corr - ci_corr, ymax = mean_biomass_corr + ci_corr),
    width = 0.2, color = "black"
  ) +
  labs(x = "Treatment", y = expression("Average Biomass 2024 (Mg ha"^-1 * ")")) +
  scale_fill_manual(values = treatment_colors) +
  theme_minimal() +
  theme(
    legend.position = "none",
    text = element_text(size = 14),
    axis.title = element_text(size = 16)
  )

# Save the updated plot
ggsave("figures/sorghum_biomass_2024_corrected.png", width = 8, height = 6, dpi = 300)

# combine data for a faceted plot (standardize columns and units)
# Ensure common parameters
plot_size_ha <- 0.145
conversion_factor <- 0.907185 # US ton -> Mg

# Moisture correction: reuse avg_moisture if already defined above; otherwise fallback to 0
if (!exists("avg_moisture")) {
  avg_moisture <- 0.0
}

# Standardize 2023 dataframe to have 'treatment' and Mg/ha
sorg_biomass_23_std <- {
  df <- sorg_biomass_23
  if ("Treatment" %in% names(df) && !("treatment" %in% names(df))) {
    df <- df |> dplyr::rename(treatment = Treatment)
  }
  df |>
    dplyr::mutate(
      year = 2023,
      Plot = as.character(Plot),
      biomass_mg_ha = ((total_biomass * conversion_factor) / plot_size_ha) * (1 - avg_moisture)
    ) |>
    dplyr::select(year, Plot, treatment, biomass_mg_ha)
}

# Standardize 2024 dataframe to have Mg/ha (already has 'treatment')
sorg_biomass_24_std <- sorg_biomass_24 |>
  dplyr::mutate(
    year = 2024,
    Plot = as.character(Plot),
    biomass_mg_ha = ((total_biomass * conversion_factor) / plot_size_ha) * (1 - avg_moisture)
  ) |>
  dplyr::select(year, Plot, treatment, biomass_mg_ha)

# Combine and standardize treatment labels
treatment_levels <- c("Sorghum", "Sorghum + WCC")
combined_biomass <- dplyr::bind_rows(sorg_biomass_23_std, sorg_biomass_24_std) |>
  dplyr::mutate(treatment = trimws(treatment)) |>
  dplyr::mutate(treatment = dplyr::case_when(
    treatment == "No cover" ~ "Sorghum",
    treatment %in% c("Cover", "Sorghum + Rye") ~ "Sorghum + WCC",
    TRUE ~ treatment
  ))

# Summarize by treatment and year
combined_biomass_summary <- combined_biomass |>
  dplyr::group_by(year, treatment) |>
  dplyr::summarise(
    mean_biomass = mean(biomass_mg_ha, na.rm = TRUE),
    sd_biomass = sd(biomass_mg_ha, na.rm = TRUE),
    n = dplyr::n(),
    se_biomass = sd_biomass / sqrt(n),
    ci = stats::qt(0.975, df = pmax(n - 1, 1)) * se_biomass,
    .groups = "drop"
  )


# Faceted plot by year with 95% CI
biomass_faceted <- ggplot2::ggplot(
  combined_biomass_summary,
  ggplot2::aes(x = treatment, y = mean_biomass, fill = treatment)
) +
  ggplot2::geom_col(color = "black", width = 0.7) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = mean_biomass - ci, ymax = mean_biomass + ci), width = 0.2, color = "black") +
  ggplot2::facet_wrap(~year, nrow = 1) +
  ggplot2::labs(x = "Treatment", y = expression("Moisture-corrected Biomass (Mg ha"^-1 * ")")) +
  ggplot2::scale_fill_manual(values = treatment_colors) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.position = "none",
    text = ggplot2::element_text(size = 14),
    axis.title = ggplot2::element_text(size = 16)
  ) +
  theme_publication() +
  scale_fill_treatments() +
  labs(fill = "Treatment") +
  theme(legend.position = "none")

print(biomass_faceted)
ggplot2::ggsave("figures/sorghum_biomass_faceted_by_year_corrected.png", biomass_faceted, width = 10, height = 6, dpi = 300)

mean(biomass_faceted$data$mean_biomass)
# Now create a boxplot instead of a bar plot
biomass_boxplot <- ggplot2::ggplot(
  combined_biomass, # Use the individual data points instead of summary
  ggplot2::aes(x = treatment, y = biomass_mg_ha, fill = treatment)
) +
  ggplot2::geom_boxplot(width = 0.7, color = "black", outlier.shape = 16) +
  # Add individual data points with jitter for better visualization
  ggplot2::geom_jitter(width = 0.2, alpha = 0.5, size = 2) +
  ggplot2::facet_wrap(~year, nrow = 1) +
  ggplot2::labs(x = "Treatment", y = expression("Moisture-corrected Biomass (Mg ha"^-1 * ")")) +
  ggplot2::scale_fill_manual(values = treatment_colors) +
  theme_publication() +
  scale_fill_treatments() +
  labs(fill = "Treatment") +
  theme(legend.position = "none")

print(biomass_boxplot)
ggplot2::ggsave("figures/sorghum_biomass_boxplot_by_year.png", biomass_boxplot, width = 10, height = 6, dpi = 300)


# Rye Biomass

rye_biomass_23 <- read_excel("data/BiomassData_Rye_20230522.xlsx") |> dplyr::mutate(year = 2023)
rye_biomass_24 <- read_excel("data/BiomassData_Rye_20240515.xlsx") |> dplyr::mutate(year = 2024)

# Combine data
combined_biomass_rye <- dplyr::bind_rows(rye_biomass_23, rye_biomass_24)

# Process Rye Data to match Sorghum Data
# Units: g/m^2 -> Mg/ha (multiply by 0.01)
rye_processed <- combined_biomass_rye |>
  dplyr::mutate(
    dry_biomass_g = `Dry Mass + Dry Bag Mass` - `Dry Bag Mass`,
    biomass_mg_ha = dry_biomass_g * 0.01, # Convert g/m2 to Mg/ha
    treatment = "Sorghum + WCC",
    component = "Rye",
    Plot = as.character(`Plot ID`)
  ) |>
  dplyr::select(year, Plot, treatment, biomass_mg_ha, component)

# Prepare Sorghum data (from combined_biomass created above)
sorghum_processed <- combined_biomass |>
  dplyr::mutate(component = "Sorghum") |>
  dplyr::select(year, Plot, treatment, biomass_mg_ha, component)

# Combine all biomass
all_biomass <- dplyr::bind_rows(sorghum_processed, rye_processed)

# Summaries for sorghum-only bars and error bars (total should be sorghum).
sorghum_stats <- sorghum_processed |>
  dplyr::filter(treatment %in% treatment_levels) |>
  dplyr::group_by(year, treatment) |>
  dplyr::summarise(
    mean_sorghum = mean(biomass_mg_ha, na.rm = TRUE),
    sd_sorghum = sd(biomass_mg_ha, na.rm = TRUE),
    n = dplyr::n(),
    se_sorghum = sd_sorghum / sqrt(n),
    ci_sorghum = stats::qt(0.975, df = pmax(n - 1, 1)) * se_sorghum,
    .groups = "drop"
  ) |>
  dplyr::mutate(
    treatment = factor(treatment, levels = treatment_levels),
    fill_key = dplyr::case_when(
      as.character(treatment) == "Sorghum" ~ "Sorghum",
      as.character(treatment) == "Sorghum + WCC" ~ "Sorghum + WCC",
      TRUE ~ as.character(treatment)
    )
  )

# Rye means for textured overlay (does not add to bar height).
rye_stats <- rye_processed |>
  dplyr::filter(treatment %in% treatment_levels) |>
  dplyr::group_by(year, treatment) |>
  dplyr::summarise(
    mean_rye = mean(biomass_mg_ha, na.rm = TRUE),
    sd_rye = sd(biomass_mg_ha, na.rm = TRUE),
    n = dplyr::n(),
    se_rye = sd_rye / sqrt(n),
    ci_rye = stats::qt(0.975, df = pmax(n - 1, 1)) * se_rye,
    .groups = "drop"
  ) |>
  dplyr::mutate(treatment = factor(treatment, levels = treatment_levels))

rye_overlay <- rye_stats |>
  dplyr::left_join(
    sorghum_stats |>
      dplyr::select(year, treatment, mean_sorghum),
    by = c("year", "treatment")
  ) |>
  dplyr::mutate(
    rye_height = pmin(mean_rye, mean_sorghum),
    fill_key = "Rye"
  ) |>
  dplyr::filter(!is.na(rye_height))

fill_levels <- c("Sorghum", "Sorghum + WCC", "Rye")
fill_palette <- c(
  "Sorghum" = "#D8D97AFF",
  "Sorghum + WCC" = "#95C36EFF",
  "Rye" = "#F5DEB3"
)

sorghum_stats <- sorghum_stats |>
  dplyr::mutate(fill_key = factor(fill_key, levels = fill_levels))
rye_overlay <- rye_overlay |>
  dplyr::mutate(fill_key = factor(fill_key, levels = fill_levels))

# Use ggpattern for texture when available; fall back to semi-transparent fill.
rye_layer <- if (requireNamespace("ggpattern", quietly = TRUE)) {
  ggpattern::geom_col_pattern(
    data = rye_overlay,
    ggplot2::aes(x = treatment, y = rye_height, fill = fill_key),
    pattern = "stripe",
    pattern_fill = "grey30",
    pattern_colour = "grey30",
    pattern_angle = 45,
    pattern_density = 0.4,
    alpha = 0.4,
    color = "black",
    width = 0.7,
    inherit.aes = FALSE
  )
} else {
  ggplot2::geom_col(
    data = rye_overlay,
    ggplot2::aes(x = treatment, y = rye_height, fill = fill_key),
    alpha = 0.4,
    color = "black",
    width = 0.7,
    inherit.aes = FALSE
  )
}

final_biomass_plot <- ggplot2::ggplot() +
  ggplot2::geom_col(
    data = sorghum_stats,
    ggplot2::aes(x = treatment, y = mean_sorghum, fill = fill_key),
    color = "black",
    width = 0.7
  ) +
  rye_layer +
  ggplot2::geom_errorbar(
    data = rye_stats,
    ggplot2::aes(
      x = treatment,
      ymin = pmax(0, mean_rye - ci_rye),
      ymax = mean_rye + ci_rye
    ),
    width = 0.15,
    inherit.aes = FALSE
  ) +
  ggplot2::geom_errorbar(
    data = sorghum_stats,
    ggplot2::aes(
      x = treatment,
      ymin = mean_sorghum - ci_sorghum,
      ymax = mean_sorghum + ci_sorghum
    ),
    width = 0.2
  ) +
  ggplot2::facet_wrap(~year, nrow = 1) +
  ggplot2::scale_fill_manual(
    values = fill_palette,
    breaks = fill_levels,
    name = "Biomass Component",
    drop = FALSE
  ) +
  ggplot2::labs(
    x = "",
    y = expression("Biomass (Mg ha"^-1*")")
  ) +
  theme_publication() +
  ggplot2::theme(
    legend.position = "top",
    legend.justification = "center"
  )

print(final_biomass_plot)
ggplot2::ggsave("figures/final_biomass_stacked.png", final_biomass_plot, width = 174, height = 174, units = "mm", dpi = 600)

# Statistical tests: treatments within each year and years within each treatment (sorghum biomass only).
sorghum_tests_by_year <- combined_biomass |>
  dplyr::filter(treatment %in% treatment_levels) |>
  dplyr::group_by(year) |>
  dplyr::group_modify(~{
    if (dplyr::n_distinct(.x$treatment) < 2) {
      return(dplyr::tibble(
        comparison = "Sorghum vs Sorghum + WCC",
        t = NA_real_,
        df = NA_real_,
        p_value = NA_real_,
        note = "Only one treatment level in this year"
      ))
    }
    tst <- stats::t.test(biomass_mg_ha ~ treatment, data = .x)
    dplyr::tibble(
      comparison = "Sorghum vs Sorghum + WCC",
      t = unname(tst$statistic),
      df = unname(tst$parameter),
      p_value = tst$p.value,
      note = NA_character_
    )
  }) |>
  dplyr::ungroup()

sorghum_tests_by_treatment <- combined_biomass |>
  dplyr::filter(treatment %in% treatment_levels) |>
  dplyr::group_by(treatment) |>
  dplyr::group_modify(~{
    if (dplyr::n_distinct(.x$year) < 2) {
      return(dplyr::tibble(
        comparison = "2023 vs 2024",
        t = NA_real_,
        df = NA_real_,
        p_value = NA_real_,
        note = "Only one year for this treatment"
      ))
    }
    tst <- stats::t.test(biomass_mg_ha ~ factor(year), data = .x)
    dplyr::tibble(
      comparison = "2023 vs 2024",
      t = unname(tst$statistic),
      df = unname(tst$parameter),
      p_value = tst$p.value,
      note = NA_character_
    )
  }) |>
  dplyr::ungroup()

print(sorghum_tests_by_year)
print(sorghum_tests_by_treatment)

