library(readxl)
sorg_biomass_23 <- read_excel("C:/Users/jflater/Box/CABBI/Data/MeasuredData/SABR/SmallDrainagePlots/Biomass/2023GrowingSeason/DerivedDatasets/chopper_load_weights_yields.xlsx")
#######
sorg_biomass_23 <- sorg_biomass_23 %>%
  group_by(Plot) %>%
  mutate(total_biomass = sum(`Net Weight`)) %>%
  select(`Harvest Date`, Plot, Treatment, total_biomass) %>%
  distinct()

sorg_biomass_23

# Bar plot with total_biomass by treatment and with error bars showing sd
library(ggplot2)
library(dplyr)
plot_size_ha <- 0.145

sorg_biomass_23 %>%
  group_by(Treatment) %>%
  summarise(mean_biomass = mean(total_biomass / plot_size_ha), sd_biomass = sd(total_biomass / plot_size_ha)) %>%
  ggplot(aes(x = Treatment, y = mean_biomass)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_biomass - sd_biomass, ymax = mean_biomass + sd_biomass), width = 0.2) +
  labs(x = "Treatment", y = "Average Biomass (tons/ha)")

sorg_biomass_23 %>%
  group_by(Treatment) %>%
  summarise(
    mean_biomass = mean(total_biomass / plot_size_ha),
    sd_biomass = sd(total_biomass / plot_size_ha),
    se_biomass = sd_biomass / sqrt(n())
  ) %>%
  ggplot(aes(x = Treatment, y = mean_biomass)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_biomass - se_biomass, ymax = mean_biomass + se_biomass),
    width = 0.2
  ) +
  labs(x = "Treatment", y = "Average Biomass (tons/ha) (SE)")

sorg_biomass_23 %>%
  group_by(Treatment) %>%
  summarise(
    mean_biomass = mean(total_biomass / plot_size_ha),
    sd_biomass = sd(total_biomass / plot_size_ha),
    n = n(),
    se_biomass = sd_biomass / sqrt(n()),
    ci = qt(0.975, df = n - 1) * se_biomass
  ) %>%
  ggplot(aes(x = Treatment, y = mean_biomass)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_biomass - ci, ymax = mean_biomass + ci),
    width = 0.2
  ) +
  labs(x = "Treatment", y = "Average Biomass (tons/ha) (95% CI)") +
  theme_minimal()

plot_size_ha <- 0.145 # make sure to use same value as final figure qmd
conversion_factor <- 0.907185 # 1 US ton = 0.907185 metric tons (or megagrams)

biomass_summary <- sorg_biomass_23 %>%
  group_by(Treatment) %>%
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
biomass_summary <- biomass_summary %>%
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
sorg_biomass_24 <- read_excel("C:/Users/jflater/Box/CABBI/Data/MeasuredData/SABR/SmallDrainagePlots/Biomass/2024GrowingSeason/RecordingDataSheets/sorg_chopper_harvest_load_weights.xlsx")
sorg_biomass_24 <- sorg_biomass_24 %>%
  dplyr::filter(Study == "Drainage") %>%
  group_by(Plot) %>%
  mutate(total_biomass = sum(`Net Weight (t)`)) %>%
  select(Date, Plot, total_biomass) %>%
  distinct() %>%
  mutate(treatment = case_when(
    Plot %in% c("2", "3", "5", "15") ~ "No cover",
    TRUE ~ "Cover",
  ))

# Figure of total_biomass by Plot
sorg_biomass_24 %>%
  ggplot(aes(x = Plot, y = total_biomass, fill = treatment)) +
  geom_bar(stat = "identity") +
  labs(x = "Plot", y = "Total Biomass (tons)")

plot_size_ha <- 0.145
conversion_factor <- 0.907185 # 1 US ton = 0.907185 metric tons (or megagrams)

biomass_summary <- sorg_biomass_24 %>%
  group_by(treatment) %>%
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

biomass_summary <- biomass_summary %>%
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
    df <- df %>% dplyr::rename(treatment = Treatment)
  }
  df %>%
    dplyr::mutate(
      year = 2023,
      Plot = as.character(Plot),
      biomass_mg_ha = ((total_biomass * conversion_factor) / plot_size_ha) * (1 - avg_moisture)
    ) %>%
    dplyr::select(year, Plot, treatment, biomass_mg_ha)
}

# Standardize 2024 dataframe to have Mg/ha (already has 'treatment')
sorg_biomass_24_std <- sorg_biomass_24 %>%
  dplyr::mutate(
    year = 2024,
    Plot = as.character(Plot),
    biomass_mg_ha = ((total_biomass * conversion_factor) / plot_size_ha) * (1 - avg_moisture)
  ) %>%
  dplyr::select(year, Plot, treatment, biomass_mg_ha)

# Combine
combined_biomass <- dplyr::bind_rows(sorg_biomass_23_std, sorg_biomass_24_std)

# Summarize by treatment and year
combined_biomass_summary <- combined_biomass %>%
  dplyr::group_by(year, treatment) %>%
  dplyr::summarise(
    mean_biomass = mean(biomass_mg_ha, na.rm = TRUE),
    sd_biomass = sd(biomass_mg_ha, na.rm = TRUE),
    n = dplyr::n(),
    se_biomass = sd_biomass / sqrt(n),
    ci = stats::qt(0.975, df = pmax(n - 1, 1)) * se_biomass,
    .groups = "drop"
  ) %>% # rename treatments to Sorghum and Sorghum + Rye
  dplyr::mutate(treatment = dplyr::case_when(
    treatment == "No cover" ~ "Sorghum",
    treatment == "Cover" ~ "Sorghum + Rye",
    TRUE ~ treatment
  ))


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
