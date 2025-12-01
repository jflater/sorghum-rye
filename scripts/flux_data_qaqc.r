library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(janitor)

# Load the datasets
source("scripts/read_flux_to_add.r")

cat("=== FLUX DATA QA/QC REPORT ===\n\n")

# =============================================================================
# 1. TEMPORAL COMPLETENESS CHECKS
# =============================================================================
cat("1. TEMPORAL COMPLETENESS\n")
cat("========================\n\n")

# Check seasonal_flux temporal coverage
seasonal_dates <- seasonal_flux %>% 
  mutate(date = as.Date(year_month_day)) %>%
  arrange(date)

cat("SEASONAL FLUX DATASET:\n")
cat("Date range:", as.character(min(seasonal_dates$date)), "to", as.character(max(seasonal_dates$date)), "\n")
cat("Total measurement days:", n_distinct(seasonal_dates$date), "\n")
cat("Growing seasons covered:", paste(unique(seasonal_dates$growing_season), collapse = ", "), "\n")

# Check for date gaps in seasonal data
date_sequence <- seq.Date(min(seasonal_dates$date), max(seasonal_dates$date), by = "day")
missing_dates <- date_sequence[!date_sequence %in% seasonal_dates$date]
cat("Missing dates in sequence:", length(missing_dates), "\n")
if(length(missing_dates) > 0 && length(missing_dates) <= 10) {
  cat("First few missing dates:", paste(head(missing_dates, 10), collapse = ", "), "\n")
} else if(length(missing_dates) > 10) {
  cat("First 10 missing dates:", paste(head(missing_dates, 10), collapse = ", "), "...\n")
}

# Check flux_data temporal coverage
flux_dates <- flux_data %>%
  mutate(date = as.Date(DATE_TIME)) %>%
  arrange(date)

cat("\nFLUX_DATA (TO ADD) DATASET:\n")
cat("Date range:", as.character(min(flux_dates$date)), "to", as.character(max(flux_dates$date)), "\n")
cat("Total measurement days:", n_distinct(flux_dates$date), "\n")
cat("Measurement dates:", paste(unique(flux_dates$date), collapse = ", "), "\n")

# Check temporal overlap/gaps between datasets
all_seasonal_dates <- unique(seasonal_dates$date)
all_flux_dates <- unique(flux_dates$date)
overlapping_dates <- intersect(all_seasonal_dates, all_flux_dates)
cat("Overlapping dates between datasets:", length(overlapping_dates), "\n")
if(length(overlapping_dates) > 0) {
  cat("Overlapping dates:", paste(overlapping_dates, collapse = ", "), "\n")
}

# =============================================================================
# 2. SPATIAL/PLOT COMPLETENESS CHECKS  
# =============================================================================
cat("\n\n2. SPATIAL/PLOT COMPLETENESS\n")
cat("=============================\n\n")

# Check plot coverage in seasonal_flux
seasonal_plots <- seasonal_flux %>%
  distinct(plot, Treatment, RowvsInterrow) %>%
  arrange(plot)

cat("SEASONAL FLUX DATASET:\n")
cat("Total unique plots:", n_distinct(seasonal_flux$plot), "\n")
cat("Plot range:", min(seasonal_plots$plot), "to", max(seasonal_plots$plot), "\n")
cat("Treatments:", paste(unique(seasonal_plots$Treatment), collapse = ", "), "\n")
cat("Row vs Interrow positions:", paste(unique(seasonal_plots$RowvsInterrow), collapse = ", "), "\n")

# Check for expected plot-treatment-position combinations
seasonal_combinations <- seasonal_flux %>%
  count(plot, Treatment, RowvsInterrow) %>%
  arrange(plot)

cat("Plot-Treatment-Position combinations:\n")
print(seasonal_combinations)

# Check plot coverage in flux_data
flux_plots <- flux_data %>%
  distinct(plot, location) %>%
  arrange(plot)

cat("\nFLUX_DATA (TO ADD) DATASET:\n")
cat("Total unique plots:", n_distinct(flux_data$plot), "\n")
cat("Plot range:", min(flux_plots$plot), "to", max(flux_plots$plot), "\n")
cat("Locations:", paste(unique(flux_plots$location), collapse = ", "), "\n")

# Check for missing plots in expected range
all_seasonal_plots <- unique(seasonal_plots$plot)
all_flux_plots <- unique(flux_plots$plot)

# Assuming plot numbering should be continuous
seasonal_plot_nums <- as.numeric(all_seasonal_plots)
expected_seasonal_plots <- sprintf("%02d", min(seasonal_plot_nums):max(seasonal_plot_nums))
missing_seasonal_plots <- expected_seasonal_plots[!expected_seasonal_plots %in% all_seasonal_plots]

cat("Missing plots in seasonal data (if continuous numbering expected):", 
    ifelse(length(missing_seasonal_plots) > 0, paste(missing_seasonal_plots, collapse = ", "), "None"), "\n")

flux_plot_nums <- as.numeric(all_flux_plots)
expected_flux_plots <- sprintf("%02d", min(flux_plot_nums):max(flux_plot_nums))
missing_flux_plots <- expected_flux_plots[!expected_flux_plots %in% all_flux_plots]

cat("Missing plots in flux_data (if continuous numbering expected):", 
    ifelse(length(missing_flux_plots) > 0, paste(missing_flux_plots, collapse = ", "), "None"), "\n")

# =============================================================================
# 3. VARIABLE COMPLETENESS CHECKS
# =============================================================================
cat("\n\n3. VARIABLE COMPLETENESS\n")
cat("=========================\n\n")

# Check seasonal_flux variables
cat("SEASONAL FLUX DATASET:\n")
cat("Variables:", paste(colnames(seasonal_flux), collapse = ", "), "\n")

seasonal_completeness <- seasonal_flux %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
  mutate(missing_percent = round(missing_count / nrow(seasonal_flux) * 100, 2)) %>%
  arrange(desc(missing_count))

cat("Missing data by variable:\n")
print(seasonal_completeness)

# Check flux_data variables
cat("\nFLUX_DATA (TO ADD) DATASET:\n")
cat("Variables:", paste(colnames(flux_data), collapse = ", "), "\n")

flux_completeness <- flux_data %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
  mutate(missing_percent = round(missing_count / nrow(flux_data) * 100, 2)) %>%
  arrange(desc(missing_count))

cat("Missing data by variable:\n")
print(flux_completeness)

# =============================================================================
# 4. DATA QUALITY CHECKS
# =============================================================================
cat("\n\n4. DATA QUALITY CHECKS\n")
cat("=======================\n\n")

# Check for duplicates in seasonal_flux
seasonal_duplicates <- seasonal_flux %>%
  count(year_month_day, plot, RowvsInterrow) %>%
  filter(n > 1)

cat("SEASONAL FLUX DUPLICATES:\n")
if(nrow(seasonal_duplicates) > 0) {
  cat("Found", nrow(seasonal_duplicates), "duplicate plot-date-position combinations:\n")
  print(seasonal_duplicates)
} else {
  cat("No duplicates found\n")
}

# Check for duplicates in flux_data  
flux_duplicates <- flux_data %>%
  count(DATE_TIME, plot, location) %>%
  filter(n > 1)

cat("\nFLUX_DATA DUPLICATES:\n")
if(nrow(flux_duplicates) > 0) {
  cat("Found", nrow(flux_duplicates), "duplicate plot-time-location combinations:\n")
  print(flux_duplicates)
} else {
  cat("No duplicates found\n")
}

# Range checks for key variables
cat("\nRANGE CHECKS:\n")

# Seasonal flux range checks
cat("Seasonal flux ranges:\n")
seasonal_numeric <- seasonal_flux %>%
  select_if(is.numeric) %>%
  summarise(across(everything(), list(min = ~min(., na.rm = TRUE), 
                                     max = ~max(., na.rm = TRUE),
                                     median = ~median(., na.rm = TRUE))))

print(seasonal_numeric)

# Check for extreme values (potential outliers)
flux_outliers <- seasonal_flux %>%
  filter(abs(best_flux_nmol_1m_2s_1) > 50) %>%  # Adjust threshold as appropriate
  select(year_month_day, plot, Treatment, RowvsInterrow, best_flux_nmol_1m_2s_1)

cat("\nPotential flux outliers (>50 nmol/m2/s):\n")
if(nrow(flux_outliers) > 0) {
  print(flux_outliers)
} else {
  cat("No extreme flux values found\n")
}

# Temperature and moisture range checks
temp_outliers <- seasonal_flux %>%
  filter(ts_2_mean < -10 | ts_2_mean > 50) %>%  # Adjust as appropriate for your climate
  select(year_month_day, plot, ts_2_mean)

cat("\nPotential temperature outliers (<-10°C or >50°C):\n")
if(nrow(temp_outliers) > 0) {
  print(temp_outliers)
} else {
  cat("No temperature outliers found\n")
}

moisture_outliers <- seasonal_flux %>%
  filter(swc_2_mean < 0 | swc_2_mean > 1) %>%  # Soil water content should be 0-1
  select(year_month_day, plot, swc_2_mean)

cat("\nPotential soil moisture outliers (<0 or >1):\n")
if(nrow(moisture_outliers) > 0) {
  print(moisture_outliers)
} else {
  cat("No soil moisture outliers found\n")
}

# =============================================================================
# 5. SUMMARY AND RECOMMENDATIONS
# =============================================================================
cat("\n\n5. SUMMARY AND RECOMMENDATIONS\n")
cat("===============================\n\n")

cat("DATA COMPLETENESS SUMMARY:\n")
cat("- Seasonal flux dataset:", nrow(seasonal_flux), "observations across", 
    n_distinct(seasonal_flux$plot), "plots\n")
cat("- Additional flux dataset:", nrow(flux_data), "observations from", 
    length(unique(flux_data$source_file)), "measurement days\n")
cat("- Total missing values in seasonal data:", sum(seasonal_completeness$missing_count), "\n")
cat("- Total missing values in flux_data:", sum(flux_completeness$missing_count), "\n")

cat("\nRECOMMENDATIONS:\n")
if(any(seasonal_completeness$missing_percent > 10)) {
  high_missing <- seasonal_completeness %>% filter(missing_percent > 10)
  cat("- CRITICAL: Variables with >10% missing data:", paste(high_missing$variable, collapse = ", "), "\n")
}

if(length(missing_dates) > 365) {
  cat("- CRITICAL: Large temporal gaps detected - check measurement protocols\n")
}

if(nrow(seasonal_duplicates) > 0 | nrow(flux_duplicates) > 0) {
  cat("- WARNING: Duplicate measurements found - review data processing\n")
}

if(nrow(flux_outliers) > 0 | nrow(temp_outliers) > 0 | nrow(moisture_outliers) > 0) {
  cat("- WARNING: Potential outliers detected - review for measurement errors\n")
}

cat("- NEXT STEPS: Review flagged issues and implement data cleaning procedures\n")

cat("\n=== END QA/QC REPORT ===\n")