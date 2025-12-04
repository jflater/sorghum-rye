# Soil Inorganic Nitrogen Analysis Script
# Approach: Non-Parametric (Kruskal-Wallis + Dunn's Test)
# Grouping: Date x Treatment AND Year x Treatment
# Author: Agricultural Data Expert

# 1. Setup ----------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse, # Data manipulation
  rstatix, # Tidy statistics (includes dunn_test)
  multcompView, # For generating letters from pairwise p-values
  gt, # Tables
  glue # String formatting
)

# 2. Load and Preprocess --------------------------------------------------
data <- read_csv("data/soils/cleaned_soil_data.csv", show_col_types = FALSE)
colnames(data)
clean_data <- data %>%
  filter(treatment %in% c("Sorghum", "Sorghum + Rye")) %>%
  mutate(total_n_mg_per_kg = ammonia_mg_per_kg + nitrate_mg_per_kg) %>%
  pivot_longer(
    cols = c(ammonia_mg_per_kg, nitrate_mg_per_kg, total_n_mg_per_kg),
    names_to = "analyte",
    values_to = "ppm"
  ) %>%
  mutate(
    analyte = case_when(
      analyte == "ammonia_mg_per_kg" ~ "Ammonium (NH4-N)",
      analyte == "nitrate_mg_per_kg" ~ "Nitrate (NO3-N)",
      analyte == "total_n_mg_per_kg" ~ "Total Inorganic N",
      TRUE ~ analyte
    ),
    treatment = as.factor(treatment),
    year = as.character(year) # Ensure year is treated as a grouping category
  )

# 3. Statistical Analysis Function (Kruskal-Wallis) -----------------------
run_stats_kw <- function(df) {
  # A. Global Test: Kruskal-Wallis
  kw_res <- kruskal_test(df, ppm ~ treatment)
  p_val <- kw_res$p

  # B. Post-Hoc: Dunn's Test
  letters_df <- tibble(treatment = unique(df$treatment), letter = "a")

  if (!is.na(p_val) && p_val < 0.05) {
    dunn_res <- dunn_test(df, ppm ~ treatment, p.adjust.method = "holm") |>
      mutate(pair_name = paste(group1, group2, sep = "-"))

    p_vec <- setNames(dunn_res$p.adj, dunn_res$pair_name)

    # Generate letters (Compact Letter Display)
    letters_list <- multcompLetters(p_vec, threshold = 0.05)

    letters_df <- tibble(
      treatment = names(letters_list$Letters),
      letter = letters_list$Letters
    )
  } else {
    letters_df <- tibble(
      treatment = unique(df$treatment),
      letter = ""
    )
  }
  # D. Summary Stats (Mean, SE, Variance)
  # We use type="common" to get variance (var) along with mean/se
  stats_summary <- df %>%
    group_by(treatment) %>%
    get_summary_stats(ppm, type = "common") %>%
    left_join(letters_df, by = "treatment") %>%
    mutate(
      kruskal_p = p_val,
      test_method = "Kruskal-Wallis"
    )

  return(stats_summary)
}

# 4. Execute Analysis (Date Level AND Year Level) -------------------------

# Run 1: Group by Date x Analyte
results_date <- clean_data %>%
  group_by(date, analyte) %>%
  nest() %>%
  mutate(stats = map(data, run_stats_kw)) %>%
  unnest(stats) %>%
  select(-data) %>%
  mutate(
    group_type = "By Date",
    date = as.character(date)
  ) # Ensure date is character for stacking

# Run 2: Group by Year x Analyte
results_year <- clean_data %>%
  group_by(year, analyte) %>%
  nest() %>%
  mutate(stats = map(data, run_stats_kw)) %>%
  unnest(stats) %>%
  select(-data) %>%
  rename(date = year) %>% # Rename year to date column to stack them
  mutate(group_type = "By Year")

# Combine results
results_combined <- bind_rows(results_date, results_year)

# 5. Format for Publication -----------------------------------------------

# Part A: The Means Rows
means_table <- results_combined %>%
  mutate(
    # Create value string: Mean ± SE (Letter)
    display_value = glue("{sprintf('%.2f', mean)} ± {sprintf('%.2f', se)} {letter}"),
    display_value = str_trim(display_value)
  ) %>%
  select(group_type, date, treatment, analyte, display_value) %>%
  pivot_wider(
    names_from = analyte,
    values_from = display_value
  )

# Part B: The Source of Variation (Kruskal-Wallis P-values)
p_values_table <- results_combined %>%
  distinct(group_type, date, analyte, kruskal_p) %>%
  mutate(
    display_value = case_when(
      kruskal_p < 0.001 ~ "< 0.001",
      TRUE ~ sprintf("%.3f", kruskal_p)
    ),
    treatment = "Source: Treatment (P-value)"
  ) %>%
  select(group_type, date, treatment, analyte, display_value) %>%
  pivot_wider(
    names_from = analyte,
    values_from = display_value
  )

# Part C: Combine and Order
final_table_data <- bind_rows(means_table, p_values_table) %>%
  mutate(
    # Sort order: Date groups first, then Year groups
    # Within groups: Treatments first, then P-value row
    type_rank = ifelse(group_type == "By Date", 1, 2),
    row_rank = ifelse(treatment == "Source: Treatment (P-value)", 2, 1)
  ) %>%
  arrange(type_rank, date, row_rank, treatment) %>%
  select(-type_rank, -row_rank, -group_type)

# 6. Generate RTF ---------------------------------------------------------

gt_tbl <- final_table_data %>%
  group_by(date) %>%
  gt() %>%
  cols_label(treatment = "Treatment") %>%
  tab_header(
    title = "Soil Inorganic Nitrogen Response",
    subtitle = "Analysis performed by Date and by Year. Values are Mean ± SE. Letters represent Dunn's Test (p < 0.05)."
  ) %>%
  # Style the P-value row
  tab_style(
    style = cell_text(weight = "bold", style = "italic"),
    locations = cells_body(
      columns = treatment,
      rows = treatment == "Source: Treatment (P-value)"
    )
  ) %>%
  # Add border above P-value row
  tab_style(
    style = cell_borders(sides = "top", color = "black", weight = px(1)),
    locations = cells_body(
      rows = treatment == "Source: Treatment (P-value)"
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_options(
    row_group.background.color = "#E0E0E0",
    table.font.names = "Times New Roman"
  )

gtsave(gt_tbl, "soil_nitrogen_kruskal.rtf")
gt_tbl
# Save detailed CSV with Variance included
write_csv(results_combined, "soil_nitrogen_kruskal_stats.csv")

print("Analysis Complete. Table includes Date-level and Year-level analysis.")
