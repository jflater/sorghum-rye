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
  glue, # String formatting
  janitor # Data cleaning
)

# 2. Load and Preprocess --------------------------------------------------
data <- read_csv("data/soils/cleaned_soil_data.csv", show_col_types = FALSE)
colnames(data)
clean_data <- data %>%
  mutate(total_n_mg_per_kg = ammonia_mg_per_kg + nitrate_mg_per_kg) %>%
  pivot_longer(
    cols = c(ammonia_mg_per_kg, nitrate_mg_per_kg, total_n_mg_per_kg),
    names_to = "analyte",
    values_to = "ppm" # it's actually mg/kg
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



library(tidyverse)
library(rstatix)
library(multcompView)
library(gt)
library(patchwork)
library(glue)

# --- 1. PREPARE DATA ---
# (Assuming 'clean_data' is loaded from your CSV)
# Ensure we have the calculated columns
df_analysis <- data %>%
  filter(treatment %in% c("Sorghum", "Sorghum + Rye")) %>%
  mutate(
    total_n = ammonia_mg_per_kg + nitrate_mg_per_kg,
    nitrate_prop = nitrate_mg_per_kg / total_n * 100, # Percentage
    year = as.character(year)
  )

# --- 2. GENERATE TABLE 2 ---

# Function to run Kruskal-Wallis & Get Letters per Year
get_year_stats <- function(data, yr) {
  # Filter year
  df_yr <- data %>% filter(year == yr)
  
  # 1. Global P-value
  kw <- kruskal_test(df_yr, total_n ~ treatment)
  p_val <- kw$p
  
  # 2. Letters (Dunn's Test)
  if(p_val < 0.05) {
    dunn <- dunn_test(df_yr, total_n ~ treatment, p.adjust.method = "holm")
    dunn <- dunn %>% mutate(pair = paste(group1, group2, sep="-"))
    p_vec <- setNames(dunn$p.adj, dunn$pair)
    lets <- multcompLetters(p_vec)$Letters
  } else {
    lets <- setNames(rep("", length(unique(df_yr$treatment))), unique(df_yr$treatment))
  }
  
  # 3. Calculate Summary Stats
  stats <- df_yr %>%
    group_by(treatment) %>%
    summarise(
      n_samples = n(),
      mean = mean(total_n, na.rm=TRUE),
      sd = sd(total_n, na.rm=TRUE),
      min = min(total_n, na.rm=TRUE),
      median = median(total_n, na.rm=TRUE),
      max = max(total_n, na.rm=TRUE)
    ) %>%
    mutate(
      letter = lets[treatment],
      year = yr,
      p_global = p_val
    )
  
  return(stats)
}

# Run for both years
stats_2023 <- get_year_stats(df_analysis, "2023")
stats_2024 <- get_year_stats(df_analysis, "2024")
table_data <- bind_rows(stats_2023, stats_2024)

# Format for Display
final_table_2 <- table_data %>%
  mutate(
    `Mean ± Standard Deviation` = glue("{sprintf('%.2f', mean)} ± {sprintf('%.2f', sd)}{letter}"),
    `P-value` = ifelse(p_global < 0.001, "< 0.001", sprintf("%.3f", p_global))
  ) %>%
  select(
    Year = year, 
    Treatment = treatment, 
    `Number of Soil Samples` = n_samples, 
    `Mean ± Standard Deviation`, 
    Minimum = min, 
    Median = median, 
    Maximum = max,
    p_val_raw = `P-value` # Keep for pivot logic
  )

# Create the "P-value Row" structure
gt_prep <- final_table_2 %>%
  group_by(Year) %>%
  mutate(
    # Create a p-value label only for the last row of the group to avoid duplication
    p_row_label = ifelse(row_number() == n(), p_val_raw, NA) 
  ) %>%
  ungroup()

# Generate GT Table
# Note: GT handles the grouping nicely, so we don't need a literal separate row in data
library(tidyverse)
library(gt)
library(glue)

# 1. FIX THE P-VALUE LOGIC
# We define the text strings explicitly before making the table to avoid the "TRUE" bug.
p_val_2023 <- unique(stats_2023$p_global)
year_label_23 <- ifelse(p_val_2023 < 0.001, 
                        "2023\n(P < 0.001)", 
                        paste0("2023\n(P = ", sprintf("%.3f", p_val_2023), ")"))

p_val_2024 <- unique(stats_2024$p_global)
year_label_24 <- ifelse(p_val_2024 < 0.001, 
                        "2024\n(P < 0.001)", 
                        paste0("2024\n(P = ", sprintf("%.3f", p_val_2024), ")"))

# 2. PREPARE THE DATA WITH A DEDICATED YEAR COLUMN
gt_ready <- bind_rows(stats_2023, stats_2024) %>%
  mutate(
    # Create the combined Mean ± SD string
    `Mean ± Standard Deviation` = glue("{sprintf('%.2f', mean)} ± {sprintf('%.2f', sd)}{letter}"),
    
    # Create the Year Column with the P-value included
    Year = case_when(
      year == "2023" ~ year_label_23,
      year == "2024" ~ year_label_24
    )
  ) %>%
  select(
    Year, 
    Treatment = treatment, 
    `Number of Soil Samples` = n_samples, 
    `Mean ± Standard Deviation`, 
    Minimum = min, 
    Median = median, 
    Maximum = max
  )

# 3. GENERATE THE TABLE (FLAT STYLE)
table2_gt <- gt_ready %>%
  gt() %>% # removed 'groupname_col' so Year stays as a column
  
  # Format the headers
  cols_label(
    Year = "Year",
    Treatment = "Treatment",
    `Number of Soil Samples` = "N",
    `Mean ± Standard Deviation` = "Mean ± SD",
    Minimum = "Min",
    Median = "Med",
    Maximum = "Max"
  ) %>%
  
  # Format numbers
  fmt_number(columns = c(Minimum, Median, Maximum), decimals = 2) %>%
  
  # Align columns for readability
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left", columns = c(Treatment)) %>%
  
  # Handle the line break in the Year column (2023 \n P=...)
  fmt_markdown(columns = Year) %>%
  
  # Add Header
  tab_header(
    title = md("**Table 2. Soil salt-extractable inorganic nitrogen (ammonium + nitrate)**")
  ) 

# 4. ADD FOOTNOTE
nitrate_range <- range(df_analysis$nitrate_prop, na.rm = TRUE)
footnote_text <- glue("Nitrate ranged from {sprintf('%.1f', nitrate_range[1])} to {sprintf('%.1f', nitrate_range[2])}% of inorganic N (Figure S1).")

table2_gt <- table2_gt %>%
  tab_footnote(footnote = footnote_text)

# Save/Print
print(table2_gt)

# save .rtf
gtsave(table2_gt, "Table_2_Soil_Inorganic_Nitrogen.rtf")


# --- 3. GENERATE FIGURE S1 ---

# Panel A: Boxplot of Total N concentrations
p1 <- ggplot(df_analysis, aes(x = treatment, y = total_n, fill = treatment)) +
  geom_boxplot(alpha = 0.6, outlier.shape = 21) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1) + # Show raw points behind
  facet_wrap(~year) +
  scale_fill_manual(values = c("Sorghum" = "grey70", "Sorghum + Rye" = "#5D9C59")) + # Custom colors
  labs(
    title = "A. Total Inorganic Nitrogen Distribution",
    y = expression(Total~Inorganic~N~(mg~N~kg^{-1})),
    x = NULL
  ) +
  theme_bw() +
  theme(legend.position = "none")

# Panel B: Density Plot of Nitrate Proportion
# This shows "How much of the N is Nitrate?"
p2 <- ggplot(df_analysis, aes(x = nitrate_prop, fill = treatment)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~year) +
  scale_fill_manual(values = c("Sorghum" = "grey70", "Sorghum + Rye" = "#5D9C59")) +
  labs(
    title = "B. Nitrate Proportion of Total Inorganic N",
    x = "Nitrate Portion (%)",
    y = "Density"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

# Combine
fig_s1 <- p1 / p2 

# Display
print(fig_s1)
ggsave("Figure_S1_Soil_Distributions.png", fig_s1, width = 7, height = 8)


# Print the range in nitrate concenteration as text output
nitrate_range <- range(df_analysis$nitrate_mg_per_kg, na.rm = TRUE)
cat(glue("Nitrate concentration ranged from {sprintf('%.2f', nitrate_range[1])} to {sprintf('%.2f', nitrate_range[2])} mg/kg.\n"))

# Print the range in ammonium concenteration as text output
ammonium_range <- range(df_analysis$ammonia_mg_per_kg, na.rm = TRUE)
cat(glue("Ammonium concentration ranged from {sprintf('%.2f', ammonium_range[1])} to {sprintf('%.2f', ammonium_range[2])} mg/kg.\n"))
