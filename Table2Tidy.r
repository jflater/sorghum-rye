---
title: "Soil Inorganic Nitrogen Analysis"
author: "Agricultural Data Expert"
format:
  html:
    toc: true
    toc-depth: 2
  docx:
    reference-doc: default
execute:
  echo: true
  warning: false
  message: false
---

# Purpose

Clean, analyze, and generate publication-ready tables for soil inorganic nitrogen (NH4-N, NO3-N, Total Inorganic N).

## 1. Setup

```{r}
# Load packages (install pacman if needed)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,  # Data manipulation
  rstatix,    # Tidy statistics
  emmeans,    # Estimated marginal means (for post-hoc)
  multcomp,   # For CLD (Compact Letter Display)
  gt,         # Publication-quality tables
  glue        # String interpolation
)
```

## 2. Load and Preprocess

```{r}
# Read cleaned soil data
data <- readr::read_csv("cleaned_soil_data.csv", show_col_types = FALSE)

# Create Total N, pivot long, and clean labels
clean_data <- data %>%
  # Create Total N variable
  dplyr::mutate(total_n_ppm = ammonia_ppm + nitrate_ppm) %>%
  # Convert to long format for iteration
  tidyr::pivot_longer(
    cols = c(ammonia_ppm, nitrate_ppm, total_n_ppm),
    names_to = "analyte",
    values_to = "ppm"
  ) %>%
  # Clean up analyte names for display and ensure treatment is a factor
  dplyr::mutate(
    analyte = dplyr::case_when(
      analyte == "ammonia_ppm" ~ "Ammonium (NH4-N)",
      analyte == "nitrate_ppm" ~ "Nitrate (NO3-N)",
      analyte == "total_n_ppm" ~ "Total Inorganic N",
      TRUE ~ analyte
    ),
    treatment = as.factor(treatment)
  )
```

## 3. Statistical Analysis Function

We wrap the stats in a function to apply safely across groups.

```{r}
run_stats <- function(df) {
  # A. Normality Check (Shapiro-Wilk)
  # Note: If n < 3, shapiro.test fails; add error handling.
  shapiro <- tryCatch(
    stats::shapiro.test(df$ppm)$p.value,
    error = function(e) NA_real_
  )

  # B. ANOVA
  # For this standard request, we stick to raw scale unless specified.
  model <- stats::lm(ppm ~ treatment, data = df)
  anova_res <- stats::anova(model)
  p_val <- tryCatch(anova_res$`Pr(>F)`[1], error = function(e) NA_real_)

  # C. Post-hoc (Tukey HSD with Letters)
  # Only run if model is significant (p < 0.05); otherwise blank letters.
  if (!is.na(p_val) && p_val < 0.05) {
    emm <- emmeans::emmeans(model, ~ treatment)
    cld_res <- multcomp::cld(
      emm,
      Letters = letters,
      adjust = "tukey",
      reversed = TRUE
    ) %>%
      tibble::as_tibble() %>%
      dplyr::select(treatment, .group) %>%
      dplyr::rename(letter = .group) %>%
      dplyr::mutate(letter = stringr::str_trim(letter))
  } else {
    cld_res <- tibble::tibble(
      treatment = unique(df$treatment),
      letter = ""  # Leave blank if non-significant
    )
  }

  # D. Summary Stats
  stats_summary <- df %>%
    dplyr::group_by(treatment) %>%
    rstatix::get_summary_stats(ppm, type = "mean_se") %>%
    dplyr::left_join(cld_res, by = "treatment") %>%
    dplyr::mutate(
      shapiro_p = shapiro,
      anova_p = p_val
    )

  return(stats_summary)
}
```

## 4. Execute Analysis

```{r}
results <- clean_data %>%
  dplyr::group_by(date, analyte) %>%
  tidyr::nest() %>%
  dplyr::mutate(stats = purrr::map(data, run_stats)) %>%
  tidyr::unnest(stats) %>%
  dplyr::select(-data)

# Quick peek
dplyr::glimpse(results)
```

## 5. Format for Publication

Create the display string: "Mean ± SE (Letter)".

```{r}
formatted_table <- results %>%
  dplyr::mutate(
    display_value = glue::glue("{sprintf('%.2f', mean)} ± {sprintf('%.2f', se)} {letter}"),
    display_value = stringr::str_trim(display_value)
  ) %>%
  dplyr::select(date, treatment, analyte, display_value) %>%
  tidyr::pivot_wider(
    names_from = analyte,
    values_from = display_value
  )

# Preview
formatted_table %>% dplyr::arrange(date, treatment) %>% head(10)
```

## 6. Generate Tables

Generate an RTF table using gt and save a CSV for checking.

```{r}
gt_tbl <- formatted_table %>%
  dplyr::group_by(date) %>%
  gt::gt() %>%
  gt::cols_label(
    treatment = "Treatment"
  ) %>%
  gt::tab_header(
    title = "Soil Inorganic Nitrogen Response to Treatments",
    subtitle = "Values represent Mean ± Standard Error (ppm). Letters indicate significant differences (p < 0.05, Tukey HSD)."
  ) %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_column_labels()
  ) %>%
  gt::tab_options(
    row_group.background.color = "#E0E0E0",
    table.font.names = "Times New Roman"
  )

# Save outputs
gt::gtsave(gt_tbl, "soil_nitrogen_results.rtf")
readr::write_csv(formatted_table, "soil_nitrogen_formatted.csv")

print("Analysis Complete. Generated 'soil_nitrogen_results.rtf' and 'soil_nitrogen_formatted.csv'.")
```