# Recommended Changes for Submission_Figures_Tables.qmd

This document provides specific, actionable changes to address the issues identified in the code review.

## HIGH PRIORITY CHANGES

### 1. Standardize Pipe Operators

**Location**: Lines 114-138

**Current Code**:
```r
gdd <- gdd %>%
  select(day, doy, gdd_50_86) %>%
  mutate(
    date = as.Date(day),
    year = year(date),
  ) %>%
  group_by(year) %>%
  arrange(date) %>%
  mutate(cumulative_gdd = cumsum(gdd_50_86))

rain <- rain %>%
  select(day, doy, precipmm) %>%
  mutate(
    date = as.Date(day),
    year = year(date),
  ) %>%
  group_by(year) %>%
  arrange(date) %>%
  mutate(cumulative_precip = cumsum(precipmm))
```

**Recommended Change**:
```r
gdd <- gdd |>
  select(day, doy, gdd_50_86) |>
  mutate(
    date = as.Date(day),
    year = year(date),
  ) |>
  group_by(year) |>
  arrange(date) |>
  mutate(cumulative_gdd = cumsum(gdd_50_86))

rain <- rain |>
  select(day, doy, precipmm) |>
  mutate(
    date = as.Date(day),
    year = year(date),
  ) |>
  group_by(year) |>
  arrange(date) |>
  mutate(cumulative_precip = cumsum(precipmm))
```

---

### 2. Fix Figure 1 Dimensions

**Location**: Line 249-250

**Current Code**:
```r
ggsave("figures/figure1_weather.png", six_panel_plot, 
       width = 84, height = 105, dpi = 600, bg = "white", units = "mm")
```

**Recommended Change**:
```r
ggsave("figures/figure1_weather.png", six_panel_plot, 
       width = 174, height = 90, dpi = 600, bg = "white", units = "mm")
```

**Rationale**: All submission figures should use 174mm width (2-column manuscript standard)

---

### 3. Add Missing Package Dependencies

**Location**: Lines 36-42 (setup chunk)

**Current Code**:
```r
```{r setup, message=FALSE, warning=FALSE}
library(tidyverse)
library(readxl)
library(lubridate)
library(patchwork)
options(width = 80)
```

**Recommended Change**:
```r
```{r setup, message=FALSE, warning=FALSE}
library(tidyverse)
library(readxl)
library(lubridate)
library(patchwork)
library(gt)         # For table generation
library(emmeans)    # For Tukey comparisons
library(multcomp)   # For compact letter display
options(width = 80)
```

---

### 4. Add Defensive Plot Number Check

**Location**: Lines 102-103

**Current Code**:
```r
flux_data <- read_csv("data/cleaned_flux_data.csv") |>
  mutate(date = as.Date(date))
```

**Recommended Change**:
```r
flux_data <- read_csv("data/cleaned_flux_data.csv") |>
  mutate(
    date = as.Date(date),
    plot = str_pad(as.character(plot), width = 2, pad = "0")  # Ensure leading zeros
  )
```

**Rationale**: Ensures plot numbers match format expected for treatment joins (e.g., "01" not "1")

---

## MODERATE PRIORITY CHANGES

### 5. Extract Constants

**Location**: Add after line 95 (before data loading)

**Add New Section**:
```r
# Study constants
STUDY_YEARS <- c(2023, 2024)
PLOT_AREA_FT2 <- 120 * 160
PLOT_AREA_HA <- PLOT_AREA_FT2 / 107639.1041671

# Environmental costs from Preza-Fontes et al. (2023)
COST_N2O_PER_KG <- 16.18   # Dollars per kg N2O-N
COST_LEACH_PER_KG <- 18.54 # Dollars per kg leached N

# Plot aesthetics
MOISTURE_PLOT_BUFFER <- 1.12  # 12% headroom for significance stars
TEMP_MAX_LIMIT <- 40          # Fixed upper limit for temperature plots
```

Then update usage throughout:
- Line 329: `global_moisture_limits[2] * MOISTURE_PLOT_BUFFER`
- Line 330: `global_temp_limits[2] <- TEMP_MAX_LIMIT`
- Line 1260: Use `PLOT_AREA_HA` instead of recalculating
- Line 1838: Use `COST_N2O_PER_KG` and `COST_LEACH_PER_KG`

---

### 6. Document Statistical Test Selection

**Location**: Line 298 (before `get_date_pvals` function)

**Add Comment Block**:
```r
# Statistical approach for soil measurements:
# - t-tests used for daily treatment comparisons (reasonable with n=4 per treatment)
# - Non-parametric alternatives (Wilcoxon) used for leaching data with more variability
# - Tukey HSD for annual cumulative comparisons (controls family-wise error rate)
```

**Location**: Line 1321 (before Wilcoxon tests)

**Add Comment Block**:
```r
# Using Wilcoxon rank-sum test (non-parametric) because:
# 1. Leaching data shows high variability
# 2. Robust to outliers and non-normal distributions
# 3. Appropriate for small sample sizes (n=3-4 per treatment per date)
```

---

### 7. Fix Figure 3 Filename

**Location**: Line 923

**Current Code**:
```r
ggsave(
  filename = "figures/figure2_flux_precip_cumulative_2023_2024_plotlevelCI_dualaxis.png",
  plot = figure3_dual_axis,
  width = 174,
  height = 174,
  units = "mm",
  dpi = 600,
  bg = "white"
)
```

**Recommended Change**:
```r
ggsave(
  filename = "figures/figure3_n2o_flux_precipitation.png",
  plot = figure3_dual_axis,
  width = 174,
  height = 174,
  units = "mm",
  dpi = 600,
  bg = "white"
)
```

---

### 8. Remove or Document Duplicate Figure Code

**Option A: Remove Duplicates (Recommended)**

Remove lines 373-498 (original 4-panel soil figure) since the revised 6-panel version (lines 2703-2748) is superior.

Add comment at line 276:
```r
# NOTE: 4-panel soil figure code moved to "Figure revisions" section (line 2703)
# to maintain consistency with submission layout standards
```

**Option B: Keep Both with Clear Documentation**

Add comment at line 276:
```r
# Generate initial 4-panel version for exploratory analysis
# Final 6-panel version with difference summaries created in "Figure revisions" section
```

---

## LOW PRIORITY IMPROVEMENTS

### 9. Add File Existence Checks

**Location**: Lines 100-103 (data loading)

**Add Before Loading**:
```r
# Verify required data files exist
required_files <- c(
  "data/rain/historical_rain.xlsx",
  "data/cleaned_flux_data.csv",
  "data/clean_leaching.csv"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing required data files:\n", paste(missing_files, collapse = "\n"),
       "\nRun data cleaning scripts first: scripts/clean_flux.r, scripts/clean_leaching.r")
}
```

---

### 10. Add Treatment Filtering Comments

**Location**: Lines 281, 620, 938, 1282

**Add Comments**:
```r
# Focus on sorghum-based systems for direct comparison
# (Corn and Soy shown separately in supplementary analyses)
filter(treatment %in% c("Sorghum", "Sorghum + Rye")) |>
```

---

### 11. Document 2-Year Total Assumptions

**Location**: Line 1788

**Add Comment**:
```r
# Calculate 2-year totals using variance propagation (sum of means, quadrature for SD)
# Assumes independence between years (reasonable given different weather conditions)
n2o_two_year <- n2o_summary |>
  filter(year %in% c(2023, 2024)) |>
  group_by(treatment) |>
  summarise(
    mean_n2o = sum(mean_n2o, na.rm = TRUE),
    sd_n2o = sqrt(sum(sd_n2o^2, na.rm = TRUE)),  # Propagation of uncertainty
    .groups = "drop"
  )
```

---

### 12. Improve Significance Line Annotation

**Location**: Lines 386, 409, 437, 465

**Current Code**:
```r
geom_hline(yintercept = 0.5, color = "black", linewidth = 7) +
geom_segment(
  data = moisture_segments |> filter(year == 2023),
  aes(x = x, xend = xend, y = y, yend = y, color = Treatment),
  linewidth = 7, inherit.aes = FALSE, show.legend = FALSE
)
```

**Recommended Alternative**:
```r
# Use rectangles for clearer significance indicators
geom_rect(
  data = moisture_segments |> filter(year == 2023),
  aes(xmin = x, xmax = xend, ymin = y - 0.02, ymax = y + 0.02, fill = Treatment),
  inherit.aes = FALSE, show.legend = FALSE, alpha = 0.8
)
```

Or add comment explaining current approach:
```r
# Thick horizontal lines (linewidth = 7) indicate periods of significant difference
# Color-coded by treatment showing lower values
```

---

## IMPLEMENTATION CHECKLIST

After making changes, verify:

- [ ] All `%>%` replaced with `|>`
- [ ] Figure 1 dimensions updated to 174mm width
- [ ] Setup chunk includes gt, emmeans, multcomp
- [ ] Plot numbers defensively formatted with str_pad
- [ ] Constants extracted and used consistently
- [ ] Statistical test choices documented
- [ ] Figure 3 filename corrected
- [ ] Duplicate code removed or documented
- [ ] File existence checks added
- [ ] Treatment filtering decisions explained
- [ ] 2-year total assumptions documented

---

## TESTING RECOMMENDATIONS

After implementing changes:

1. **Render Test**: `quarto render Submission_Figures_Tables.qmd`
2. **Check Outputs**:
   - All figures saved to `SubmissionFiguresTables/`
   - Dimensions match specifications (174mm width)
   - No errors or warnings during rendering
3. **Visual Inspection**:
   - Panel tags consistent (a), b), c)...)
   - Color schemes match across figures
   - Significance indicators visible and accurate
4. **Data Validation**:
   - Treatment assignments correct (spot check plot numbers)
   - Summary statistics match expectations
   - Table values align with figure data

---

**Change Priority Summary**:
- **Must Fix (4 items)**: Pipes, dimensions, packages, plot formatting
- **Should Fix (4 items)**: Constants, documentation, filenames, duplicates
- **Nice to Have (4 items)**: Validation checks, comments, assumptions

**Estimated Implementation Time**: 30-45 minutes
