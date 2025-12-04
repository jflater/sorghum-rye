# Sorghum-Rye N₂O Emissions Study - AI Agent Instructions

## Project Overview
Agricultural research project analyzing nitrous oxide (N₂O) emissions and nitrogen leaching in sorghum systems with/without cereal rye cover crops. The codebase processes field measurements (flux chambers, tile drainage, soil samples) and generates publication-ready figures and tables using R and Quarto.

## Architecture & Data Flow

### Core Pipeline (Sequential Processing)
1. **Raw data ingestion** (`data/raw/*.json`) → LI-COR survey chamber N₂O measurements
2. **Soil Flux Pro processing** → `.json` exported as `.csv` files → placed in `data/processed/`
3. **Processing scripts** (`scripts/*.r`) → Clean and standardize data with treatment assignments
4. **Cleaned datasets** (`data/*.csv`) → Analysis-ready data with consistent plot numbering
5. **Quarto documents** (`*.qmd`) → Publication figures/tables with embedded statistics
6. **GitHub Pages** (future: self-contained folder in repo root) → Rendered HTML via `_quarto.yml` configuration

### Critical Data Transformations
- **Plot numbering**: Always use `str_pad(plot, width = 2, pad = "0")` to ensure leading zeros (e.g., "01" not "1")
- **Flux units**: Convert nmol·m⁻²·s⁻¹ → g N·ha⁻¹·day⁻¹ via `nmols_to_grams_hectare_day()` in `scripts/myfunctions.r`
- **Treatment assignment**: Join with `data/metadata/plot_treatments.csv` by plot + year (treatments rotated annually)
- **Column cleaning**: Apply `janitor::clean_names()` immediately after reading any CSV
- **Pipe operator**: Use base R pipe `|>` in new code (legacy code may use `%>%`)

### Multi-Year Handling
Treatments change by year (2023 vs 2024). Always filter `plot_treatments.csv` by year before joining:
```r
trt_year <- bind_rows(
  trt |> filter(date == "2023-07-01") |> mutate(year = 2023),
  trt |> filter(date == "2024-07-01") |> mutate(year = 2024)
)
```

## Development Workflows

### Regenerating Cleaned Data
Run in order (dependencies cascade):
```r
source("scripts/clean_flux.r")        # Creates data/cleaned_flux_data.csv
source("scripts/clean_leaching.r")    # Creates data/clean_leaching.csv
source("scripts/clean_soil.R")        # Creates data/soils/cleaned_soil_data.csv
```

### Building Publication Documents
```bash
# Render specific document
quarto render Final_Figures_Tables.qmd

# Render entire site (defined in _quarto.yml)
quarto render

# Preview with live reload
quarto preview
```

### Adding New Flux Data
1. Place LI-COR `.csv` exports in `data/processed/` (LI-COR `.json` files processed in Soil Flux Pro software)
2. Run `scripts/process_raw_flux.r` (auto-detects new dates, avoids duplicates, archives input files)
3. Re-run `scripts/clean_flux.r` to merge with treatments

## Project-Specific Conventions

### Statistical Approach (Non-Parametric)
**Prefer non-parametric tests** to avoid data transformation assumptions:
```r
# Kruskal-Wallis for overall differences
kruskal.test(response ~ treatment, data = df)

# Dunn's test for pairwise comparisons (with Holm adjustment)
library(rstatix)
dunn_test(response ~ treatment, data = df, p.adjust.method = "holm")
```

Avoid transforming data - use rank-based methods when normality/homogeneity assumptions fail.

### Visualization Standards
- **Theme**: Always apply `theme_publication()` (defined in `.qmd` files, not in separate script)
- **Colors**: Use `treatment_colors` vector for consistency:
  - Sorghum: `#D8D97AFF` (yellow-tan)
  - Sorghum + Rye: `#95C36EFF` (green)
  - Corn: `#74C8C3FF` (teal)
  - Soy: `#0A2E57FF` (navy)
- **Multi-panel**: Use `patchwork` (`+` for horizontal, `/` for vertical layout)
- **Date ranges**: Facet by year, align panels using shared x-axis limits

### Quarto Configuration
- **Output**: `_quarto.yml` defines website structure and navbar
- **Self-contained**: Use `embed-resources: true` in YAML for standalone HTML
- **Code folding**: Default to `code-fold: true` in publication documents

## Key Files Reference

| File | Purpose | Important Patterns |
|------|---------|-------------------|
| `data/metadata/plot_treatments.csv` | Daily treatment assignments (10,982 rows) | Filter by year before joining |
| `scripts/myfunctions.r` | Unit conversion utilities | `nmols_to_grams_hectare_day()` |
| `Final_Figures_Tables.qmd` | Primary publication document (2,615 lines) | Statistical helper functions, `theme_publication()` |
| `TODO.md` | Active development tracker | Known issues with figure alignment, date trimming |

## Common Pitfalls

1. **Missing leading zeros**: Plot "1" won't join with "01" in treatment metadata
2. **Year-agnostic joins**: Treatments rotate annually—always join on both plot AND year
3. **Using `%>%` in new code**: Prefer base R pipe `|>` for new code
4. **Duplicate flux processing**: `process_raw_flux.r` checks existing dates to prevent re-processing
5. **Hardcoded paths**: Scripts assume working directory is project root (use relative paths)

## Testing & Validation

No formal test suite. Validate changes by:
1. Checking row counts after joins (use `anti_join()` to find unmatched records)
2. Verifying treatment distributions: `table(df$treatment, useNA = "ifany")`
3. Comparing date ranges: `range(df$date, na.rm = TRUE)`
4. Re-rendering affected `.qmd` files to catch visualization breaks

## External Dependencies

- **R Packages**: `tidyverse`, `janitor`, `lubridate`, `gt`, `patchwork`, `emmeans`, `rstatix`, `multcompView`
- **Quarto CLI**: Required for rendering (not bundled with R)
- **Soil Flux Pro**: External LI-COR software for processing raw `.json` chamber data to `.csv`

## Active Development Areas (per TODO.md)

- Figure 1 x-axis alignment issues
- Cumulative N-loss date trimming (should match sampling period, not full year)
- Adding significance annotations to daily flux panels
- Standardizing date range calculations across figures
