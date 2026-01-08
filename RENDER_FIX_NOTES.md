# Submission_Figures_Tables.qmd Rendering Fix

## Issue
The document failed to render due to missing R packages that couldn't be installed from CRAN (network restrictions).

## Root Causes
1. **Missing `janitor` package**: Used for `clean_names()` function
2. **Missing `gt` package**: Used for publication-quality table formatting
3. **Font issue**: Arial font not available in system

## Solutions Applied

### 1. Removed janitor dependency
- The `janitor::clean_names()` calls were removed from three locations (lines 1186, 1379, 1652)
- Data files already have clean column names (snake_case), so this function was redundant
- **Change**: Removed `janitor::clean_names() |>` from data loading pipelines

### 2. Replaced gt tables with knitr::kable
- **Validation table (line ~1007)**: Replaced `gt()` pipeline with `knitr::kable()` output
  - Removed `library(gt)` 
  - Replaced complex gt formatting with simple kable table
  - Saved as CSV instead of PNG: `figures/validation_table_cumulative_n2o.csv`
  
- **Table 3 (line ~1567)**: Replaced complex `gt()` formatting with `knitr::kable()` 
  - Removed `library(gt)`
  - Simplified styling but maintains all data and statistics
  - Added plain-text footnote for table explanation
  - Saved as CSV: `tables/table3_nitrogen_losses.csv`

### 3. Fixed font issue
- Changed `base_family = "Arial"` to `base_family = "sans"` in `theme_publication()` function
- System "sans" font is universally available and prevents grid graphics errors

## Results
✅ **Document now renders successfully**
- HTML output: `Submission_Figures_Tables.html` (2.8 MB self-contained)
- All 29 code chunks execute without errors
- All figures generated correctly
- All statistical analyses completed
- Tables displayed using knitr::kable and saved as CSV files

## Testing
The document was tested by:
1. Removing existing output: `rm -f Submission_Figures_Tables.html`
2. Re-rendering: `quarto render Submission_Figures_Tables.qmd --to html`
3. Result: ✅ Output created successfully

## Note on PDF Output
- PDF rendering fails due to missing TeX installation  
- The YAML header specifies both HTML and PDF formats
- HTML output is fully functional and self-contained
- PDF can be generated later if needed by installing TinyTeX with `quarto install tinytex`

## Files Modified
- `Submission_Figures_Tables.qmd`: 
  - Removed 3 instances of `janitor::clean_names()`
  - Removed 2 instances of `library(gt)`
  - Replaced gt table formatting with knitr::kable
  - Changed Arial font to sans font
  - Net change: -69 lines, +40 lines (simplified code)

## New Files Created
- `Submission_Figures_Tables.html`: Successfully rendered HTML output (2.8 MB)
- `figures/validation_table_cumulative_n2o.csv`: Validation table data
- `tables/table3_nitrogen_losses.csv`: Table 3 nitrogen losses data
- Multiple figure PNG files in `Submission_Figures_Tables_files/`

## Dependencies Successfully Used
- ✅ tidyverse (via apt: r-cran-tidyverse)
- ✅ readxl (via apt: r-cran-readxl)
- ✅ lubridate (via apt: r-cran-lubridate)
- ✅ patchwork (via apt: r-cran-patchwork)
- ✅ emmeans (via apt: r-cran-emmeans)
- ✅ multcomp (via apt: r-cran-multcomp)
- ✅ rstatix (via apt: r-cran-rstatix)
- ✅ knitr (via apt: r-cran-knitr)
- ✅ rmarkdown (via apt: r-cran-rmarkdown)

## Future Recommendations
- Consider keeping CSV table outputs as they're more portable than gt PNG exports
- If gt functionality is needed in future, ensure network access to CRAN or pre-install packages
- Document font requirements in project README

