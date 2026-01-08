# Submission_Figures_Tables.qmd Rendering Fix

## Issue
The document failed to render due to missing R packages that couldn't be installed from CRAN (network restrictions).

## Root Causes
1. **Missing `janitor` package**: Used for `clean_names()` function
2. **Missing `gt` package**: Used for publication-quality table formatting
3. **Font issue**: Arial font not available in system

## Solutions Applied

### 1. Removed janitor dependency
- The `janitor::clean_names()` calls were removed from three locations
- Data files already have clean column names (snake_case), so this function was redundant

### 2. Replaced gt tables with knitr::kable
- **Validation table (line ~1007)**: Replaced `gt()` pipeline with `knitr::kable()` output
  - Saved as CSV instead of PNG: `figures/validation_table_cumulative_n2o.csv`
  
- **Table 3 (line ~1567)**: Replaced complex `gt()` formatting with `knitr::kable()` 
  - Simplified styling but maintains all data and statistics
  - Saved as CSV: `tables/table3_nitrogen_losses.csv`

### 3. Fixed font issue
- Changed `base_family = "Arial"` to `base_family = "sans"` in `theme_publication()`
- System "sans" font is universally available

## Results
✅ **Document now renders successfully**
- HTML output: `Submission_Figures_Tables.html` (676 KB)
- All figures generated correctly
- All statistical analyses completed
- Tables displayed and saved as CSV files

## Note on PDF Output
- PDF rendering fails due to missing TeX installation
- HTML output is fully functional and self-contained
- PDF can be generated later if needed by installing TinyTeX

## Files Changed
- `Submission_Figures_Tables.qmd`: Updated to remove dependencies on unavailable packages

## New Files Created
- `Submission_Figures_Tables.html`: Successfully rendered HTML output
- `figures/validation_table_cumulative_n2o.csv`: Validation table data
- `tables/table3_nitrogen_losses.csv`: Table 3 nitrogen losses data
