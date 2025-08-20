# Date Comparison Analysis

## Overview
This document summarizes the process of comparing unique dates between two CSV files:
- `seasonal_flux_combined.csv` (column: year_month_day)
- `unique_dates_coords_by_file_date_flux.csv` (column: date)

## Methodology

### 1. Data Extraction
- **seasonal_flux_combined.csv**: Used `cut` command to extract the first column (year_month_day) and removed header row
- **unique_dates_coords_by_file_date_flux.csv**: Used `cut` command to extract the second column (date) and removed header row

### 2. Date Processing
- Applied `sort -u` to both datasets to get unique dates in sorted order
- Saved results to temporary files for comparison

### 3. Comparison Analysis
- Used `comm` command to identify differences between the two date sets:
  - `comm -23`: Dates in flux file but not in coords file
  - `comm -13`: Dates in coords file but not in flux file
  - `comm -12`: Common dates between both files

## Results Summary

| Metric | Count |
|--------|-------|
| Dates in seasonal_flux_combined.csv | 78 |
| Dates in unique_dates_coords_by_file_date_flux.csv | 90 |
| Common dates | 78 |
| Additional dates in coords file | 12 |

## Key Findings
- **Complete subset relationship**: All dates from `seasonal_flux_combined.csv` are present in `unique_dates_coords_by_file_date_flux.csv`
- **Additional data**: The coords file contains 12 additional dates not found in the flux file
- **No missing data**: No dates from the flux file are missing from the coords file

## Additional Dates in Coords File

The following table shows the 12 dates present in `unique_dates_coords_by_file_date_flux.csv` but not in `seasonal_flux_combined.csv`:

| Date | File | Status |
|------|------|------|
| 2023-04-12 | 20230412_small_plots.json | Only test data |
| 2023-06-19 | 2023_06_19_small_plots.json | Added to folder on desktop |
| 2023-06-21 | 2023_06_21_small_plots.json | Added to folder on desktop |
| 2023-06-23 | 2023_06_23_small_plots.json | Added to folder on desktop |
| 2023-06-26 | 2023_06_26_small_plots.json |
| 2023-06-28 | 2023_06_28_small_plots.json |
| 2023-07-06 | 2023_07_06_small_plots.json |
| 2023-08-16 | SABR_N2O_Methods.json |
| 2023-08-17 | SABR_N2O_Methods_Day2.json |
| 2024-02-07 | 20240131_small_plots.json |
| 2024-07-09 | Test-7-9-2024.json |
| 2024-12-17 | 20241217_small_plots.json |