# Data Lineage: 2023_2024_SABR_tile_flow_n.csv

## Overview
This document traces the complete data lineage for `2023_2024_SABR_tile_flow_n.csv`, which contains daily tile flow data with nitrogen concentrations (measured and interpolated) for plots 1-15 from 2023-2024.

## Final Output File
**Location:** `data/clean/2023_2024_SABR_tile_flow_n.csv`

**Description:** Daily tile drainage flow measurements combined with nitrogen concentration data (nitrate and ammonia). Contains both measured values from lab analysis and interpolated values to fill temporal gaps.

**Key Fields:**
- `date`: Daily measurement date
- `plot`: Plot number (1-15)
- `flow_gallons`: Daily flow volume in gallons
- `flow_l`: Daily flow volume in liters
- `nitrate_mg_ml`: Measured nitrate concentration
- `ammonia_mg_l`: Measured ammonia concentration
- `sample_y_n`: Sample collection indicator
- `approx_nitrate_mg_l`: Linearly interpolated nitrate concentration
- `approx_ammonia_mg_l`: Linearly interpolated ammonia concentration

## Data Processing Pipeline

### Primary Creation Script
**Script:** `scripts/R/calculate_n_loss_water.R` (Line 43)

**Function:**
- Combines flow data with nitrogen concentration measurements
- Performs linear interpolation by plot and year to estimate missing nitrogen values
- Converts flow from gallons to liters (×3.78541)
- Creates analysis-ready dataset for nitrogen loss calculations

## Input Data Sources

### 1. Flow Data Chain

#### Final Flow Data
**File:** `data/clean/2023_2024_SABR_tile_flow.csv`
- **Created by:** `scripts/R/calculate_flow_from_clean_dat.r` (Line 30)
- **Process:** Aggregates sub-daily measurements to daily totals by plot

#### Processed Logger Data
**File:** `data/processed/Logger_Combined_Data23to24.csv`
- **Created by:** `scripts/R/clean_meter_dat_files.r` (Line 29)
- **Process:** Combines multiple .dat files from automated flow loggers
- **Column structure:** timestamp, record_number, plot_01 through plot_15

#### Raw Logger Files
**Source:** Box folder `SABR_SmallDrainage_PitFlow_2025_01_23_0105`
- **Format:** .dat files from Campbell Scientific data loggers
- **Frequency:** Sub-daily automated measurements
- **Content:** Raw flow measurements from tile drainage systems

### 2. Nitrogen Concentration Data

#### 2023 Nitrogen Data
**File:** `data/clean/water_n_2023.csv`
- **Created by:** `scripts/R/import_and_clean_water_quality_from_box.R` (Line 67)
- **Input Sources:**
  - Lab analysis: `data/raw/SABR_tile_MASTER_pre2024.csv` (Line 22)
  - Sample metadata: Box Excel file `TileDrainage_2023.xlsx` (Line 48)

#### 2024 Nitrogen Data  
**File:** `data/clean/water_n_2024.csv`
- **Created by:** `scripts/R/import_and_clean_water_quality_from_box.R` (Line 107)
- **Input Sources:**
  - Lab analysis: `data/raw/X2024_SABR_MASTER_water.csv` (Line 23)
  - Sample metadata: Box Excel file `TileDrainage_2024.xlsx` (Line 71)

## Data Processing Steps

### Flow Data Processing
1. **Raw logger data** (.dat files) → Combined into single dataset
2. **Pivot longer** to convert wide format (plot columns) to long format
3. **Daily aggregation** by date and plot (sum of sub-daily measurements)
4. **Unit conversion** from gallons to liters

### Nitrogen Data Processing
1. **Lab analysis results** joined with **sample collection metadata**
2. **Data cleaning** removes invalid samples and corner plots (NE, SE, SW, NW)
3. **Standardization** of column names and data types
4. **Quality control** filtering based on sample collection flags

### Final Integration
1. **Left join** flow data with nitrogen concentration data by date and plot
2. **Linear interpolation** within each plot-year group to estimate missing nitrogen values
3. **Data validation** and final formatting

## Quality Control Measures

- Excludes corner plots (NE, SE, SW, NW) that are not part of the experimental design
- Filters out samples marked as invalid (sample_y_n == "N")
- Uses linear interpolation grouped by plot and year to maintain temporal relationships
- Preserves original measured values alongside interpolated estimates

## Dependencies

### R Libraries
- tidyverse (data manipulation)
- janitor (data cleaning)
- zoo (time series interpolation)
- lubridate (date handling)

### External Data Sources
- Campbell Scientific data logger files
- Laboratory nitrogen analysis results
- Sample collection metadata from field records

## Usage Notes

This dataset serves as the foundation for:
- Nitrogen loss calculations from tile drainage
- Water quality analysis across experimental plots
- Temporal analysis of nutrient concentrations
- Comparison between measured and estimated values

The interpolated values (`approx_nitrate_mg_l`, `approx_ammonia_mg_l`) should be used cautiously and their limitations noted in any downstream analysis.

---
*Generated: 2025-01-09*
*Project: SABR Tile Water Study*