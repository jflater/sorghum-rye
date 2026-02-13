# Impact of Cover Crops on Nitrous Oxide Emissions in Sorghum Systems

This repository contains data and analysis scripts for a study investigating the impact of cover crops on nitrous oxide (N₂O) emissions in sorghum systems. The study compares N₂O fluxes from sorghum and sorghum intercropped with rye, as well as other treatments including corn and soy.

## 📊 Reproducible Research Companion

**New!** We've created a comprehensive [Reproducible Research Companion](Reproducible_Research_Companion.qmd) document that consolidates all analysis code for the manuscript **"Integrating Winter Cover Crops With Biomass Sorghum Can Reduce Nitrogen Losses"**.

This document includes:
- ✅ Complete code for all 5 figures and 2 tables in the manuscript
- ✅ Scientific context for each analysis
- ✅ Clean, publication-ready code with exploratory analysis removed
- ✅ Consistent styling and transparent methodology
- ✅ Self-contained HTML output suitable for sharing

### Viewing the Companion

Visit the [GitHub Pages site](https://jflater.github.io/sorghum-rye/Reproducible_Research_Companion.html) to view the rendered document, or open `Reproducible_Research_Companion.qmd` locally to see the source code.

### Rendering Locally

To render the document yourself:

```bash
# Install Quarto if needed: https://quarto.org/docs/get-started/
quarto render Reproducible_Research_Companion.qmd
```

**Note:** Figure 5 (Biomass Yields) requires sorghum biomass data files that are stored externally. The code is provided with `eval: false` to show the complete workflow. Users with access to these files can update the paths and enable execution.

## Data: `data/clean_leaching.csv`

This file contains cleaned, plot‑level tile drainage data used for the nitrate leaching analyses (Figure 3 and related tables). Each row represents a date × plot observation with flow, concentration, and derived nitrogen loss fields.

**Key columns (units):**
- `date`: Sampling date (YYYY‑MM‑DD).
- `year`: Year extracted from `date`.
- `plot`: Plot identifier (string; leading zeros preserved in some plots).
- `treatment`: Treatment label (e.g., `Sorghum`, `Sorghum + Rye`, `Corn`, `Soy`).
- `flow_gallons`: Drain flow volume (gallons).
- `flow_l`: Drain flow volume (liters).
- `nitrate_mg_ml`: Nitrate concentration; **stored as mg/L** (naming legacy).
- `ammonia_mg_l`: Ammonium concentration (mg/L).
- `nitrate_loss_mg`: Nitrate‑N mass loss (mg).
- `ammonia_loss_mg`: Ammonium‑N mass loss (mg).
- `total_n_loss_mg`: Total inorganic N loss (mg) = nitrate + ammonium.
- `cumulative_flow_l`: Cumulative flow by plot and year (liters).
- `cumulative_n_loss_mg`: Cumulative total N loss by plot and year (mg).
- `sample_y_n`, `approx_nitrate_mg_l`, `approx_ammonia_mg_l`: Sampling flags and imputed concentration fields used during cleaning.

**Leaching loss calculation used in the manuscript:**

1. **Per‑day N loss (kg N ha⁻¹ d⁻¹):** `daily_n_loss_kgha = (total_n_loss_mg / 1e6) / plot_area_ha`
2. **Annual cumulative leaching per plot (kg N ha⁻¹):** `annual_cumulative_leaching = (max(cumulative_n_loss_mg) / 1e6) / plot_area_ha`

Where `plot_area_ft2 = 120 * 160` and `plot_area_ha = plot_area_ft2 / 107639.1041671`.

These are the values used for annual treatment comparisons, statistical tests, and the leaching panels in the manuscript.
