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