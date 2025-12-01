# Project TODO List

## Immediate Tasks

### 1. Re-run Data Processing
- [ ] Run `scripts/clean_leaching.r` to regenerate `data/clean_leaching.csv` with corrected `total_n_loss_mg` calculations
- [ ] Verify that NA values are properly handled in the new dataset

### 2. Figure 1 Improvements
- [ ] Fix x-axis alignment issues to match Figures 2 and 3
- [ ] Apply consistent formatting (faceting structure, theme elements)
- [ ] Ensure all panels use the same date breaks and alignment
- [ ] Consider combining precipitation/GDD into faceted structure like other figures

### 3. Figure Reorganization
**Current structure needs restructuring:**

#### Option A: Separate Flow and N-Loss Figures
- [ ] **Figure 3**: Flow + Precipitation (like Figure 2 structure)
  - Top panel: Daily precipitation (faceted by year)
  - Bottom panel: Cumulative tile drain flow (faceted by year)
  
- [ ] **Figure 4**: Monthly + Cumulative N-Loss 
  - Top panel: Monthly N loss (faceted by year, trimmed to concentration sampling period)
  - Bottom panel: Cumulative N loss (faceted by year, trimmed to concentration sampling period)
  
- [ ] **Figure 5**: Annual stacked N losses (current Figure 4)

#### Option B: Add Third Panel to Figure 3
- [ ] **Figure 3**: Three-panel leaching figure
  - Top: Cumulative flow (full year)
  - Middle: Monthly N loss (trimmed to sampling period) 
  - Bottom: Cumulative N loss (trimmed to sampling period)

### 4. Statistical Analysis Verification
- [ ] **Table significance testing**: Verify pairwise t-test calculations for soil nitrogen tables
- [ ] **Daily flux significance**: Add statistical testing and significance stars to Figure 2 daily flux panels
- [ ] **Monthly N-loss significance**: Add statistical testing and significance stars to monthly N-loss panels
- [ ] Review ANOVA source of variation calculations for all tables

## Code Quality Improvements

### 5. Date Range Consistency
- [ ] Create helper function for consistent date range calculations across figures
- [ ] Ensure all figures use the same approach as Figure 2 for date trimming
- [ ] Document the difference between measurement periods vs. full calendar year

### 6. Theme and Styling Consistency  
- [ ] Verify `theme_publication()` is applied consistently across all figures
- [ ] Standardize legend positioning and formatting
- [ ] Ensure consistent color schemes across figures
- [ ] Check axis label formatting and mathematical expressions

## Enhancement Ideas

### 7. Figure Quality Enhancements
- [ ] **Error bars**: Ensure consistent SE representation across all plots
- [ ] **Axis scaling**: Consider log-scale for variables with large ranges
- [ ] **Seasonal indicators**: Add crop planting/harvest dates to relevant figures
- [ ] **Weather context**: Consider adding temperature data to precipitation panels

### 8. Table Improvements  
- [ ] **Effect sizes**: Add effect size calculations alongside p-values
- [ ] **Confidence intervals**: Consider reporting 95% CI instead of/alongside SE
- [ ] **Multiple comparisons**: Review p-value adjustment methods
- [ ] **Table formatting**: Ensure consistent significant digit reporting

### 9. Data Visualization Best Practices
- [ ] **Color accessibility**: Verify color schemes work for colorblind readers
- [ ] **Panel labels**: Add clear panel labels (A, B, C, etc.) for multi-panel figures
- [ ] **Caption improvements**: Enhance figure captions with more detail about methods
- [ ] **Trend lines**: Consider adding trend lines or smooth fits where appropriate

### 10. Reproducibility and Documentation
- [ ] **Code comments**: Add more detailed comments explaining statistical choices
- [ ] **Method documentation**: Document interpolation methods and assumptions
- [ ] **Quality checks**: Add data validation steps to processing scripts
- [ ] **Version control**: Ensure all changes are properly tracked

## Notes
- Priority should be on getting basic figure structure correct before moving to enhancements
- Consider creating a separate script for figure themes and helper functions to reduce code duplication
- The flow data provides important context for understanding N-loss patterns - don't lose this in the reorganization
- Statistical significance testing should be consistent across all analyses (same α level, adjustment methods, etc.)