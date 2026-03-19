
library(tidyverse)
library(readxl)
library(patchwork) # For combining panels
library(lme4)      # Linear mixed effects models
library(lmerTest)  # P-values for fixed effects via Satterthwaite
library(emmeans)   # Treatment-specific estimates & pairwise comparisons
library(gt)       # For rendering tables in HTML

# --- 1. LOAD & CLEAN TEXTURE DATA (Your Code) ---
# Note: Ensure the path to the Excel file is correct in your local environment
texture_raw <- read_excel("data/PSA Report - AGRON-McDaniel-Potter-2024.xlsm",
    sheet = "Main Data File format", n_max = 22
)

# Ensure leaching data is available (re-load if necessary)
if (!exists("leaching_data")) {
  leaching_data <- read_csv("data/clean_leaching.csv") |>
    janitor::clean_names() |>
    mutate(
      date = as.Date(date),
      year = year(date)
    ) |>
    filter(year %in% c(2023, 2024))
}

# Calculate annual cumulative nitrogen leaching per plot
plot_area_ft2 <- 120 * 160
plot_area_ha <- plot_area_ft2 / 107639.1041671

annual_leaching <- leaching_data |>
  mutate(plot = as.numeric(plot)) |>
  group_by(plot, treatment, year) |>
  summarise(
    # mg -> kg, then divide by hectares
    annual_cumulative_leaching = (max(cumulative_n_loss_mg, na.rm = TRUE) / 1e6) / plot_area_ha, 
    .groups = "drop",
    annual_drainage_l = max(cumulative_flow_l, na.rm = TRUE),
    annual_drainage_mm = (annual_drainage_l / plot_area_ha) / 10000
  ) |>
  filter(is.finite(annual_cumulative_leaching), annual_cumulative_leaching >= 0) |>
  arrange(desc(annual_cumulative_leaching))

# render whole table in .html
annual_leaching 
gt(annual_leaching)
texture <- texture_raw %>%
    select(Sample, Sand = S, Silt = T, Clay = C) %>%
    separate(Sample, into = c("ID", "Plot"), sep = "_") %>%
    mutate(Plot = as.numeric(Plot)) %>%
    drop_na() %>%
    mutate(
        Sand = as.numeric(Sand),
        Silt = as.numeric(Silt),
        Clay = as.numeric(Clay)
    ) %>%
    select(Plot, Sand, Silt, Clay) # We only need Plot and Texture cols

# --- 2. JOIN WITH DRAINAGE & LEACHING DATA ---
# We use 'annual_leaching' from your previous analysis steps
# Ensure 'drainage_mm' is the correct column name for water volume in your dataset
plot_hydrology <- annual_leaching %>%
  # Select key vars. IF YOUR DRAINAGE COL IS NAMED DIFFERENTLY, CHANGE 'drainage_mm' BELOW
  select(year, plot, treatment, drainage_mm = annual_drainage_mm, leaching_kg_ha = annual_cumulative_leaching) %>% 
  left_join(texture, by = c("plot" = "Plot")) %>%
  mutate(
    year = as.factor(year),
    # Ensure treatment order for coloring
    treatment = factor(treatment, levels = c("Corn", "Soy", "Sorghum", "Sorghum + Rye"))
  )

# --- 3. LINEAR MIXED EFFECTS MODELS ---
# Random effect: plot (same plots measured in 2023 & 2024)
# Fixed effects: drainage_mm, treatment, and their interaction

m0  <- lme4::lmer(leaching_kg_ha ~ 1                          + (1 | plot), data = plot_hydrology, REML = FALSE)
m1  <- lme4::lmer(leaching_kg_ha ~ drainage_mm                + (1 | plot), data = plot_hydrology, REML = FALSE)
m2  <- lme4::lmer(leaching_kg_ha ~ drainage_mm + treatment    + (1 | plot), data = plot_hydrology, REML = FALSE)
m3  <- lme4::lmer(leaching_kg_ha ~ drainage_mm * treatment    + (1 | plot), data = plot_hydrology, REML = FALSE)

cat("\n--- Likelihood Ratio Tests ---\n")
cat("Adding drainage (m0 vs m1):\n");         print(anova(m0, m1))
cat("Adding treatment intercept (m1 vs m2):\n"); print(anova(m1, m2))
cat("Adding treatment:drainage slope (m2 vs m3):\n"); print(anova(m2, m3))

# Best-fit model summary with Satterthwaite p-values
m_best <- lmerTest::lmer(leaching_kg_ha ~ drainage_mm * treatment + (1 | plot),
               data = plot_hydrology, REML = TRUE)
cat("\n--- Best model (REML) summary ---\n"); print(summary(m_best))

# Treatment-specific slopes & intercepts via emmeans
slopes <- emtrends(m_best, pairwise ~ treatment, var = "drainage_mm")
cat("\n--- Treatment-specific slopes ---\n");        print(slopes$emtrends)
cat("\n--- Pairwise slope contrasts ---\n");         print(slopes$contrasts)

intercepts <- emmeans(m_best, ~ treatment, at = list(drainage_mm = 0))
cat("\n--- Treatment intercepts at drainage = 0 ---\n"); print(intercepts)
cat("\n--- Pairwise intercept contrasts ---\n");     print(pairs(intercepts))

# --- 4. GENERATE PLOTS ---

# PANEL A: Texture vs. Drainage (The "Site Characteristic" Check)
# Does sandier soil drain more? (Expect positive slope)
p_texture <- ggplot(plot_hydrology, aes(x = Sand, y = drainage_mm)) +
  geom_smooth(method = "lm", color = "black", fill = "grey80", alpha = 0.5) +
  geom_point(aes(fill = treatment), shape = 21, size = 3, color = "black", stroke = 0.5) +
  scale_fill_manual(values = c("Corn" = "#E69F00", "Soy" = "#D55E00", 
                               "Sorghum" = "grey70", "Sorghum + Rye" = "#009E73")) +
  labs(
    title = "A. Soil Texture vs. Drainage",
    x = "Sand Content (%)",
    y = "Annual Drainage (mm)"
  ) +
  theme_bw() +
  theme(legend.position = "none") # Hide legend here, show in Panel B

# PANEL B: Drainage vs. N Loss with treatment-specific LME lines
trt_colors <- c(
  "Corn" = "#E69F00", "Soy" = "#D55E00",
  "Sorghum" = "grey50", "Sorghum + Rye" = "#009E73"
)
trt_shapes <- c(
  "Corn" = 21, "Soy" = 24, "Sorghum" = 22, "Sorghum + Rye" = 21
)

# Extract model predictions for smooth treatment-specific lines
pred_grid <- expand.grid(
  drainage_mm = seq(
    min(plot_hydrology$drainage_mm),
    max(plot_hydrology$drainage_mm),
    length.out = 100
  ),
  treatment = levels(plot_hydrology$treatment)
)
pred_grid$leaching_kg_ha <- predict(
  m_best, newdata = pred_grid, re.form = NA  # population-level
)

p_loss <- ggplot(plot_hydrology, aes(x = drainage_mm, y = leaching_kg_ha)) +
  # Global dashed line (OLS, no treatment effect)
  geom_smooth(
    method = "lm", se = FALSE,
    color = "grey70", linetype = "dashed", linewidth = 0.8
  ) +
  # Treatment-specific LME population-level predictions
  geom_line(
    data = pred_grid,
    aes(color = treatment, group = treatment), linewidth = 1
  ) +
  # Points on top
  geom_point(
    aes(fill = treatment, shape = treatment),
    size = 3.5, alpha = 0.9, color = "black", stroke = 0.4
  ) +
  scale_color_manual(values = trt_colors, name = "Treatment") +
  scale_fill_manual(values  = trt_colors, name = "Treatment") +
  scale_shape_manual(values = trt_shapes, name = "Treatment") +
  labs(
    title = "B. Drainage vs. N Leaching Load",
    subtitle = "Lines = LME population-level predictions; dashed = global OLS",
    x = "Annual Drainage (mm)",
    y = expression(Annual ~ NO[3] - N ~ Leaching ~ (kg ~ N ~ ha^-1))
  ) +
  theme_bw() +
  theme(
    legend.position = "right",
    legend.box.background = element_rect(color = "black", linewidth = 0.5)
  )

# --- 4. COMBINE & SAVE ---
figure_4 <- p_texture + p_loss + 
  plot_layout(widths = c(1, 1.5)) # Give Panel B a bit more width

print(figure_4)
ggsave("FigureS_Hydrology.png", figure_4, width = 10, height = 5)

# --- 5. SUMMARY TABLE: Pairwise slope & intercept contrasts ---
slope_contrasts <- slopes$contrasts |>
  as.data.frame() |>
  mutate(effect = "slope")

intercept_contrasts <- pairs(intercepts) |>
  as.data.frame() |>
  mutate(effect = "intercept")

contrast_table <- bind_rows(slope_contrasts, intercept_contrasts) |>
  select(effect, contrast, estimate, SE, df, t.ratio, p.value) |>
  mutate(
    sig = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01  ~ "**",
      p.value < 0.05  ~ "*",
      TRUE            ~ "ns"
    )
  )

gt(contrast_table) |>
  tab_header(
    title    = "Pairwise Treatment Contrasts",
    subtitle = "LME model (Tukey-adjusted); slopes in kg/ha per mm drainage"
  ) |>
  tab_row_group(
    label = "Slope differences (kg N/ha per mm drainage)",
    rows  = effect == "slope"
  ) |>
  tab_row_group(
    label = "Intercept differences (kg N/ha at 0 mm drainage)",
    rows  = effect == "intercept"
  ) |>
  cols_hide(effect) |>
  cols_label(
    contrast = "Contrast",
    estimate = "Difference",
    SE       = "SE",
    df       = "df",
    t.ratio  = "t-ratio",
    p.value  = "p-value",
    sig      = ""
  ) |>
  fmt_number(columns = c(estimate, SE, t.ratio), decimals = 3) |>
  fmt_number(columns = df, decimals = 1) |>
  fmt_number(columns = p.value, decimals = 4) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_row_groups()
  ) |>
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(columns = sig, rows = p.value < 0.05)
  )
