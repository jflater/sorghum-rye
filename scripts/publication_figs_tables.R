# ---- Setup ----
# Load required packages
library(tidyverse)
library(lubridate)
library(ggtext)
library(ggpubr)
library(emmeans)
library(multcomp)
library(gt)

pkgs <- c(
  "ggtext", "multcomp", "gt"
)

to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) {
  install.packages(to_install, dependencies = TRUE, repos = "https://cloud.r-project.org")
}

invisible(lapply(pkgs, require, character.only = TRUE))

# Create directories for output if they do not exist
if (!dir.exists("figures")) dir.create("figures")
if (!dir.exists("tables")) dir.create("tables")

# Example data to demonstrate structure ---------------------------------
# Replace this block with code that imports your experimental data.
set.seed(123)
example_dates <- tibble(
  year = rep(c(2023, 2024), each = 8),
  date = rep(seq(as.Date("2023-05-01"), by = "month", length.out = 8), 2),
  treatment = rep(c("Sorghum", "Sorghum+CR"), each = 4, times = 2)
) %>%
  group_by(year, date, treatment) %>%
  summarise(
    ammonium = rnorm(1, mean = 15, sd = 2),
    nitrate = rnorm(1, mean = 10, sd = 3),
    .groups = "drop"
  )

# The subsequent sections assume a data frame with the columns:
# year, date, treatment, ammonium, nitrate, and replication if needed.
#---------------------------------------------------------------------------

# ---- Table 2: Salt-extractable ammonium and nitrate -----------------------
# Summary of salt-extractable ammonium and nitrate with ANOVA p-values

soil_summary <- example_dates %>%
  group_by(year, date, treatment) %>%
  summarise(
    ammonium_mean = mean(ammonium),
    ammonium_se = sd(ammonium) / sqrt(n()),
    nitrate_mean = mean(nitrate),
    nitrate_se = sd(nitrate) / sqrt(n()),
    .groups = "drop"
  )

# ANOVA for ammonium and nitrate
aov_ammonium <- aov(ammonium ~ year * date * treatment, data = example_dates)
aov_nitrate <- aov(nitrate ~ year * date * treatment, data = example_dates)

pvals <- tibble(
  source = c("date", "treatment", "date:treatment"),
  ammonium_p = summary(aov_ammonium)[[1]][c(2, 3, 6), "Pr(>F)"],
  nitrate_p = summary(aov_nitrate)[[1]][c(2, 3, 6), "Pr(>F)"]
)

# Assemble table
table2 <- soil_summary %>%
  pivot_longer(
    cols = c(ammonium_mean, ammonium_se, nitrate_mean, nitrate_se),
    names_to = c("variable", ".value"),
    names_pattern = "(.*)_(mean|se)"
  ) %>%
  mutate(result = sprintf("%.2f ± %.2f", mean, se))

table2 <- table2 %>%
  dplyr::select(year, date, treatment, variable, result) %>%
  pivot_wider(names_from = variable, values_from = result)

table2_gt <- table2 %>%
  gt() %>%
  tab_header(title = md("**Table 2. Salt-extractable ammonium and nitrate**")) %>%
  cols_label(
    ammonium = md("Ammonium<br>(mg kg<sup>-1</sup>)"),
    nitrate = md("Nitrate<br>(mg kg<sup>-1</sup>)")
  )

gtsave(table2_gt, filename = "tables/table2_salt_extractable.html")

#---------------------------------------------------------------------------

# ---- Figure 1: Temporal soil nitrate flux -------------------------------
# Time-series of soil nitrate flux with shaded standard errors and fertilizer timing

# Create example flux data
daily_flux <- tibble(
  year = rep(c(2023, 2024), each = 200),
  date = rep(seq(as.Date("2023-04-01"), as.Date("2023-10-17"), length.out = 200), 2),
  treatment = rep(c("Sorghum", "Sorghum+CR"), each = 100, times = 2)
) %>%
  mutate(
    nitrate_flux = rnorm(n(), mean = 5, sd = 1.5),
    se = runif(n(), 0.2, 0.5)
  )

fert_dates <- tibble(
  year = c(2023, 2024),
  date = as.Date(c("2023-05-15", "2024-05-20"))
)

fig1 <- daily_flux %>%
  ggplot(aes(date, nitrate_flux, color = treatment)) +
  geom_ribbon(aes(ymin = nitrate_flux - se, ymax = nitrate_flux + se, fill = treatment),
    alpha = 0.2, colour = NA
  ) +
  geom_line(size = 1) +
  geom_vline(data = fert_dates, aes(xintercept = date), linetype = "dashed") +
  facet_wrap(~year, ncol = 2) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Month", y = expression(paste("Soil NO"[3], " flux")), color = "Treatment") +
  theme_classic() +
  theme(legend.position = "top")

ggsave("figures/figure1_soil_nitrate_flux.png", fig1, width = 8, height = 4, dpi = 300)

#---------------------------------------------------------------------------

# ---- Figure 2: Temporal soil ammonium flux ------------------------------
# Same structure as Figure 1 but for ammonium

fig2_data <- daily_flux %>%
  mutate(ammonium_flux = rnorm(n(), mean = 3, sd = 1))

fig2 <- fig2_data %>%
  ggplot(aes(date, ammonium_flux, color = treatment)) +
  geom_ribbon(aes(ymin = ammonium_flux - se, ymax = ammonium_flux + se, fill = treatment),
    alpha = 0.2, colour = NA
  ) +
  geom_line(size = 1) +
  geom_vline(data = fert_dates, aes(xintercept = date), linetype = "dashed") +
  facet_wrap(~year, ncol = 2) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Month", y = expression(paste("Soil NH"[4]^"+", " flux")), color = "Treatment") +
  theme_classic() +
  theme(legend.position = "top")

ggsave("figures/figure2_soil_ammonium_flux.png", fig2, width = 8, height = 4, dpi = 300)

#---------------------------------------------------------------------------

# ---- Figure 3: Cumulative N losses by cropping system -------------------
# Bar chart with significance letters for cumulative N2O losses

cumulative_losses <- tibble(
  year = rep(c(2023, 2024), each = 4),
  system = rep(c("Maize", "Soybean", "Sorghum", "Sorghum+CR"), times = 2),
  n2o = runif(8, 5, 15)
)

# ANOVA and letters
n2o_aov <- aov(n2o ~ system * year, data = cumulative_losses)
n2o_letters <- multcomp::cld(emmeans(n2o_aov, ~ system | year), Letters = letters)

fig3 <- cumulative_losses %>%
  left_join(n2o_letters, by = c("system", "year")) %>%
  ggplot(aes(system, n2o, fill = system)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = .group), vjust = -0.5, color = "black") +
  facet_wrap(~year) +
  scale_fill_brewer(palette = "Paired") +
  labs(y = expression(paste("Cumulative N"[2], "O-N (kg ha"^{
    -1
  }, ")")), x = "Cropping system") +
  theme_classic() +
  theme(legend.position = "none")

ggsave("figures/figure3_cumulative_n2o.png", fig3, width = 6, height = 4, dpi = 300)

#---------------------------------------------------------------------------

# ---- Figure 4: Cropping system comparison of biomass --------------------
# Example bar chart for aboveground biomass by cropping system

biomass <- tibble(
  year = rep(c(2023, 2024), each = 4),
  system = rep(c("Maize", "Soybean", "Sorghum", "Sorghum+CR"), times = 2),
  biomass = runif(8, 4, 12)
)

fig4 <- biomass %>%
  ggplot(aes(system, biomass, fill = system)) +
  geom_col(width = 0.7) +
  facet_wrap(~year) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Aboveground biomass (Mg ha^-1)", x = "Cropping system") +
  theme_classic() +
  theme(legend.position = "none")

ggsave("figures/figure4_biomass.png", fig4, width = 6, height = 4, dpi = 300)

#---------------------------------------------------------------------------

# ---- Table 3: Soil N losses and environmental damage cost ---------------
# Table summarising N2O loss, N leaching and environmental cost

n_loss <- cumulative_losses %>%
  mutate(
    n_leaching = runif(n(), 2, 8),
    env_cost = n2o * 1.5 + n_leaching * 0.8
  )

# Source of variation p-values
loss_aov <- lm(cbind(n2o, n_leaching, env_cost) ~ year * system, data = n_loss)
loss_p <- anova(loss_aov)

loss_table <- n_loss %>%
  select(year, system, n2o, n_leaching, env_cost)

table3_gt <- loss_table %>%
  gt(groupname_col = "year") %>%
  fmt_number(columns = c(n2o, n_leaching, env_cost), decimals = 2) %>%
  tab_header(title = md("**Table 3. Soil N losses and environmental damage cost**")) %>%
  cols_label(
    system = "Cropping system",
    n2o = html("N<sub>2</sub>O loss"),
    n_leaching = "N leaching",
    env_cost = "Environmental cost"
  )

gtsave(table3_gt, filename = "tables/table3_soil_N_losses.html")

#---------------------------------------------------------------------------

# End of script -----------------------------------------------------------
