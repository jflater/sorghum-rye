library(tidyverse)
library(janitor)

daily_precip <- read_csv("data/rain/daily_precip_2023_2024.csv") %>%
  select(day, precipmm)

seasonal_flux <- read_csv("data/seasonal_flux_combined.csv") %>%
  rename(day = year_month_day) %>%
  group_by(Treatment, day) %>%
  summarise(
    mean_flux = mean(gnha_day_no_negative, na.rm = TRUE),
    se_flux = sd(gnha_day_no_negative, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  filter(Treatment %in% c("Sorghum", "Sorghum + Rye") | is.na(Treatment)) %>%
  pivot_wider(names_from = Treatment, values_from = mean_flux)

compare_df_cols(
  daily_precip,
  seasonal_flux
)

# join by day
daily <- daily_precip %>%
  left_join(seasonal_flux, by = "day")

write_csv(daily, "data/daily_precip_flux_combined.csv")

library(tidyverse)
library(lubridate)

treatment_colors <- c(
  "Corn" = "#fda500",
  "Soy" = "#E9967A",
  "Sorghum" = "#D2B48C",
  "Sorghum + Rye" = "#8B4513"
)

theme_sabr <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      axis.title = element_text(size = base_size + 2),
      axis.text = element_text(size = base_size),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.title = element_text(size = base_size),
      legend.text = element_text(size = base_size - 1),
      strip.text = element_text(size = base_size + 2),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_blank()
    )
}
# --- data ---
raw <- read_csv("data/daily_precip_flux_combined.csv") %>%
  mutate(day = as.Date(day))

df_long <- raw %>%
  pivot_longer(c(Sorghum, `Sorghum + Rye`),
    names_to = "treatment", values_to = "value"
  )

fert_events <- data.frame(
  growing_season = c(2023, 2024),
  fertilizer_date = as.Date(c("2023-05-05", "2024-07-17"))
)
p4 <- ggplot(df_long, aes(x = day, y = value, color = treatment)) +
  geom_line(size = 1.2, alpha = 0.8, na.rm = TRUE) +
  geom_point(size = 2, position = position_dodge(width = 0.5)) +
  geom_errorbar(
    aes(ymin = value - se_flux, ymax = value + se_flux),
    width = 0.2,
    alpha = 0.5,
    position = position_dodge(width = 0.5)
  ) +
  geom_vline(
    data = fert_events,
    aes(xintercept = fertilizer_date, color = "Fertilizer Date"),
    linetype = "dashed",
    alpha = 0.7,
    show.legend = TRUE
  ) +
  geom_label(
    data = fert_events,
    inherit.aes = FALSE,
    aes(
      x = fertilizer_date,
      y = 90,
      label = format(fertilizer_date, "%d-%b-%Y")
    ),
    size = 3,
    hjust = 0,
    vjust = 0.5,
    show.legend = FALSE
  ) +
  scale_x_date(date_breaks = "4 week", date_labels = "%d-%b-%Y") +
  labs(
    x = "Date",
    y = expression("Daily N"[2] * "O Flux (g N ha"^"-1" * ")"),
    color = "Legend"
  ) +
  theme_sabr() +
  scale_color_manual(values = c(treatment_colors, "Fertilizer Date" = "red"))
print(p4)
