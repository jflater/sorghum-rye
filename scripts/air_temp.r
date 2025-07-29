library(readxl)
library(tidyverse)
library(lubridate)

gdd <- read_xlsx("sorghum-rye/data/rain/historical_rain.xlsx")

gdd <- gdd %>%
  select(day, doy, gdd_50_86) %>%
  mutate(
    date = as.Date(day),
    year = year(date),
  ) %>%
  group_by(year) %>%
  arrange(date) %>%
  mutate(cumulative_gdd = cumsum(gdd_50_86))

gdd <- gdd %>%
  mutate(doy_date = as.Date(doy - 1, origin = "2000-01-01"))

gdd %>%
  ggplot(aes(x = doy_date, y = cumulative_gdd, group = year)) +
  # All years gray
  geom_line(
    data = filter(gdd, !(year %in% c(2023, 2024))),
    color = "gray",
    size = 0.8,
    alpha = .5
  ) +
  # 2023 and 2024 colored
  geom_line(data = filter(gdd, year == 2023), color = "#E41A1C", size = 1.2) +
  geom_line(data = filter(gdd, year == 2024), color = "#377EB8", size = 1.2) +
  scale_x_date(
    date_breaks = "1 month", # Breaks at each month
    date_labels = "%b" # Label as abbreviated month names
  ) +
  labs(
    x = "Month",
    y = "Growing Degree Days (GDD)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

mean_gdd <- gdd %>%
  group_by(doy_date) %>%
  summarize(mean_gdd_d = mean(cumulative_gdd, na.rm = TRUE)) %>%
  mutate(year = "Mean") %>%
  filter(doy_date != "2000-12-31")

hist_env <- gdd %>%
  filter(!(year %in% c(2023, 2024))) %>%
  group_by(doy_date) %>%
  summarize(
    min_gdd = min(gdd_50_86, na.rm = TRUE),
    max_gdd = max(gdd_50_86, na.rm = TRUE)
  )

gdd_2023 <- filter(gdd, year == 2023)
gdd_2024 <- filter(gdd, year == 2024)

ggplot() +
  # Shaded region for historical envelope
  geom_ribbon(
    data = hist_env,
    aes(x = doy, ymin = min_gdd, ymax = max_gdd),
    fill = "gray80", alpha = 0.6
  ) +
  # 2023 solid line
  geom_line(
    data = gdd_2023,
    aes(x = doy, y = mean_gdd),
    color = "#E41A1C", size = 1.2, linetype = "solid"
  ) +
  # 2024 dashed line
  geom_line(
    data = gdd_2024,
    aes(x = doy, y = mean_gdd),
    color = "#377EB8", size = 1.2, linetype = "dashed"
  ) +
  scale_x_continuous(
    breaks = seq(1, 366, by = 30),
    labels = function(x) format(as.Date(x - 1, origin = "2000-01-01"), "%b")
  ) +
  labs(
    title = "Cumulative GDD: Historical Range vs. Recent Years",
    x = "Day of Year",
    y = "Cumulative GDD ()"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Custom legend via manual color/linetype mapping:
  scale_color_manual(
    name = "Year",
    values = c("2023" = "#E41A1C", "2024" = "#377EB8")
  ) +
  scale_linetype_manual(
    name = "Year",
    values = c("2023" = "solid", "2024" = "dashed")
  )
