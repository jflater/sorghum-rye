library(readxl)
library(tidyverse)
library(lubridate)
rain_bar <- read_xlsx("data/rain/historical_rain.xlsx")

# Filter to 2023 and 2024
rain_bar <- rain_bar %>%
  filter(year(day) %in% c(2023, 2024)) %>%
  select(day, precipmm) %>%
  mutate(day = as.Date(day))
class(rain_bar$day)

# bar plot with precipmm by day for each year
p1 <- rain_bar %>%
  mutate(year = year(day)) %>%
  ggplot(aes(x = day, y = precipmm, fill = as.factor(year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Daily Precipitation in 2023 and 2024",
    x = "Date",
    y = "Precipitation (mm)",
    fill = "Year"
  ) +
  theme_minimal() +
  scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p1
p1$data %>%
  write_csv("data/rain/daily_precip_2023_2024.csv")
