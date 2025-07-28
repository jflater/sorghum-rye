library(readxl)
library(tidyverse)

temp <- read_xlsx("sorghum-rye/data/rain/historical_rain.xlsx")

colnames(temp)

temp <- temp %>%
  select(day, doy, meanC) %>%
  mutate(
    date = as.Date(day),
    year = year(date),
  )

temp %>%
  ggplot(aes(x = doy, y = meanC, group = year)) +
  # All years gray
  geom_line(data = filter(temp, !(year %in% c(2023, 2024))),
            color = "gray",
            size = 0.8,
            alpha = .5) +
  # 2023 and 2024 colored
  geom_line(data = filter(temp, year == 2023), color = "#E41A1C", size = 1.2) +
  geom_line(data = filter(temp, year == 2024), color = "#377EB8", size = 1.2) +
  scale_x_continuous(
    breaks = seq(1, 366, by = 30),
    labels = function(x) format(as.Date(x - 1, origin = "2000-01-01"), "%b")
  ) +
  labs(
    x = "Month",
    y = "Mean Temperature (C)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

hist_env <- temp %>%
  filter(!(year %in% c(2023, 2024))) %>%
  group_by(doy) %>%
  summarize(
    min_temp = min(meanC, na.rm = TRUE),
    max_temp = max(meanC, na.rm = TRUE)
  )

temp_2023 <- filter(temp, year == 2023)
temp_2024 <- filter(temp, year == 2024)

