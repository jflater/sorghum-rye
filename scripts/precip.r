library(tidyverse)
library(readxl)
library(lubridate)

rain <- read_xlsx("sorghum-rye/data/rain/historical_rain.xlsx")

rain <- rain %>%
  select(day, precipmm) %>%
  mutate(
    date = as.Date(day),
    year = year(date),
    doy = yday(date),  # Add day of year
    dummy_date = as.Date(paste0("2000-", sprintf("%03d", yday(date))), "%Y-%j") # All dates in 2000
  ) %>%
  group_by(year) %>%
  arrange(date) %>%
  mutate(cumulative_precip = cumsum(precipmm))

rain %>%
  ggplot(aes(x = dummy_date, y = cumulative_precip, group = year)) +
  # All years gray
  geom_line(data = filter(rain, !(year %in% c(2023, 2024))),
            color = "gray", size = 0.8) +
  # 2023 and 2024 colored
  geom_line(data = filter(rain, year == 2023), color = "#E41A1C", size = 1.2) +
  geom_line(data = filter(rain, year == 2024), color = "#377EB8", size = 1.2) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b"
  ) +
  labs(
    title = "Cumulative Rainfall by Year",
    x = "Date (January–December)",
    y = "Cumulative Precipitation (mm)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

rain %>%
  ggplot(aes(x = doy, y = cumulative_precip, group = year)) +
  # All years gray
  geom_line(data = filter(rain, !(year %in% c(2023, 2024))),
            color = "gray", size = 0.8) +
  # 2023 and 2024 colored
  geom_line(data = filter(rain, year == 2023), color = "#E41A1C", size = 1.2) +
  geom_line(data = filter(rain, year == 2024), color = "#377EB8", size = 1.2) +
  scale_x_continuous(
    breaks = seq(1, 366, by = 30),
    labels = function(x) format(as.Date(x - 1, origin = "2000-01-01"), "%b")
  ) +
  labs(
    title = "Cumulative Rainfall by Year",
    x = "Day of Year",
    y = "Cumulative Precipitation (mm)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

mean_cum <- rain %>%
  group_by(doy) %>%
  summarize(cumulative_precip = mean(cumulative_precip, na.rm = TRUE)) %>%
  mutate(year = "Mean") %>%
  filter(doy != 366) # For merging and labeling

# Combine all into one data frame for plotting
rain_long <- rain %>%
  filter(year %in% c(2023, 2024)) %>%   # Only plot 2023 and 2024 as lines in color
  mutate(year = as.character(year)) %>%
  bind_rows(mean_cum) # add the mean as its own 'year'

# For all OTHER years, add a gray background
background <- rain %>%
  filter(!(year %in% c(2023, 2024))) 

rain_plot <- ggplot() +
  # Gray lines for all other years
  geom_line(
    data = background,
    aes(x = doy, y = cumulative_precip, group = year),
    color = "gray", size = 0.8, alpha = 0.6
  ) +
  # Colored lines for 2023, 2024, and mean
  geom_line(
    data = rain_long,
    aes(x = doy, y = cumulative_precip, color = year, group = year),
    size = 1.2
  ) +
  scale_color_manual(
    name = "Year",
    values = c("2023" = "#E41A1C", "2024" = "#377EB8", "Mean" = "black"),
    labels = c("2023", "2024", "Mean (All Years)")
  ) +
  scale_x_continuous(
    breaks = seq(1, 366, by = 30),
    labels = function(x) format(as.Date(x - 1, origin = "2000-01-01"), "%b")
  ) +
  labs(
    x = "Day of Year",
    y = "Cumulative Precipitation (mm)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

rain_plot

#### Version 2 
# Exclude current years from the envelope
hist_env <- rain %>%
  filter(!(year %in% c(2023, 2024))) %>%
  group_by(doy) %>%
  summarize(
    min_cum = min(cumulative_precip, na.rm = TRUE),
    max_cum = max(cumulative_precip, na.rm = TRUE)
  )

rain_2023 <- filter(rain, year == 2023)
rain_2024 <- filter(rain, year == 2024)

ggplot() +
  # Shaded region for historical envelope
  geom_ribbon(
    data = hist_env,
    aes(x = doy, ymin = min_cum, ymax = max_cum),
    fill = "gray80", alpha = 0.6
  ) +
  # 2023 solid line
  geom_line(
    data = rain_2023,
    aes(x = doy, y = cumulative_precip),
    color = "#E41A1C", size = 1.2, linetype = "solid"
  ) +
  # 2024 dashed line
  geom_line(
    data = rain_2024,
    aes(x = doy, y = cumulative_precip),
    color = "#377EB8", size = 1.2, linetype = "dashed"
  ) +
  scale_x_continuous(
    breaks = seq(1, 366, by = 30),
    labels = function(x) format(as.Date(x - 1, origin = "2000-01-01"), "%b")
  ) +
  labs(
    title = "Cumulative Rainfall: Historical Range vs. Recent Years",
    x = "Day of Year",
    y = "Cumulative Precipitation (mm)"
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

rain_highlight <- rain %>%
  filter(year %in% c(2023, 2024)) %>%
  mutate(year = as.character(year))

ggplot() +
  geom_ribbon(
    data = hist_env,
    aes(x = doy, ymin = min_cum, ymax = max_cum),
    fill = "gray80", alpha = 0.6
  ) +
  geom_line(
    data = rain_highlight,
    aes(x = doy, y = cumulative_precip, color = year, linetype = year),
    size = 1.2
  ) +
  scale_color_manual(
    name = "Year",
    values = c("2023" = "#E41A1C", "2024" = "#377EB8"),
    labels = c("2023", "2024")
  ) +
  scale_linetype_manual(
    name = "Year",
    values = c("2023" = "solid", "2024" = "dashed"),
    labels = c("2023", "2024")
  ) +
  scale_x_continuous(
    breaks = seq(1, 366, by = 30),
    labels = function(x) format(as.Date(x - 1, origin = "2000-01-01"), "%b")
  ) +
  labs(
    title = "Cumulative Rainfall: Historical Range vs. Recent Years",
    x = "Day of Year",
    y = "Cumulative Precipitation (mm)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

mean_cum <- rain %>%
  filter(!(year %in% c(2023, 2024))) %>% # use only historical for mean, or all if preferred
  group_by(doy) %>%
  summarize(cumulative_precip = mean(cumulative_precip, na.rm = TRUE)) %>%
  mutate(year = "Mean") %>%
  filter(doy != 366) # For merging and labeling

rain_highlight <- rain %>%
  filter(year %in% c(2023, 2024)) %>%
  mutate(year = as.character(year)) %>%
  bind_rows(mean_cum)

rain_plot_v2 <- ggplot() +
  geom_ribbon(
    data = hist_env,
    aes(x = doy, ymin = min_cum, ymax = max_cum),
    fill = "gray80", alpha = 0.6
  ) +
  geom_line(
    data = rain_highlight,
    aes(x = doy, y = cumulative_precip, color = year, linetype = year),
    size = 1.2
  ) +
  scale_color_manual(
    name = "Year",
    values = c(
      "2023" = "#E41A1C",
      "2024" = "#377EB8",
      "Mean" = "black"
    ),
    labels = c(
      "2023" = "2023",
      "2024" = "2024",
      "Mean" = "Mean (Historical)"
    )
  ) +
  scale_linetype_manual(
    name = "Year",
    values = c(
      "2023" = "solid",
      "2024" = "dashed",
      "Mean" = "dotdash"
    ),
    labels = c(
      "2023" = "2023",
      "2024" = "2024",
      "Mean" = "Mean (Historical)"
    )
  ) +
  scale_x_continuous(
    breaks = seq(1, 366, by = 30),
    labels = function(x) format(as.Date(x - 1, origin = "2000-01-01"), "%b")
  ) +
  labs(
    x = "Day of Year",
    y = "Cumulative Precipitation (mm)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

rain_plot_v2
######### Smooth verstion
library(tidyverse)
library(readxl)
library(lubridate)


# Get a smoothed line for each year (using loess)
# rain_smooth <- rain %>%
#   group_by(year) %>%
#   arrange(doy) %>%
#   mutate(
#     smoothed_precip = as.numeric(loess(cumulative_precip ~ doy, span = 0.3)$fitted)
#   )

rain_smooth <- rain %>%
  group_by(year) %>%
  arrange(doy) %>%
  mutate(
    smoothed_precip = as.numeric(isoreg(doy, cumulative_precip)$yf)
  )
# Now calculate the min/max range for each doy from smoothed curves, excluding 2023 and 2024 if desired
hist_env_smooth <- rain_smooth %>%
  filter(!(year %in% c(2023, 2024))) %>%
  group_by(doy) %>%
  summarize(
    min_cum = min(smoothed_precip, na.rm = TRUE),
    max_cum = max(smoothed_precip, na.rm = TRUE)
  )

# Prepare data for plotting: 2023, 2024, Mean (from smoothed curves)
mean_smooth <- rain_smooth %>%
  group_by(doy) %>%
  summarize(smoothed_precip = mean(smoothed_precip, na.rm = TRUE)) %>%
  mutate(year = "Mean") %>%
  filter(doy != 366)

rain_highlight_smooth <- rain_smooth %>%
  filter(year %in% c(2023, 2024)) %>%
  mutate(year = as.character(year)) %>%
  select(year, doy, smoothed_precip) %>%
  bind_rows(mean_smooth)

# Now plot
ggplot() +
  # Smoothed shaded ribbon for min/max range
  geom_ribbon(
    data = hist_env_smooth,
    aes(x = doy, ymin = min_cum, ymax = max_cum),
    fill = "gray80", alpha = 0.6
  ) +
  # Smoothed lines for 2023, 2024, and mean
  geom_line(
    data = rain_highlight_smooth,
    aes(x = doy, y = smoothed_precip, color = year, linetype = year),
    size = 1.2
  ) +
  scale_color_manual(
    name = "Year",
    values = c(
      "2023" = "#E41A1C",
      "2024" = "#377EB8",
      "Mean" = "black"
    ),
    labels = c(
      "2023" = "2023",
      "2024" = "2024",
      "Mean" = "Mean (Historical)"
    )
  ) +
  scale_linetype_manual(
    name = "Year",
    values = c(
      "2023" = "solid",
      "2024" = "dashed",
      "Mean" = "dotdash"
    ),
    labels = c(
      "2023" = "2023",
      "2024" = "2024",
      "Mean" = "Mean (Historical)"
    )
  ) +
  scale_x_continuous(
    breaks = seq(1, 366, by = 30),
    labels = function(x) format(as.Date(x - 1, origin = "2000-01-01"), "%b")
  ) +
  labs(
    title = "Smoothed Cumulative Rainfall: Historical Range, Recent Years, and Mean",
    x = "Day of Year",
    y = "Cumulative Precipitation (mm)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

######### Air Temperature
temp <- read_xlsx("sorghum-rye/data/rain/historical_rain.xlsx")
