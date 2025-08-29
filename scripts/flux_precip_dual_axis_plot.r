library(readr)
library(dplyr)
library(ggplot2)
library(scales)

# Read clean flux data
clean_flux <- read_csv("data/clean_flux.csv", show_col_types = FALSE)

# Read precipitation data
precip_data <- read_csv("data/rain/daily_precip_2023_2024.csv", show_col_types = FALSE)

# Prepare flux data - aggregate by date
flux_summary <- clean_flux %>%
  group_by(year_month_day) %>%
  summarise(
    mean_flux_gnha = mean(gnha_day, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(day = year_month_day)

# Prepare precipitation data
precip_summary <- precip_data %>%
  mutate(day = as.Date(day)) %>%
  select(day, precipmm)

# Combine the datasets
combined_data <- full_join(flux_summary, precip_summary, by = "day") %>%
  filter(!is.na(day)) %>%
  arrange(day)

# Create scaling factor for dual axis (precipitation on inverted right axis)
flux_max <- max(combined_data$mean_flux_gnha, na.rm = TRUE)
precip_max <- max(combined_data$precipmm, na.rm = TRUE)
scale_factor <- flux_max / precip_max

# Create the dual-axis plot
p <- ggplot(combined_data, aes(x = day)) +
  # Flux data (left y-axis)
  geom_line(aes(y = mean_flux_gnha), color = "blue", size = 1) +
  geom_point(aes(y = mean_flux_gnha), color = "blue", size = 1.5) +
  
  # Precipitation data (right y-axis, inverted)
  geom_col(aes(y = precip_max - precipmm), 
           alpha = 0.6, fill = "lightblue", width = 1) +
  
  # Customize axes
  scale_y_continuous(
    name = "N2O Flux (g N ha⁻¹ day⁻¹)",
    sec.axis = sec_axis(
      trans = ~ precip_max - .,
      name = "Precipitation (mm)",
      breaks = seq(0, precip_max, by = 10)
    )
  ) +
  
  # Customize x-axis
  scale_x_date(
    name = "Date",
    date_breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  
  # Theme and formatting
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title.y.left = element_text(color = "blue"),
    axis.text.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "darkblue"),
    axis.text.y.right = element_text(color = "darkblue"),
    panel.grid.minor = element_blank()
  ) +
  
  labs(
    title = "N2O Flux and Precipitation Over Time",
    subtitle = "Blue line: N2O flux (left axis), Gray bars: Precipitation (right axis, inverted)"
  )

# Display the plot
print(p)

# Save the plot
ggsave("figures/flux_precip_dual_axis_plot.png", 
       plot = p, 
       width = 12, 
       height = 8, 
       dpi = 300)

print("Plot saved as figures/flux_precip_dual_axis_plot.png")