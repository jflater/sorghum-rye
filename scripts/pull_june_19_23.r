library(tidyverse)


data <- read_csv("sorghum-rye/data/processed_flux_data.csv",
                 show_col_types = FALSE)

data_june_19_23 <- data %>%
  filter(date == as.Date("2023-06-19"))

write_csv(data_june_19_23, "sorghum-rye/data/processed/flux_data_june_19_23.csv")
