library(readxl)
library(tidyverse)
library(janitor)
# Read in the two soil data files
# pH and GWC might be hidden in other sheets
soil_2023_gwc <- read_xlsx("data/soils/2023_SABR_MASTER_soil.xlsx", sheet = 2)
soil_2023_ph <- read_xlsx("data/soils/2023_SABR_MASTER_soil.xlsx", sheet = 3)
soil_2024 <- read_xlsx("data/soils/2024_SABR_MASTER_soil.xlsx")

soil_2023_gwc <- soil_2023_gwc %>%
  filter(
    str_detect(`Project/sampling`, str_c("12-2023|Spring 2023")) &
      !str_detect(`Plot ID`, str_c("NE|SE|SW|NW"))
  )
