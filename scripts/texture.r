library(readxl)
library(tidyverse)
texture <- read_excel("data/PSA Report - AGRON-McDaniel-Potter-2024.xlsm",
    sheet = "Main Data File format", n_max = 22
)

texture <- texture %>%
    select(Sample, Sand = S, Silt = T, Clay = C) %>%
    separate(Sample, into = c("ID", "Plot"), sep = "_") %>%
    mutate(Plot = as.numeric(Plot)) %>%
    drop_na() %>%
    mutate(
        Sand = as.numeric(Sand),
        Silt = as.numeric(Silt),
        Clay = as.numeric(Clay)
    )

# get average texture for each Sand, Silt, Clay
texture_avg <- texture %>%
    group_by(ID) %>%
    summarise(
        Sand = mean(Sand, na.rm = TRUE),
        Silt = mean(Silt, na.rm = TRUE),
        Clay = mean(Clay, na.rm = TRUE),
        .groups = "drop"
    )
texture_avg
