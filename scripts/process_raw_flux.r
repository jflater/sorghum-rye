library("tidyverse")

# Path to .csv files exported from LI-COR software
data_path <- "sorghum-rye/data/processed/"

# List all .csv files in the data_path directory
file_list <- list.files(path = data_path, pattern = "*.csv", full.names = TRUE)
file_list

