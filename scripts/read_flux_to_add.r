library(readr)
library(dplyr)
library(purrr)

flux_folder <- "../data/flux_to_add_to_analysis"

file_list <- list.files(flux_folder, pattern = "*.csv", full.names = TRUE)

read_flux_file <- function(file_path) {
  data <- read_csv(file_path, skip = 3, show_col_types = FALSE)
  data$source_file <- basename(file_path)
  return(data)
}

flux_data <- map_dfr(file_list, read_flux_file)

print(paste("Read", nrow(flux_data), "rows from", length(file_list), "files"))
print("Files processed:")
print(basename(file_list))
