# This script will loop through all JSON files in the data/raw folder.
# It will then extract the dates from the JSON and write them to a CSV file.
# The CSV file will have 4 columns - file_name, date, x, and y.

library(jsonlite)
library(tidyverse)
library(purrr)

# Path to JSON files
data_path <- "sorghum-rye/data/raw/"

# List all .json files in the data_path directory
file_list <- list.files(path = data_path,
                        pattern = "*.json",
                        full.names = FALSE)
file_list

# Function to extract dates and coordinates from a JSON file

data <- fromJSON(file.path(data_path, file_list[1]))

extract_dates <- function(file) {
  json_data <- fromJSON(file)
  # Extract relevant fields
  data_frame <- tibble(
    file_name = basename(file),
    date = as.Date(json_data$measurements$date),
    x = json_data$location$x,
    y = json_data$location$y
  )
  data_frame
}

# lapply the function to all files and combine the results
data <- map_dfr(file.path(data_path, file_list), extract_dates)
