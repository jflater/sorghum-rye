library(jsonlite)
library(purrr)
library(dplyr)
library(lubridate)

data_path <- "sorghum-rye/data/raw/"
file_list <- list.files(data_path, "\\.json$", full.names = TRUE)

extract_dates_coords <- function(fp) {
  js <- fromJSON(fp, simplifyVector = FALSE)
  # js$datasets is a list of length ~30; each element is a 1‑key list
  map_dfr(js$datasets, function(ds_entry) {
    # pull out the single key (e.g. "15_i" or "11_I")
    ds_name <- names(ds_entry)[1]
    ds_val  <- ds_entry[[ds_name]]
    # now grab the REP_1 header
    hdr <- ds_val$reps$REP_1$header
    # parse once
    dt <- ymd_hms(hdr$Date, tz = hdr$TimeZone)
    tibble(
      file       = basename(fp),
      dataset    = ds_name,
      datetime   = dt,
      date       = as_date(dt),
      latitude   = hdr$latitude,
      longitude  = hdr$longitude
    )
  })
}

all_records <- map_dfr(file_list, extract_dates_coords)

# you should now see ~16 + 30 = 46 rows (or whatever your JSON counts are)
print(all_records)


summary_by_file <- all_records %>%
  group_by(file, date) %>%
  summarise(
    mean_latitude  = mean(latitude,  na.rm = TRUE),
    mean_longitude = mean(longitude, na.rm = TRUE),
    .groups = "drop"
  )

print(summary_by_file)
