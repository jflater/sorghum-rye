# Convert nmols n2o per meter squared per second to grams per hectare per day of nitorgen
# LiCor survey chambers read in nmols n2o per meter squared per second


nmols_to_grams_hectare_day <- function(nmols) {
  print("Converting nmols n2o per meter squared per second to grams per hectare per day of nitorgen")
  mols <- nmols / 1e9
  grams_n <- mols * 28.0134
  grams_per_m2_per_day <- grams_n * 86400
  grams_per_hectare_per_day <- grams_per_m2_per_day * 10000
  return(grams_per_hectare_per_day)
}
