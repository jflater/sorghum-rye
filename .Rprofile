# Project-specific .Rprofile for sorghum-rye
# This file runs automatically when R starts in this project

# Load conflicted package and set preferences
if (requireNamespace("conflicted", quietly = TRUE)) {
  library(conflicted)

  # Set dplyr function preferences
  conflicts_prefer(dplyr::filter)
  conflicts_prefer(dplyr::select)
  conflicts_prefer(dplyr::lag)
  conflicts_prefer(dplyr::summarise)

  # Set other common preferences
  conflicts_prefer(lubridate::date, .quiet = TRUE)

  # Handle multcomp conflicts (if package is available)
  if (requireNamespace("multcomp", quietly = TRUE)) {
    conflicts_prefer(dplyr::select, .quiet = TRUE) # Ensure dplyr::select wins over multcomp
  }

  cat("✓ Function conflicts resolved with dplyr preferences\n")
} else {
  cat("Note: Install 'conflicted' package for better function conflict management\n")
}

# Set default options
options(
  # Increase width for console output
  width = 120,

  # Don't convert strings to factors by default
  stringsAsFactors = FALSE,

  # Set number of digits to display
  digits = 3,

  # Warn on partial matches
  warnPartialMatchDollar = TRUE,
  warnPartialMatchArgs = TRUE,
  warnPartialMatchAttr = TRUE
)

# Startup message
cat("🌾 Sorghum-Rye project loaded!\n")
