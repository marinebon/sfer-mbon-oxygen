getListOfValues <- function() {
  if (!requireNamespace("here", quietly = TRUE)) {
    install.packages("here", repos = "https://cloud.r-project.org")
  }

  source(here::here("scripts/read_ctd_mapping.R"), local = TRUE)
  mapping <- read_ctd_mapping()
  cruises <- unique_cruise_ids(mapping)

  values <- cruises[file.exists(
    here::here("data", "interpolated", cruises, "oxygen_field.csv")
  )]

  cruise_filter <- Sys.getenv("CRUISE", unset = "")
  if (nzchar(cruise_filter)) {
    values <- values[values == cruise_filter]
  }

  values
}
