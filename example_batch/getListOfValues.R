getListOfValues <- function() {
  if (!requireNamespace("here", quietly = TRUE)) {
    install.packages("here", repos = "https://cloud.r-project.org")
  }

  source(here::here("scripts/read_ctd_mapping.R"), local = TRUE)
  mapping <- read_ctd_mapping()
  return(unique_cruise_ids(mapping))
}
