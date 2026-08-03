getData <- function(batch_value) {
  if (!requireNamespace("here", quietly = TRUE)) {
    install.packages("here", repos = "https://cloud.r-project.org")
  }

  clean_root <- here::here("data", "02_clean")
  prefix <- paste0("SFER_CTD_", batch_value, "_")
  clean_files <- list.files(
    clean_root,
    pattern = paste0("^", prefix, ".*\\.csv$"),
    full.names = TRUE
  )
  clean_files <- clean_files[basename(clean_files) != "processing_log.csv"]

  if (length(clean_files) == 0) {
    stop(
      "Cleaned CTD data not found for ", batch_value,
      ". Run `make process` first."
    )
  }

  source(here::here("R/load_clean_ctd_observations.R"), local = TRUE)
  observations <- load_clean_ctd_observations(clean_files)

  interp_file <- here::here(
    "data", "interpolated", batch_value, "oxygen_field.csv"
  )

  list(
    observations = observations,
    oxygen_field = if (file.exists(interp_file)) {
      read.csv(interp_file, stringsAsFactors = FALSE)
    } else {
      NULL
    }
  )
}
