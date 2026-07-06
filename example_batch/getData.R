getData <- function(batch_value) {
  if (!requireNamespace("here", quietly = TRUE)) {
    install.packages("here", repos = "https://cloud.r-project.org")
  }

  processed_file <- here::here(
    "data/processed", batch_value, "ctd_binned.csv"
  )
  interp_file <- here::here(
    "data/interpolated", batch_value, "oxygen_field.csv"
  )

  if (!file.exists(processed_file)) {
    stop(
      "Processed CTD data not found for ", batch_value,
      ". Run `make process` (or `make publish`) first."
    )
  }

  list(
    ctd_binned = read.csv(processed_file, stringsAsFactors = FALSE),
    oxygen_field = if (file.exists(interp_file)) {
      read.csv(interp_file, stringsAsFactors = FALSE)
    } else {
      NULL
    }
  )
}
