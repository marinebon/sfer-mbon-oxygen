getListOfValues <- function() {
  # Example batch placeholder; only render when matching processed data exists.
  values <- "example_1"
  processed_root <- if (requireNamespace("here", quietly = TRUE)) {
    here::here("data", "processed")
  } else {
    "data/processed"
  }

  values[file.exists(file.path(processed_root, values, "ctd_binned.csv"))]
}
