#!/usr/bin/env Rscript

ensure_packages <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, repos = "https://cloud.r-project.org")
    }
  }
}

ensure_packages(c("here", "readr", "dplyr", "oce"))

library(here)
library(readr)
library(dplyr)
library(oce)

source(here("R/get_metadata_from_cast_id.R"))
source(here("R/ctd_load_from_csv.R"))
source(here("R/clean_ctd_cast.R"))
source(here("R/summarize_processing.R"))
source(here("scripts/read_ctd_mapping.R"))

raw_root <- here("data", "01_raw")
clean_root <- here("data", "02_clean")
log_file <- here("data", "02_clean", "processing_log.csv")
dir.create(clean_root, recursive = TRUE, showWarnings = FALSE)

mapping <- read_ctd_mapping()
stats <- c(cleaned = 0L, skipped = 0L, empty = 0L, failed = 0L)
log_rows <- list()

for (cruise_id in cruise_ids_for_run(mapping)) {
  cruise_raw_dir <- file.path(raw_root, cruise_id)
  if (!dir.exists(cruise_raw_dir)) {
    warning("No raw data directory for ", cruise_id, ". Run `make download` first.")
    next
  }

  raw_files <- list.files(cruise_raw_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(raw_files) == 0) {
    warning("No raw CTD files found for ", cruise_id)
    next
  }

  message("Cleaning ", length(raw_files), " casts for ", cruise_id)

  for (raw_file in raw_files) {
    cast_id <- sub("\\.csv$", "", basename(raw_file))
    output_file <- file.path(clean_root, paste0(cast_id, ".csv"))

    result <- tryCatch(
      clean_ctd_cast(
        raw_file = raw_file,
        output_file = output_file,
        cruise_id = cruise_id
      ),
      error = function(e) {
        message("Failed: ", cast_id, " - ", conditionMessage(e))
        list(
          status = "failed",
          cast_id = cast_id,
          cruise_id = cruise_id,
          raw_rows = count_pressure_rows(raw_file),
          clean_rows = 0L,
          error = conditionMessage(e)
        )
      }
    )

    stats[result$status] <- stats[result$status] + 1L
    if (result$status == "cleaned") {
      message("Wrote ", result$output_file, " (", result$rows, " rows)")
    } else if (result$status == "skipped") {
      message("Skipping: ", cast_id, " (already processed)")
    }

    log_rows[[length(log_rows) + 1L]] <- data.frame(
      cast_id = result$cast_id,
      cruise_id = result$cruise_id,
      status = result$status,
      raw_rows = if (!is.null(result$raw_rows)) result$raw_rows else NA_integer_,
      clean_rows = if (!is.null(result$clean_rows)) result$clean_rows else NA_integer_,
      error = if (!is.null(result$error)) result$error else NA_character_,
      stringsAsFactors = FALSE
    )
  }
}

if (length(log_rows) > 0) {
  write.csv(do.call(rbind, log_rows), log_file, row.names = FALSE)
}

cat(
  "\nCleaning complete:",
  stats["cleaned"], "cleaned,",
  stats["skipped"], "skipped,",
  stats["empty"], "empty,",
  stats["failed"], "failed\n"
)
cat("Log written to:", log_file, "\n")

# Fail only when casts failed and none were cleaned or already up to date.
if (stats["failed"] > 0L && stats["cleaned"] == 0L && stats["skipped"] == 0L) {
  quit(status = 1)
}
