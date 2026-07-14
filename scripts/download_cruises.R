#!/usr/bin/env Rscript

ensure_packages <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, repos = "https://cloud.r-project.org")
    }
  }
}

ensure_packages(c("here"))

library(here)

source(here("R/erddap_ctd_resolve.R"))
source(here("scripts/read_ctd_mapping.R"))

loc <- here("data", "01_raw")
log_file <- here("data", "01_raw", "download_log.csv")
dir.create(loc, recursive = TRUE, showWarnings = FALSE)

cruise_ids <- cruise_ids_for_run()
if (length(cruise_ids) == 0) {
  message("Discovering SFER CTD cruises from ERDDAP...")
  cruise_ids <- discover_sfer_cruise_ids()
}

if (length(cruise_ids) == 0) {
  stop("No SFER CTD cruises found in ERDDAP catalog.")
}

log_rows <- list()
stats <- c(downloaded = 0L, skipped = 0L, failed = 0L)

for (cruise_id in cruise_ids) {
  catalog_ids <- fetch_cruise_erddap_ids(cruise_id)
  cruise_dir <- here(loc, cruise_id)
  dir.create(cruise_dir, recursive = TRUE, showWarnings = FALSE)

  if (length(catalog_ids) == 0) {
    warning("No ERDDAP datasets found for cruise ", cruise_id)
    next
  }

  for (erddap_id in catalog_ids) {
    parsed <- parse_sfer_ctd_id(erddap_id)
    if (is.null(parsed)) {
      warning("Skipping unrecognized dataset id: ", erddap_id)
      next
    }

    filename <- paste0(erddap_id, ".csv")
    file_path <- here(cruise_dir, filename)
    file_rel <- file.path(cruise_id, filename)

    if (file.exists(file_path)) {
      cat("Skipping:", file_rel, "\n")
      stats["skipped"] <- stats["skipped"] + 1L
      log_rows[[length(log_rows) + 1L]] <- data.frame(
        cruise_id = parsed$cruise_id,
        station_id = parsed$station_id,
        erddap_dataset_id = erddap_id,
        status = "skipped",
        file = file_rel,
        stringsAsFactors = FALSE
      )
      next
    }

    cat("Downloading:", erddap_id, "->", file_rel, "\n")
    result <- tryCatch(
      {
        download_erddap_dataset_csv(erddap_id, file_path)
        "downloaded"
      },
      error = function(e) {
        cat("Failed:", erddap_id, "-", conditionMessage(e), "\n")
        "failed"
      }
    )

    stats[result] <- stats[result] + 1L
    log_rows[[length(log_rows) + 1L]] <- data.frame(
      cruise_id = parsed$cruise_id,
      station_id = parsed$station_id,
      erddap_dataset_id = erddap_id,
      status = result,
      file = file_rel,
      stringsAsFactors = FALSE
    )
  }
}

if (length(log_rows) > 0) {
  write.csv(do.call(rbind, log_rows), log_file, row.names = FALSE)
}

cat(
  "\nDownload complete:",
  stats["downloaded"], "downloaded,",
  stats["skipped"], "skipped,",
  stats["failed"], "failed\n"
)
cat("Log written to:", log_file, "\n")

if (stats["downloaded"] == 0L && stats["failed"] > 0L && stats["skipped"] == 0L) {
  quit(status = 1)
}
