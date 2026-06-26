#!/usr/bin/env Rscript

ensure_packages <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, repos = "https://cloud.r-project.org")
    }
  }
}

ensure_packages(c("here", "readr", "rerddap"))

library(here)
library(readr)
library(rerddap)

source(here("scripts/read_ctd_mapping.R"))
source(here("R/align_raw_ctd_filename.R"))
source(here("R/erddap_ctd_resolve.R"))

database <- ERDDAP_BASE
Sys.setenv(RERDDAP_DEFAULT_URL = database)

mapping <- read_ctd_mapping()
loc <- here("data", "01_raw")
log_file <- here("data", "01_raw", "download_log.csv")
dir.create(loc, recursive = TRUE, showWarnings = FALSE)

download_one <- function(erddap_id, file_path) {
  out <- info(erddap_id)
  ctd_data <- tabledap(out, url = eurl())
  write.csv(ctd_data, file_path, row.names = FALSE)
}

log_rows <- list()
stats <- c(downloaded = 0L, skipped = 0L, missing = 0L, failed = 0L)

for (cruise_id in unique_cruise_ids(mapping)) {
  stations <- stations_for_cruise(mapping, cruise_id)
  catalog_ids <- fetch_cruise_erddap_ids(cruise_id)
  cruise_dir <- here(loc, cruise_id)
  dir.create(cruise_dir, recursive = TRUE, showWarnings = FALSE)

  if (length(catalog_ids) == 0) {
    warning("No ERDDAP datasets found for cruise ", cruise_id)
  }

  for (i in seq_len(nrow(stations))) {
    row <- stations[i, ]
    aligned_filename <- align_raw_ctd_filename(paste0(row$dataset_id, ".csv"))
    file_path <- here(cruise_dir, aligned_filename)
    file_rel <- file.path(cruise_id, aligned_filename)
    erddap_id <- resolve_erddap_dataset_id(
      row$cruise_id,
      row$station,
      catalog_ids = catalog_ids
    )

    if (file.exists(file_path)) {
      cat("Skipping:", file_rel, "\n")
      stats["skipped"] <- stats["skipped"] + 1L
      log_rows[[length(log_rows) + 1L]] <- data.frame(
        cruise_id = row$cruise_id,
        station = row$station,
        mapping_dataset_id = row$dataset_id,
        erddap_dataset_id = erddap_id,
        status = "skipped",
        file = file_rel,
        stringsAsFactors = FALSE
      )
      next
    }

    if (!erddap_id %in% catalog_ids && !erddap_dataset_exists(erddap_id)) {
      cat("Missing:", row$cruise_id, "station", row$station,
          "(expected ", erddap_id, ")\n", sep = "")
      stats["missing"] <- stats["missing"] + 1L
      log_rows[[length(log_rows) + 1L]] <- data.frame(
        cruise_id = row$cruise_id,
        station = row$station,
        mapping_dataset_id = row$dataset_id,
        erddap_dataset_id = erddap_id,
        status = "missing",
        file = file_rel,
        stringsAsFactors = FALSE
      )
      next
    }

    cat("Downloading:", erddap_id, "->", file_rel, "\n")
    result <- tryCatch(
      {
        download_one(erddap_id, file_path)
        "downloaded"
      },
      error = function(e) {
        cat("Failed:", erddap_id, "-", conditionMessage(e), "\n")
        "failed"
      }
    )

    stats[result] <- stats[result] + 1L
    log_rows[[length(log_rows) + 1L]] <- data.frame(
      cruise_id = row$cruise_id,
      station = row$station,
      mapping_dataset_id = row$dataset_id,
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
  stats["missing"], "missing,",
  stats["failed"], "failed\n"
)
cat("Log written to:", log_file, "\n")

if (stats["downloaded"] == 0L && stats["failed"] > 0L) {
  quit(status = 1)
}
