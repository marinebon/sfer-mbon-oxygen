count_pressure_rows <- function(csv_file) {
  df <- readr::read_csv(csv_file, show_col_types = FALSE)
  sum(!is.na(df$sea_water_pressure))
}

summarize_cast_processing <- function(raw_file, clean_root) {
  cast_id <- sub("\\.csv$", "", basename(raw_file))
  cruise_id <- basename(dirname(raw_file))
  clean_file <- file.path(clean_root, paste0(cast_id, ".csv"))

  raw_rows <- count_pressure_rows(raw_file)
  raw_df <- readr::read_csv(raw_file, show_col_types = FALSE)
  depth_col <- if ("depth" %in% names(raw_df)) "depth" else "sea_water_pressure"

  if (file.exists(clean_file)) {
    clean_df <- readr::read_csv(clean_file, show_col_types = FALSE)
    clean_rows <- nrow(clean_df)
    status <- "cleaned"
  } else {
    clean_df <- NULL
    clean_rows <- 0L
    status <- "missing_output"
  }

  pct_kept <- if (raw_rows > 0) round(100 * clean_rows / raw_rows, 1) else NA_real_
  pct_removed <- if (raw_rows > 0) round(100 * (raw_rows - clean_rows) / raw_rows, 1) else NA_real_

  data.frame(
    cast_id = cast_id,
    cruise_id = cruise_id,
    station = if (!is.null(clean_df) && "station" %in% names(clean_df)) {
      as.character(clean_df$station[[1]])
    } else {
      NA_character_
    },
    status = status,
    raw_rows = raw_rows,
    clean_rows = clean_rows,
    rows_removed = raw_rows - clean_rows,
    pct_kept = pct_kept,
    pct_removed = pct_removed,
    raw_max_depth_m = suppressWarnings(max(raw_df[[depth_col]], na.rm = TRUE)),
    clean_max_depth_m = if (!is.null(clean_df) && depth_col %in% names(clean_df)) {
      suppressWarnings(max(clean_df[[depth_col]], na.rm = TRUE))
    } else {
      NA_real_
    },
    stringsAsFactors = FALSE
  )
}

summarize_all_processing <- function(
    raw_root = here::here("data", "01_raw"),
    clean_root = here::here("data", "02_clean"),
    log_file = here::here("data", "02_clean", "processing_log.csv")
) {
  source(here::here("scripts/read_ctd_mapping.R"), local = TRUE)

  mapping <- read_ctd_mapping()
  cast_summaries <- list()

  for (cruise_id in unique_cruise_ids(mapping)) {
    cruise_raw_dir <- file.path(raw_root, cruise_id)
    if (!dir.exists(cruise_raw_dir)) {
      next
    }

    raw_files <- list.files(cruise_raw_dir, pattern = "\\.csv$", full.names = TRUE)
    for (raw_file in raw_files) {
      cast_summaries[[length(cast_summaries) + 1L]] <- summarize_cast_processing(
        raw_file,
        clean_root
      )
    }
  }

  if (length(cast_summaries) == 0) {
    stop("No raw CTD files found under ", raw_root, ". Run `make download` first.")
  }

  summary <- do.call(rbind, cast_summaries)

  if (file.exists(log_file)) {
    log <- read.csv(log_file, stringsAsFactors = FALSE)
    summary <- summary %>%
      dplyr::select(-status) %>%
      dplyr::left_join(
        log[, c("cast_id", "status")],
        by = "cast_id"
      )
  }

  summary
}

summarize_processing_by_cruise <- function(cast_summary) {
  cast_summary %>%
    dplyr::group_by(cruise_id) %>%
    dplyr::summarise(
      casts = dplyr::n(),
      cleaned = sum(status == "cleaned"),
      skipped = sum(status == "skipped"),
      empty = sum(status == "empty"),
      failed = sum(status == "failed"),
      raw_rows = sum(raw_rows, na.rm = TRUE),
      clean_rows = sum(clean_rows, na.rm = TRUE),
      pct_kept = ifelse(raw_rows > 0, round(100 * clean_rows / raw_rows, 1), NA_real_),
      .groups = "drop"
    )
}
