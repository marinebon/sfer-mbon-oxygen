clean_ctd_cast <- function(
    raw_file,
    output_file = NULL,
    cruise_id = NULL,
    decimate_p = 0.1,
    overwrite = FALSE
) {
  cast_id <- sub("\\.csv$", "", basename(raw_file))

  if (is.null(cruise_id)) {
    cruise_id <- basename(dirname(raw_file))
  }

  if (is.null(output_file)) {
    output_file <- here::here("data", "02_clean", paste0(cast_id, ".csv"))
  }

  if (file.exists(output_file) && !overwrite) {
    raw_rows <- count_pressure_rows(raw_file)
    clean_rows <- nrow(read.csv(output_file))
    return(list(
      status = "skipped",
      cast_id = cast_id,
      cruise_id = cruise_id,
      output_file = output_file,
      raw_rows = raw_rows,
      clean_rows = clean_rows
    ))
  }

  metadata <- get_metadata_from_cast_id(cast_id, cruise_id = cruise_id)
  cast <- ctd_load_from_csv(raw_file, cast_id = cast_id, cruise_id = cruise_id)

  trimmed_cast <- oce::ctdTrim(cast)
  decimated_cast <- oce::ctdDecimate(trimmed_cast, p = decimate_p)
  cleaned_scans <- decimated_cast@data$scan

  ctd_raw <- readr::read_csv(raw_file, show_col_types = FALSE)
  raw_rows <- sum(!is.na(ctd_raw$sea_water_pressure))
  cast_df <- ctd_raw %>%
    dplyr::filter(!is.na(sea_water_pressure)) %>%
    dplyr::mutate(row_num = dplyr::row_number()) %>%
    dplyr::filter(row_num %in% cleaned_scans) %>%
    dplyr::select(-row_num) %>%
    dplyr::mutate(
      station = metadata$station_id,
      cruise_id = metadata$cruise_id
    )

  if (nrow(cast_df) == 0) {
    return(list(
      status = "empty",
      cast_id = cast_id,
      cruise_id = cruise_id,
      output_file = output_file,
      raw_rows = raw_rows,
      clean_rows = 0L
    ))
  }

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  write.csv(cast_df, output_file, row.names = FALSE)

  list(
    status = "cleaned",
    cast_id = cast_id,
    cruise_id = cruise_id,
    rows = nrow(cast_df),
    output_file = output_file,
    raw_rows = raw_rows,
    clean_rows = nrow(cast_df)
  )
}
