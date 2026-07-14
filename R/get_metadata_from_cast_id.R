get_metadata_from_cast_id <- function(cast_id, cruise_id = NULL, raw_file = NULL) {
  if (grepl("^SFER_CTD_", cast_id)) {
    if (!exists("parse_sfer_ctd_id", mode = "function")) {
      if (requireNamespace("here", quietly = TRUE)) {
        source(here::here("R/erddap_ctd_resolve.R"), local = TRUE)
      } else {
        source("R/erddap_ctd_resolve.R", local = TRUE)
      }
    }

    parsed <- parse_sfer_ctd_id(cast_id)
    if (is.null(parsed)) {
      stop("Could not parse SFER CTD cast id: ", cast_id)
    }

    station_id <- parsed$station_id
    if (!is.null(raw_file) && file.exists(raw_file)) {
      station_from_file <- read_station_from_erddap_csv(raw_file)
      if (!is.na(station_from_file) && nzchar(station_from_file)) {
        station_id <- station_from_file
      }
    }

    return(list(
      cast_id = cast_id,
      cruise_id = if (is.null(cruise_id)) parsed$cruise_id else cruise_id,
      station_id = station_id
    ))
  }

  cruise_id <- sub("_.*", "", cast_id)
  end_of_cast_id <- sub("^[^_]+_[^_]+_", "", cast_id)
  station_id_as_entered <- sub("^[A-Za-z]{1,3}[0-9]{4,5}_?", "", end_of_cast_id)
  station_id <- sub("(?i)(stn|sta)_?", "", station_id_as_entered, perl = TRUE)

  list(
    cast_id = cast_id,
    cruise_id = cruise_id,
    station_id = station_id
  )
}
