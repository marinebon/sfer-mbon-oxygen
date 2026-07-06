get_metadata_from_cast_id <- function(cast_id, cruise_id = NULL) {
  if (grepl("^SFER_CTD_", cast_id)) {
    if (is.null(cruise_id)) {
      cruise_id <- sub("^SFER_CTD_([^_]+)_.*", "\\1", cast_id)
    }
    station_id <- sub(paste0("^SFER_CTD_", cruise_id, "_"), "", cast_id)
    return(list(
      cast_id = cast_id,
      cruise_id = cruise_id,
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
