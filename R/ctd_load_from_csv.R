ctd_load_from_csv <- function(file, cast_id = NULL, cruise_id = NULL) {
  if (is.null(cast_id)) {
    cast_id <- sub("\\.csv$", "", basename(file))
  }

  ctd_raw <- readr::read_csv(file, show_col_types = FALSE)

  metadata <- get_metadata_from_cast_id(cast_id, cruise_id = cruise_id)
  station_id <- metadata$station_id
  cruise_id <- metadata$cruise_id

  lat <- ctd_raw$latitude[[1]]
  lon <- ctd_raw$longitude[[1]]

  ctd_temp <- with(
    ctd_raw,
    oce::as.ctd(
      salinity = sea_water_salinity,
      temperature = sea_water_temperature,
      pressure = sea_water_pressure,
      station = station_id,
      cruise = cruise_id,
      longitude = lon,
      latitude = lat,
      time = time_elapsed
    )
  )

  core_columns <- c(
    "sea_water_salinity", "sea_water_temperature", "sea_water_pressure",
    "latitude", "longitude", "time_elapsed"
  )
  additional_columns <- setdiff(names(ctd_raw), core_columns)

  for (param_name in additional_columns) {
    ctd_temp <- oce::oceSetData(
      object = ctd_temp,
      name = param_name,
      value = ctd_raw[[param_name]],
      originalName = param_name
    )
  }

  ctd_temp
}
