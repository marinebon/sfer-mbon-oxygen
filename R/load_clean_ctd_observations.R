load_clean_ctd_observations <- function(clean_files) {
  dfs <- lapply(clean_files, function(path) {
    df <- read.csv(path, stringsAsFactors = FALSE)
    depth_col <- if ("depth" %in% names(df)) {
      "depth"
    } else if ("sea_water_pressure" %in% names(df)) {
      "sea_water_pressure"
    } else {
      stop("No depth column in ", path)
    }

    cast_id <- sub("\\.csv$", "", basename(path))
    station <- if ("station" %in% names(df)) {
      as.character(df$station[[1]])
    } else {
      get_metadata_from_cast_id(cast_id)$station_id
    }

    data.frame(
      cast_id = cast_id,
      station = station,
      longitude = as.numeric(df$longitude),
      latitude = as.numeric(df$latitude),
      depth = as.numeric(df[[depth_col]]),
      dissolved_oxygen = as.numeric(df$dissolved_oxygen),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, dfs)
}
