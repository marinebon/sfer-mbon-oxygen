#' Shallowest depth where dissolved oxygen falls below a threshold.
#'
#' For each horizontal grid cell, scans the interpolated profile from the
#' surface downward and returns the minimum depth at which O₂ < threshold.
anoxic_depth_grid <- function(field, threshold) {
  if (is.null(field) || nrow(field) == 0) {
    return(NULL)
  }

  depth_col <- field$depth_m
  o2_col <- field$dissolved_oxygen

  cells <- unique(field[, c("longitude", "latitude")])
  rows <- lapply(seq_len(nrow(cells)), function(i) {
    lon <- cells$longitude[i]
    lat <- cells$latitude[i]
    mask <- field$longitude == lon & field$latitude == lat
    profile <- field[mask, c("depth_m", "dissolved_oxygen"), drop = FALSE]
    profile <- profile[order(profile$depth_m), , drop = FALSE]

    below <- profile$dissolved_oxygen < threshold & !is.na(profile$dissolved_oxygen)
    depth <- if (!any(below)) {
      NA_real_
    } else {
      min(profile$depth_m[below])
    }

    data.frame(
      longitude = lon,
      latitude = lat,
      anoxic_depth_m = depth,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

build_profile_payload <- function(field) {
  if (is.null(field) || nrow(field) == 0) {
    return(list(cells = list()))
  }

  split_keys <- interaction(field$longitude, field$latitude, drop = TRUE)
  groups <- split(field, split_keys)

  cells <- unname(lapply(groups, function(profile) {
    profile <- profile[order(profile$depth_m), , drop = FALSE]
    list(
      lon = jsonlite::unbox(as.numeric(profile$longitude[1])),
      lat = jsonlite::unbox(as.numeric(profile$latitude[1])),
      depths = as.numeric(profile$depth_m),
      oxygens = as.numeric(profile$dissolved_oxygen)
    )
  }))

  list(cells = cells)
}

anoxic_depth_grid_rectangles <- function(grid) {
  if (is.null(grid) || nrow(grid) == 0) {
    return(NULL)
  }

  lons <- sort(unique(grid$longitude))
  lats <- sort(unique(grid$latitude))
  dx <- if (length(lons) > 1) diff(lons)[1] / 2 else 0.01
  dy <- if (length(lats) > 1) diff(lats)[1] / 2 else 0.01

  grid$lon1 <- grid$longitude - dx
  grid$lon2 <- grid$longitude + dx
  grid$lat1 <- grid$latitude - dy
  grid$lat2 <- grid$latitude + dy
  grid
}

DEFAULT_ANOXIC_O2_THRESHOLD <- 3.8

default_anoxic_threshold <- function(field, observations) {
  DEFAULT_ANOXIC_O2_THRESHOLD
}
