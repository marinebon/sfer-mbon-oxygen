BLUETOPO_LAND_ELEVATION_M <- -0.2

list_bluetopo_rasters <- function(
    bath_root = here::here("data", "bathymetry")) {
  if (!dir.exists(bath_root)) {
    return(character())
  }

  tifs <- list.files(
    bath_root,
    pattern = "_bluetopo_.*\\.tif$",
    full.names = TRUE
  )
  tifs[!grepl("\\.aux\\.xml$", tifs, fixed = FALSE)]
}

bluetopo_rasters_overlapping <- function(
    lon_min,
    lon_max,
    lat_min,
    lat_max,
    bath_root = here::here("data", "bathymetry")) {
  tifs <- list_bluetopo_rasters(bath_root)
  if (length(tifs) == 0) {
    return(character())
  }

  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' is required to load bathymetry.")
  }

  vapply(tifs, function(path) {
    raster <- terra::rast(path)
    extent <- terra::ext(raster)
    as.numeric(
      extent[1] <= lon_max &&
        extent[2] >= lon_min &&
        extent[3] <= lat_max &&
        extent[4] >= lat_min
    )
  }, numeric(1)) > 0 -> overlap
  tifs[overlap]
}

extract_raster_values <- function(raster, coords) {
  extracted <- terra::extract(raster, coords)
  as.numeric(extracted[[ncol(extracted)]])
}

#' Sample seafloor depth (m, positive down) on a lon/lat grid from BlueTopo rasters.
sample_volume_bathymetry <- function(
    lons,
    lats,
    bath_root = here::here("data", "bathymetry")) {
  lons <- sort(unique(as.numeric(lons)))
  lats <- sort(unique(as.numeric(lats)))
  if (length(lons) == 0 || length(lats) == 0) {
    return(NULL)
  }

  if (!requireNamespace("terra", quietly = TRUE)) {
    warning("Package 'terra' is not installed; skipping bathymetry.")
    return(NULL)
  }

  lon_min <- min(lons)
  lon_max <- max(lons)
  lat_min <- min(lats)
  lat_max <- max(lats)
  tifs <- bluetopo_rasters_overlapping(
    lon_min,
    lon_max,
    lat_min,
    lat_max,
    bath_root = bath_root
  )
  if (length(tifs) == 0) {
    return(NULL)
  }

  grid <- expand.grid(
    longitude = lons,
    latitude = lats,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  coords <- as.matrix(grid[, c("longitude", "latitude"), drop = FALSE])
  elevation <- rep(NA_real_, nrow(grid))

  for (path in tifs) {
    raster <- terra::rast(path)[[1]]
    values <- extract_raster_values(raster, coords)
    replace <- is.finite(values) &
      values < BLUETOPO_LAND_ELEVATION_M &
      (is.na(elevation) | values < elevation)
    elevation[replace] <- values[replace]
  }

  depth <- ifelse(
    is.finite(elevation) & elevation < BLUETOPO_LAND_ELEVATION_M,
    -elevation,
    NA_real_
  )
  depth_mat <- matrix(depth, nrow = length(lats), ncol = length(lons), byrow = TRUE)

  list(
    lons = lons,
    lats = lats,
    depth = depth_mat
  )
}

bathymetry_surface_colorscale <- function() {
  list(
    list(0, "#8ECAE6"),
    list(0.35, "#219EBC"),
    list(0.7, "#126782"),
    list(1, "#023047")
  )
}

default_coastline_geojson <- function(
    bath_root = here::here("data", "bathymetry")) {
  file.path(bath_root, "bluetopo_cache", "ne_10m_land.geojson")
}

#' Line segments for land/water boundaries clipped to a lon/lat bbox.
coastline_segments_for_bbox <- function(
    lon_min,
    lon_max,
    lat_min,
    lat_max,
    geojson_path = default_coastline_geojson()) {
  if (!file.exists(geojson_path)) {
    return(list())
  }
  if (!requireNamespace("sf", quietly = TRUE)) {
    warning("Package 'sf' is not installed; skipping coastline overlay.")
    return(list())
  }

  land <- sf::st_read(geojson_path, quiet = TRUE)
  bbox <- sf::st_bbox(
    c(xmin = lon_min, ymin = lat_min, xmax = lon_max, ymax = lat_max),
    crs = sf::st_crs(land)
  )
  clip <- sf::st_crop(land, bbox)
  if (nrow(clip) == 0) {
    return(list())
  }

  lines <- sf::st_cast(sf::st_cast(clip, "MULTILINESTRING"), "LINESTRING")
  segments <- lapply(seq_len(nrow(lines)), function(i) {
    coords <- sf::st_coordinates(lines[i, ])
    if (nrow(coords) < 2) {
      return(NULL)
    }
    list(
      lon = coords[, "X"],
      lat = coords[, "Y"]
    )
  })

  Filter(Negate(is.null), segments)
}

coastline_path_for_plotly <- function(segments, z = 0) {
  if (length(segments) == 0) {
    return(NULL)
  }

  n <- sum(vapply(segments, function(seg) length(seg$lon) + 1L, integer(1)))
  lon <- lat <- z_vals <- rep(NA_real_, n)
  idx <- 1L
  for (seg in segments) {
    len <- length(seg$lon)
    end <- idx + len - 1L
    lon[idx:end] <- seg$lon
    lat[idx:end] <- seg$lat
    z_vals[idx:end] <- z
    idx <- end + 2L
  }

  list(lon = lon, lat = lat, z = z_vals)
}
