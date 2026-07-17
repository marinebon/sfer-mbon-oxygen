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
