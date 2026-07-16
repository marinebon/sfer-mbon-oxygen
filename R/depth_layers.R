#' Ecological depth layers for map and summary display.
#' Adjust depth_min / depth_max if cruise-specific bounds are needed.
oxygen_depth_layers <- function(max_depth = Inf) {
  layers <- list(
    list(
      id = "neuston",
      label = "Neuston surface",
      depth_min = 0,
      depth_max = 3,
      range_label = "0–3 m"
    ),
    list(
      id = "near_surface",
      label = "Near-surface layer",
      depth_min = 3,
      depth_max = 10,
      range_label = "3–10 m"
    ),
    list(
      id = "mixed",
      label = "Mixed layer",
      depth_min = 10,
      depth_max = 50,
      range_label = "10–50 m"
    ),
    list(
      id = "circalittoral",
      label = "Circalittoral",
      depth_min = 50,
      depth_max = max_depth,
      range_label = "50 m+"
    )
  )

  layers[[length(layers)]]$depth_max <- max_depth + 0.001
  layers[[length(layers)]]$range_label <- sprintf("50–%.0f m", max_depth)

  layers
}

layer_range_labels <- function(layers) {
  vapply(layers, function(layer) layer$range_label, character(1L))
}

observation_depth_column <- function(observations) {
  obs <- as.data.frame(observations)
  if ("depth" %in% names(obs)) {
    obs$depth
  } else if ("sea_water_pressure" %in% names(obs)) {
    obs$sea_water_pressure
  } else {
    stop("Observations must include 'depth' or 'sea_water_pressure'.")
  }
}

slice_observations_in_layer <- function(observations, depth_min, depth_max) {
  obs <- as.data.frame(observations)
  obs_depth <- observation_depth_column(obs)
  keep <- !is.na(obs$longitude) &
    !is.na(obs$latitude) &
    !is.na(obs_depth) &
    !is.na(obs$dissolved_oxygen) &
    obs_depth >= depth_min &
    obs_depth < depth_max

  obs[keep, , drop = FALSE]
}

slice_observations_layer <- function(observations, depth_min, depth_max) {
  obs_slice <- slice_observations_in_layer(observations, depth_min, depth_max)
  if (nrow(obs_slice) == 0) {
    return(obs_slice)
  }

  obs_depth <- observation_depth_column(obs_slice)
  obs_slice$depth_m_obs <- obs_depth

  split_key <- if ("cast_id" %in% names(obs_slice)) {
    obs_slice$cast_id
  } else {
    interaction(obs_slice$station, obs_slice$longitude, obs_slice$latitude)
  }

  do.call(
    rbind,
    lapply(split(obs_slice, split_key), function(df) {
      depth_min_obs <- min(df$depth_m_obs, na.rm = TRUE)
      depth_max_obs <- max(df$depth_m_obs, na.rm = TRUE)
      data.frame(
        cast_id = if ("cast_id" %in% names(df)) df$cast_id[[1]] else NA_character_,
        station = df$station[[1]],
        longitude = df$longitude[[1]],
        latitude = df$latitude[[1]],
        dissolved_oxygen = mean(df$dissolved_oxygen, na.rm = TRUE),
        depth_m_obs = mean(df$depth_m_obs, na.rm = TRUE),
        depth_m_min = depth_min_obs,
        depth_m_max = depth_max_obs,
        stringsAsFactors = FALSE
      )
    })
  )
}

format_obs_depth_label <- function(depth_m_min, depth_m_max) {
  if (!is.finite(depth_m_min) || !is.finite(depth_m_max)) {
    return("NA")
  }
  if (abs(depth_m_max - depth_m_min) < 0.05) {
    sprintf("%.1f m", depth_m_min)
  } else {
    sprintf("%.1f–%.1f m", depth_m_min, depth_m_max)
  }
}

field_cell_half_widths <- function(longitudes, latitudes) {
  lons <- sort(unique(longitudes))
  lats <- sort(unique(latitudes))
  list(
    dx = if (length(lons) > 1) diff(lons)[1] / 2 else 0.01,
    dy = if (length(lats) > 1) diff(lats)[1] / 2 else 0.01
  )
}

#' Median native grid spacing from interpolated fields (full cell width).
field_grid_steps <- function(longitudes, latitudes) {
  half <- field_cell_half_widths(longitudes, latitudes)
  list(
    lon_step = half$dx * 2,
    lat_step = half$dy * 2
  )
}

snap_to_field_grid <- function(longitude, latitude, lon_step, lat_step) {
  data.frame(
    longitude = round(longitude / lon_step) * lon_step,
    latitude = round(latitude / lat_step) * lat_step,
    stringsAsFactors = FALSE
  )
}

add_field_cell_bounds <- function(grid, half = NULL) {
  if (is.null(half)) {
    half <- field_cell_half_widths(grid$longitude, grid$latitude)
  }
  grid$lon1 <- grid$longitude - half$dx
  grid$lon2 <- grid$longitude + half$dx
  grid$lat1 <- grid$latitude - half$dy
  grid$lat2 <- grid$latitude + half$dy
  grid
}
