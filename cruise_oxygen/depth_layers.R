# Ecological depth layers for map display.
# Adjust depth_min / depth_max if cruise-specific bounds are needed.

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

aggregate_field_layer <- function(field, depth_min, depth_max) {
  in_layer <- field$depth_m >= depth_min &
    field$depth_m < depth_max &
    !is.na(field$dissolved_oxygen)

  slice <- field[in_layer, , drop = FALSE]
  if (nrow(slice) == 0) {
    return(NULL)
  }

  stats::aggregate(
    dissolved_oxygen ~ longitude + latitude,
    data = slice,
    FUN = mean,
    na.rm = TRUE
  )
}

slice_observations_layer <- function(observations, depth_min, depth_max) {
  obs <- as.data.frame(observations)
  obs_depth <- if ("depth" %in% names(obs)) obs$depth else obs$sea_water_pressure
  keep <- !is.na(obs$longitude) &
    !is.na(obs$latitude) &
    !is.na(obs_depth) &
    !is.na(obs$dissolved_oxygen) &
    obs_depth >= depth_min &
    obs_depth < depth_max

  obs_slice <- obs[keep, , drop = FALSE]
  obs_slice$depth_m_obs <- obs_depth[keep]
  obs_slice
}
