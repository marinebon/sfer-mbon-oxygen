# Ecological depth layers for map display.
# Adjust depth_min / depth_max if cruise-specific bounds are needed.

field_slice_grid <- function(field_slice) {
  lons <- sort(unique(field_slice$longitude))
  lats <- sort(unique(field_slice$latitude))
  n_lon <- length(lons)
  n_lat <- length(lats)

  dx <- if (n_lon > 1) diff(lons)[1] / 2 else 0.01
  dy <- if (n_lat > 1) diff(lats)[1] / 2 else 0.01

  field_slice$lon1 <- field_slice$longitude - dx
  field_slice$lon2 <- field_slice$longitude + dx
  field_slice$lat1 <- field_slice$latitude - dy
  field_slice$lat2 <- field_slice$latitude + dy
  field_slice
}

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
  if (nrow(obs_slice) == 0) {
    return(obs_slice)
  }

  obs_slice$depth_m_obs <- obs_depth[keep]

  # One marker per cast: multiple depth samples share the same lat/lon and
  # otherwise stack invisibly, leaving only the top circle interactive.
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

build_oxygen_map_payload <- function(field, observations, layers) {
  depth_layers <- list()

  for (layer in layers) {
    field_slice <- aggregate_field_layer(
      field,
      layer$depth_min,
      layer$depth_max
    )
    if (is.null(field_slice) || nrow(field_slice) == 0) {
      next
    }

    grid_slice <- field_slice_grid(field_slice)
    field_cells <- lapply(seq_len(nrow(grid_slice)), function(i) {
      row <- grid_slice[i, ]
      list(
        lon1 = row$lon1,
        lat1 = row$lat1,
        lon2 = row$lon2,
        lat2 = row$lat2,
        o2 = row$dissolved_oxygen,
        popup = sprintf(
          "O\u2082: %.2f mg/L<br>Lat: %.5f<br>Lon: %.5f",
          row$dissolved_oxygen,
          row$latitude,
          row$longitude
        )
      )
    })

    obs_slice <- slice_observations_layer(
      observations,
      layer$depth_min,
      layer$depth_max
    )
    obs_cells <- list()
    if (nrow(obs_slice) > 0) {
      obs_slice$depth_label <- mapply(
        format_obs_depth_label,
        obs_slice$depth_m_min,
        obs_slice$depth_m_max
      )
      obs_cells <- lapply(seq_len(nrow(obs_slice)), function(i) {
        row <- obs_slice[i, ]
        list(
          lon = row$longitude,
          lat = row$latitude,
          o2 = row$dissolved_oxygen,
          station = as.character(row$station),
          depth_label = row$depth_label,
          label = sprintf(
            "Station: %s\nO\u2082: %.2f mg/L (layer mean)\nDepth: %s",
            row$station,
            row$dissolved_oxygen,
            row$depth_label
          ),
          popup = sprintf(
            "Station: %s<br>O\u2082: %.2f mg/L (layer mean)<br>Depth: %s<br>Lat: %.5f<br>Lon: %.5f",
            row$station,
            row$dissolved_oxygen,
            row$depth_label,
            row$latitude,
            row$longitude
          )
        )
      })
    }

    depth_layers[[length(depth_layers) + 1L]] <- list(
      label = layer$range_label,
      field = field_cells,
      obs = obs_cells
    )
  }

  depth_layers
}
