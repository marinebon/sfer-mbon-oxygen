source(here::here("R/depth_layers.R"))

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
