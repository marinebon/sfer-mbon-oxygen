DEFAULT_HYPOXIC_O2_THRESHOLD <- 2.0

source(here::here("R/depth_layers.R"))

list_interpolated_cruises <- function(
    interp_root = here::here("data", "interpolated")) {
  if (!dir.exists(interp_root)) {
    return(character())
  }

  cruises <- list.dirs(interp_root, full.names = FALSE, recursive = FALSE)
  cruises <- cruises[nzchar(cruises)]
  cruises[file.exists(file.path(interp_root, cruises, "oxygen_field.csv"))]
}

load_cruise_field <- function(
    cruise_id,
    interp_root = here::here("data", "interpolated")) {
  path <- file.path(interp_root, cruise_id, "oxygen_field.csv")
  if (!file.exists(path)) {
    return(NULL)
  }
  read.csv(path, stringsAsFactors = FALSE)
}

median_field_grid_steps <- function(
    cruises,
    interp_root = here::here("data", "interpolated")) {
  steps <- lapply(cruises, function(cruise_id) {
    field <- load_cruise_field(cruise_id, interp_root)
    if (is.null(field) || nrow(field) == 0) {
      return(NULL)
    }
    field_grid_steps(field$longitude, field$latitude)
  })
  steps <- Filter(Negate(is.null), steps)
  if (length(steps) == 0) {
    return(list(lon_step = 0.04, lat_step = 0.03))
  }

  lon_steps <- vapply(steps, function(x) x$lon_step, numeric(1))
  lat_steps <- vapply(steps, function(x) x$lat_step, numeric(1))
  list(
    lon_step = stats::median(lon_steps, na.rm = TRUE),
    lat_step = stats::median(lat_steps, na.rm = TRUE)
  )
}

field_hypoxic_cells_by_layer <- function(field, threshold, layers, grid_steps) {
  if (is.null(field) || nrow(field) == 0) {
    return(NULL)
  }

  rows <- lapply(layers, function(layer) {
    slice <- field[
      field$depth_m >= layer$depth_min &
        field$depth_m < layer$depth_max &
        !is.na(field$dissolved_oxygen),
      ,
      drop = FALSE
    ]
    if (nrow(slice) == 0) {
      return(NULL)
    }

    slice |>
      dplyr::mutate(
        longitude = round(.data$longitude / grid_steps$lon_step) * grid_steps$lon_step,
        latitude = round(.data$latitude / grid_steps$lat_step) * grid_steps$lat_step
      ) |>
      dplyr::group_by(.data$longitude, .data$latitude) |>
      dplyr::summarize(
        hypoxic = any(.data$dissolved_oxygen < threshold),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        layer_id = layer$id,
        layer_label = layer$range_label
      )
  })

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(NULL)
  }

  do.call(rbind, rows)
}

#' Share of interpolated cruises with hypoxia at each grid cell, by depth layer.
hypoxic_field_frequency_all_cruises <- function(
    threshold = DEFAULT_HYPOXIC_O2_THRESHOLD,
    interp_root = here::here("data", "interpolated"),
    layers = NULL) {
  cruises <- list_interpolated_cruises(interp_root)
  if (length(cruises) == 0) {
    return(data.frame())
  }

  if (is.null(layers)) {
    max_depth <- 0
    for (cruise_id in cruises) {
      field <- load_cruise_field(cruise_id, interp_root)
      if (!is.null(field) && nrow(field) > 0) {
        max_depth <- max(max_depth, max(field$depth_m, na.rm = TRUE))
      }
    }
    layers <- oxygen_depth_layers(max_depth = max_depth)
  }

  grid_steps <- median_field_grid_steps(cruises, interp_root)
  cell_half <- list(
    dx = grid_steps$lon_step / 2,
    dy = grid_steps$lat_step / 2
  )

  rows <- lapply(cruises, function(cruise_id) {
    field <- load_cruise_field(cruise_id, interp_root)
    cells <- field_hypoxic_cells_by_layer(field, threshold, layers, grid_steps)
    if (is.null(cells)) {
      return(NULL)
    }
    cells$cruise_id <- cruise_id
    cells
  })

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame())
  }

  all_cells <- do.call(rbind, rows)
  out <- all_cells |>
    dplyr::group_by(
      .data$longitude,
      .data$latitude,
      .data$layer_id,
      .data$layer_label
    ) |>
    dplyr::summarize(
      n_cruises = dplyr::n_distinct(.data$cruise_id),
      n_hypoxic = sum(.data$hypoxic),
      pct_hypoxic = 100 * sum(.data$hypoxic) / dplyr::n_distinct(.data$cruise_id),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      layer_label = factor(.data$layer_label, levels = layer_range_labels(layers))
    ) |>
    as.data.frame()

  attr(out, "cell_half") <- cell_half
  out
}

hypoxic_frequency_map_bounds <- function(frequency_df) {
  active <- frequency_df[is.finite(frequency_df$pct_hypoxic) & frequency_df$pct_hypoxic > 0, , drop = FALSE]
  if (nrow(active) == 0) {
    return(NULL)
  }

  half <- attr(frequency_df, "cell_half")
  if (is.null(half)) {
    half <- field_cell_half_widths(active$longitude, active$latitude)
  }

  list(
    lon_min = min(active$longitude, na.rm = TRUE) - half$dx,
    lon_max = max(active$longitude, na.rm = TRUE) + half$dx,
    lat_min = min(active$latitude, na.rm = TRUE) - half$dy,
    lat_max = max(active$latitude, na.rm = TRUE) + half$dy
  )
}

build_hypoxic_frequency_map_payload <- function(frequency_df, layers) {
  if (is.null(frequency_df) || nrow(frequency_df) == 0) {
    return(list())
  }

  cell_half <- attr(frequency_df, "cell_half")
  depth_layers <- list()

  for (layer in layers) {
    slice <- frequency_df[frequency_df$layer_id == layer$id, , drop = FALSE]
    slice <- slice[slice$pct_hypoxic > 0, , drop = FALSE]
    if (nrow(slice) == 0) {
      next
    }

    slice <- add_field_cell_bounds(slice, half = cell_half)

    cells <- lapply(seq_len(nrow(slice)), function(i) {
      row <- slice[i, , drop = FALSE]
      list(
        lon = as.numeric(row$longitude),
        lat = as.numeric(row$latitude),
        lon1 = as.numeric(row$lon1),
        lon2 = as.numeric(row$lon2),
        lat1 = as.numeric(row$lat1),
        lat2 = as.numeric(row$lat2),
        pct = as.numeric(row$pct_hypoxic),
        n_cruises = as.integer(row$n_cruises),
        n_hypoxic = as.integer(row$n_hypoxic),
        popup = sprintf(
          paste0(
            "Hypoxic in %d of %d cruises (%.0f%%)<br>",
            "Lat: %.5f<br>Lon: %.5f"
          ),
          row$n_hypoxic,
          row$n_cruises,
          row$pct_hypoxic,
          row$latitude,
          row$longitude
        ),
        label = sprintf(
          "%.0f%% hypoxic (%d/%d cruises)",
          row$pct_hypoxic,
          row$n_hypoxic,
          row$n_cruises
        )
      )
    })

    depth_layers[[length(depth_layers) + 1L]] <- list(
      label = layer$range_label,
      n_cells = length(cells),
      cells = cells
    )
  }

  depth_layers
}
