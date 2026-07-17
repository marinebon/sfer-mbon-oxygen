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

#' Per-cruise share of interpolated grid cells hypoxic in each depth layer.
hypoxic_field_extent_by_cruise <- function(
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
  dates <- cruise_dates_from_mapping()

  rows <- lapply(cruises, function(cruise_id) {
    field <- load_cruise_field(cruise_id, interp_root)
    cells <- field_hypoxic_cells_by_layer(field, threshold, layers, grid_steps)
    if (is.null(cells)) {
      return(NULL)
    }

    cells |>
      dplyr::group_by(.data$layer_id, .data$layer_label) |>
      dplyr::summarize(
        pct_hypoxic = 100 * mean(.data$hypoxic),
        n_cells = dplyr::n(),
        n_hypoxic_cells = sum(.data$hypoxic),
        .groups = "drop"
      ) |>
      dplyr::mutate(cruise_id = cruise_id)
  })

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame())
  }

  out <- do.call(rbind, rows)
  out <- dplyr::left_join(out, dates, by = "cruise_id")
  out |>
    dplyr::mutate(
      layer_label = factor(.data$layer_label, levels = layer_range_labels(layers))
    ) |>
    as.data.frame()
}

#' Aggregate cruise-level hypoxia onto an ISO year × week calendar grid.
prepare_hypoxic_calendar_heatmap <- function(cruise_extent) {
  if (is.null(cruise_extent) || nrow(cruise_extent) == 0) {
    return(cruise_extent)
  }

  cruise_extent |>
    dplyr::filter(!is.na(.data$cruise_date)) |>
    dplyr::mutate(
      iso_year = as.integer(format(.data$cruise_date, "%G")),
      iso_week = as.integer(format(.data$cruise_date, "%V"))
    ) |>
    dplyr::group_by(
      .data$iso_year,
      .data$iso_week,
      .data$layer_id,
      .data$layer_label
    ) |>
    dplyr::summarize(
      pct_hypoxic = max(.data$pct_hypoxic, na.rm = TRUE),
      n_cruises = dplyr::n(),
      cruise_labels = paste(
        paste0(.data$cruise_id, " (", format(.data$cruise_date, "%Y-%m-%d"), ")"),
        collapse = "; "
      ),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$iso_year, .data$iso_week, .data$layer_id)
}

hypoxic_calendar_fill_limits <- function(calendar_df) {
  c(1, 100)
}

hypoxic_calendar_year_spacing <- function(n_years) {
  if (n_years <= 6L) {
    2.1
  } else if (n_years <= 10L) {
    2.0
  } else {
    1.9
  }
}

plot_hypoxic_calendar_heatmap <- function(calendar_df, threshold) {
  if (is.null(calendar_df) || nrow(calendar_df) == 0) {
    return(NULL)
  }

  fill_limits <- hypoxic_calendar_fill_limits(calendar_df)
  fill_breaks <- c(1, 2, 5, 10, 20, 50, 100)
  years <- sort(unique(calendar_df$iso_year))
  year_spacing <- hypoxic_calendar_year_spacing(length(years))
  tile_height <- year_spacing * 0.62

  plot_df <- calendar_df |>
    dplyr::mutate(
      year_y = match(.data$iso_year, years) * year_spacing,
      pct_hypoxic = pmax(.data$pct_hypoxic, fill_limits[1])
    )

  ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data$iso_week, y = .data$year_y, fill = .data$pct_hypoxic)
  ) +
    ggplot2::geom_tile(height = tile_height, width = 0.92, color = NA) +
    ggplot2::facet_wrap(
      ggplot2::vars(.data$layer_label),
      ncol = 1,
      scales = "fixed"
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "C",
      trans = "log10",
      limits = fill_limits,
      breaks = fill_breaks,
      labels = fill_breaks,
      name = "% grid\nhypoxic",
      oob = scales::squish,
      na.value = "#f0f0f0"
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq_along(years) * year_spacing,
      labels = years,
      expand = ggplot2::expansion(mult = c(0.03, 0.03))
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(1, 52, by = 4),
      expand = ggplot2::expansion(add = 0.6)
    ) +
    ggplot2::labs(
      x = "ISO week",
      y = NULL,
      title = paste0(
        "Hypoxic interpolated field extent by cruise week (O\u2082 < ",
        threshold,
        " mg/L)"
      ),
      caption = "Tile color uses log scale (1–100%). Multiple cruises in the same week show the maximum layer extent."
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(size = 10),
      panel.grid = ggplot2::element_blank(),
      panel.spacing.y = ggplot2::unit(12, "pt"),
      axis.text.y = ggplot2::element_text(
        size = 8,
        lineheight = 0.9,
        margin = ggplot2::margin(t = 6, b = 6, unit = "pt")
      ),
      axis.ticks.y = ggplot2::element_line(linewidth = 0.3)
    )
}

FIELD_VOLUME_DEPTH_BIN_M <- 4
HYPOXIC_VOLUME_ABSENT_VALUE <- -1

#' Per-voxel share of cruises hypoxic on a common snapped 3D grid.
hypoxic_field_volume_frequency <- function(
    threshold = DEFAULT_HYPOXIC_O2_THRESHOLD,
    interp_root = here::here("data", "interpolated"),
    subsample = 2L,
    depth_bin_m = FIELD_VOLUME_DEPTH_BIN_M) {
  cruises <- list_interpolated_cruises(interp_root)
  if (length(cruises) == 0) {
    return(data.frame())
  }

  grid_steps <- median_field_grid_steps(cruises, interp_root)
  rows <- lapply(cruises, function(cruise_id) {
    field <- load_cruise_field(cruise_id, interp_root)
    if (is.null(field) || nrow(field) == 0) {
      return(NULL)
    }

    field <- field[!is.na(field$dissolved_oxygen), , drop = FALSE]
    field |>
      dplyr::mutate(
        longitude = round(.data$longitude / grid_steps$lon_step) * grid_steps$lon_step,
        latitude = round(.data$latitude / grid_steps$lat_step) * grid_steps$lat_step,
        depth_m = round(.data$depth_m / depth_bin_m) * depth_bin_m,
        hypoxic = .data$dissolved_oxygen < threshold,
        cruise_id = cruise_id
      ) |>
      dplyr::group_by(
        .data$longitude,
        .data$latitude,
        .data$depth_m,
        .data$cruise_id
      ) |>
      dplyr::summarize(
        hypoxic = any(.data$hypoxic),
        .groups = "drop"
      )
  })

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame())
  }

  freq <- dplyr::bind_rows(rows) |>
    dplyr::group_by(.data$longitude, .data$latitude, .data$depth_m) |>
    dplyr::summarize(
      n_cruises = dplyr::n_distinct(.data$cruise_id),
      n_hypoxic = sum(.data$hypoxic),
      pct_hypoxic = pmin(
        100,
        100 * sum(.data$hypoxic) / dplyr::n_distinct(.data$cruise_id)
      ),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$n_hypoxic > 0)

  if (nrow(freq) == 0) {
    return(data.frame())
  }

  dense_grid <- densify_hypoxic_volume_grid(freq, subsample = subsample)
  attr(dense_grid, "grid_steps") <- grid_steps
  attr(dense_grid, "depth_bin_m") <- depth_bin_m
  dense_grid
}

densify_hypoxic_volume_grid <- function(freq, subsample = 2L) {
  if (is.null(freq) || nrow(freq) == 0) {
    return(freq)
  }

  subsample <- as.integer(subsample)
  if (!is.finite(subsample) || subsample < 1L) {
    subsample <- 1L
  }

  lons <- sort(unique(freq$longitude))
  lats <- sort(unique(freq$latitude))
  depths <- sort(unique(freq$depth_m))
  if (subsample > 1L) {
    lons <- lons[seq(1L, length(lons), by = subsample)]
    lats <- lats[seq(1L, length(lats), by = subsample)]
    depths <- depths[seq(1L, length(depths), by = subsample)]
  }

  grid <- expand.grid(
    longitude = lons,
    latitude = lats,
    depth_m = depths,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  out <- dplyr::left_join(
    grid,
    freq[, c("longitude", "latitude", "depth_m", "pct_hypoxic", "n_cruises", "n_hypoxic")],
    by = c("longitude", "latitude", "depth_m")
  )
  out$pct_hypoxic[is.na(out$pct_hypoxic)] <- 0
  out$n_cruises[is.na(out$n_cruises)] <- 0L
  out$n_hypoxic[is.na(out$n_hypoxic)] <- 0L
  out$volume_value <- out$pct_hypoxic / 100
  out
}

hypoxic_volume_fill_limits <- function(grid_df) {
  active <- grid_df$volume_value[
    is.finite(grid_df$volume_value) & grid_df$volume_value > 0
  ]
  if (length(active) == 0) {
    return(c(0.01, 1))
  }

  c(max(min(active), 0.01), 1)
}

hypoxic_pct_log_breaks <- function() {
  pct <- c(1, 2, 5, 10, 20, 50, 100)
  list(
    tickvals = log10(pct),
    ticktext = paste0(pct, "%")
  )
}

hypoxic_volume_log_limits <- function(grid_df) {
  c(0, log10(100))
}

prepare_hypoxic_volume_plot_data <- function(grid_df) {
  plot_df <- grid_df
  plot_df$volume_value_log <- ifelse(
    is.finite(plot_df$pct_hypoxic) & plot_df$pct_hypoxic > 0,
    log10(pmax(plot_df$pct_hypoxic, 1)),
    HYPOXIC_VOLUME_ABSENT_VALUE
  )
  plot_df
}

hypoxic_volume_opacityscale_log <- function(opacity, value_min = 0) {
  # Map only visible (log-scaled) hypoxic values to the target opacity.
  # Absent voxels use HYPOXIC_VOLUME_ABSENT_VALUE and are hidden via isomin.
  list(
    list(value_min, opacity),
    list(log10(100), opacity)
  )
}

hypoxic_pct_palette <- function(n = 256) {
  viridisLite::viridis(n, option = "C")
}

hypoxic_pct_colorscale_plotly <- function(stops = 9L) {
  cols <- hypoxic_pct_palette(as.integer(stops))
  vals <- seq(0, 1, length.out = length(cols))
  stats::setNames(
    Map(function(value, color) list(value, color), vals, cols),
    NULL
  )
}

hypoxic_volume_colorscale <- function() {
  hypoxic_pct_colorscale_plotly()
}

hypoxic_volume_opacityscale <- function(opacity = 0.6) {
  list(
    list(0, 0),
    list(0.001, opacity),
    list(1, opacity)
  )
}
