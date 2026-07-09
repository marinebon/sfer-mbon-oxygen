field_to_matrix <- function(grid, value_col) {
  lons <- sort(unique(grid$longitude))
  lats <- sort(unique(grid$latitude))
  mat <- matrix(NA_real_, nrow = length(lons), ncol = length(lats))
  lon_idx <- match(grid$longitude, lons)
  lat_idx <- match(grid$latitude, lats)

  for (k in seq_len(nrow(grid))) {
    mat[lon_idx[k], lat_idx[k]] <- grid[[value_col]][k]
  }

  list(lons = lons, lats = lats, mat = mat)
}

save_field_matrix_thumbnail <- function(
    lons,
    lats,
    mat,
    output_file,
    palette = viridisLite::turbo(256)
) {
  vals <- mat[is.finite(mat)]
  if (length(vals) == 0) {
    return(invisible(FALSE))
  }

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  png(output_file, width = 480, height = 360, res = 96, bg = "white")
  on.exit(dev.off(), add = TRUE)
  par(mar = c(0.2, 0.2, 0.2, 0.2))
  image(
    lons,
    lats,
    mat,
    col = palette,
    zlim = range(vals),
    axes = FALSE,
    xlab = "",
    ylab = "",
    asp = 1
  )
  invisible(TRUE)
}

save_oxygen_field_thumbnail <- function(
    field,
    output_file,
    depth_min = 3,
    depth_max = 10
) {
  if (is.null(field) || nrow(field) == 0) {
    return(invisible(FALSE))
  }

  in_layer <- field$depth_m >= depth_min &
    field$depth_m < depth_max &
    !is.na(field$dissolved_oxygen)
  slice <- field[in_layer, , drop = FALSE]
  if (nrow(slice) == 0) {
    slice <- field[!is.na(field$dissolved_oxygen), , drop = FALSE]
  }
  if (nrow(slice) == 0) {
    return(invisible(FALSE))
  }

  grid <- stats::aggregate(
    dissolved_oxygen ~ longitude + latitude,
    data = slice,
    FUN = mean,
    na.rm = TRUE
  )
  mat_info <- field_to_matrix(grid, "dissolved_oxygen")
  save_field_matrix_thumbnail(
    mat_info$lons,
    mat_info$lats,
    mat_info$mat,
    output_file
  )
}

save_anoxic_depth_thumbnail <- function(field, output_file, observations = NULL) {
  if (is.null(field) || nrow(field) == 0) {
    return(invisible(FALSE))
  }

  threshold <- default_anoxic_threshold(field, observations)
  grid <- anoxic_depth_grid(field, threshold)
  if (is.null(grid) || nrow(grid) == 0) {
    return(invisible(FALSE))
  }

  grid <- grid[is.finite(grid$anoxic_depth_m), , drop = FALSE]
  if (nrow(grid) == 0) {
    return(invisible(FALSE))
  }

  mat_info <- field_to_matrix(grid, "anoxic_depth_m")
  save_field_matrix_thumbnail(
    mat_info$lons,
    mat_info$lats,
    mat_info$mat,
    output_file,
    palette = viridisLite::viridis(256)
  )
}
