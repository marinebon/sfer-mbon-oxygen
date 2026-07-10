resolve_color_scales <- function() {
  if (exists("oxygen_color_domain", mode = "function")) {
    return(invisible(TRUE))
  }

  candidates <- character()
  if (requireNamespace("here", quietly = TRUE)) {
    candidates <- c(candidates, here::here("R/color_scales.R"))
  }
  candidates <- c(candidates, file.path(getwd(), "R/color_scales.R"))

  for (path in unique(candidates)) {
    if (nzchar(path) && file.exists(path)) {
      source(path, local = FALSE)
      return(invisible(TRUE))
    }
  }

  stop("Could not find R/color_scales.R")
}

resolve_color_scales()

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
    palette = OXYGEN_PALETTE(256),
    zlim = NULL,
    legend_title = ""
) {
  vals <- mat[is.finite(mat)]
  if (length(vals) == 0) {
    return(invisible(FALSE))
  }
  if (is.null(zlim)) {
    zlim <- range(vals)
  }
  cols <- if (is.function(palette)) palette(256) else palette

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  png(output_file, width = 480, height = 420, res = 96, bg = "white")
  on.exit(dev.off(), add = TRUE)

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)

  par(mar = c(0.2, 0.2, 0.2, 0.2), fig = c(0, 1, 0.16, 1))
  image(
    lons,
    lats,
    mat,
    col = cols,
    zlim = zlim,
    axes = FALSE,
    xlab = "",
    ylab = "",
    asp = 1
  )

  n <- length(cols)
  par(
    mar = c(1.1, 0.8, 0.6, 0.8),
    mgp = c(1.5, 0.2, 0),
    fig = c(0, 1, 0, 0.12),
    new = TRUE
  )
  bar_x <- seq(zlim[1], zlim[2], length.out = n)
  image(
    bar_x,
    1,
    t(matrix(seq_len(n), nrow = 1)),
    col = cols,
    xlab = "",
    ylab = "",
    axes = FALSE
  )
  axis(
    1,
    at = pretty(zlim, n = 5),
    labels = sprintf("%.1f", pretty(zlim, n = 5)),
    cex.axis = 0.65
  )
  if (nzchar(legend_title)) {
    mtext(legend_title, side = 3, line = 0.1, cex = 0.8)
  }

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
    output_file,
    palette = OXYGEN_PALETTE(256),
    zlim = oxygen_color_domain(field),
    legend_title = "O\u2082 (mg/L)"
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
    palette = ANOXIC_DEPTH_PALETTE(256),
    zlim = anoxic_depth_color_domain(field),
    legend_title = "Depth below threshold (m)"
  )
}
