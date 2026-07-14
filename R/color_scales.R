# Fixed domains derived from all cleaned observations and interpolated fields.
# Set COLOR_SCALE_RESCAN=true to recompute from data on the next render.
OXYGEN_COLOR_DOMAIN <- c(0, 5.5)
ANOXIC_DEPTH_COLOR_DOMAIN <- c(0, 20)
ANOXIC_DEPTH_COLOR_LIMITS <- c(0, 584.141)

OXYGEN_PALETTE <- function(n = 256) {
  viridisLite::turbo(n)
}

ANOXIC_DEPTH_PALETTE <- function(n = 256) {
  viridisLite::viridis(n)
}

.color_domain_cache <- new.env(parent = emptyenv())

read_oxygen_values <- function(
    clean_root = file.path("data", "02_clean"),
    interpolated_root = file.path("data", "interpolated")
) {
  values <- numeric()

  if (dir.exists(clean_root)) {
    clean_files <- list.files(
      clean_root,
      pattern = "\\.csv$",
      full.names = TRUE
    )
    clean_files <- clean_files[basename(clean_files) != "processing_log.csv"]
    for (path in clean_files) {
      df <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
      if (!is.null(df) && "dissolved_oxygen" %in% names(df)) {
        values <- c(values, as.numeric(df$dissolved_oxygen))
      }
    }
  }

  if (dir.exists(interpolated_root)) {
    field_files <- list.files(
      interpolated_root,
      pattern = "oxygen_field\\.csv$",
      full.names = TRUE,
      recursive = TRUE
    )
    for (path in field_files) {
      df <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
      if (!is.null(df) && "dissolved_oxygen" %in% names(df)) {
        values <- c(values, as.numeric(df$dissolved_oxygen))
      }
    }
  }

  values[is.finite(values)]
}

read_field_depth_values <- function(
    interpolated_root = file.path("data", "interpolated")
) {
  values <- numeric()
  if (!dir.exists(interpolated_root)) {
    return(values)
  }

  field_files <- list.files(
    interpolated_root,
    pattern = "oxygen_field\\.csv$",
    full.names = TRUE,
    recursive = TRUE
  )
  for (path in field_files) {
    df <- tryCatch(read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL)
    if (!is.null(df) && "depth_m" %in% names(df)) {
      values <- c(values, as.numeric(df$depth_m))
    }
  }

  values[is.finite(values)]
}

nice_upper_limit <- function(x, step = 0.5) {
  if (!is.finite(x) || x <= 0) {
    return(step)
  }
  step * ceiling(x / step)
}

global_oxygen_color_domain <- function() {
  if (!is.null(.color_domain_cache$oxygen)) {
    return(.color_domain_cache$oxygen)
  }

  if (!identical(Sys.getenv("COLOR_SCALE_RESCAN"), "true")) {
    .color_domain_cache$oxygen <- OXYGEN_COLOR_DOMAIN
    return(OXYGEN_COLOR_DOMAIN)
  }

  root <- Sys.getenv("COLOR_SCALE_ROOT", unset = getwd())
  values <- read_oxygen_values(
    clean_root = file.path(root, "data", "02_clean"),
    interpolated_root = file.path(root, "data", "interpolated")
  )

  if (length(values) == 0) {
    domain <- OXYGEN_COLOR_DOMAIN
  } else {
    hi <- nice_upper_limit(as.numeric(stats::quantile(values, probs = 0.995, na.rm = TRUE)))
    domain <- c(0, max(hi, 1))
  }

  .color_domain_cache$oxygen <- domain
  domain
}

global_anoxic_depth_color_limits <- function() {
  if (!is.null(.color_domain_cache$anoxic_depth_limits)) {
    return(.color_domain_cache$anoxic_depth_limits)
  }

  if (!identical(Sys.getenv("COLOR_SCALE_RESCAN"), "true")) {
    .color_domain_cache$anoxic_depth_limits <- ANOXIC_DEPTH_COLOR_LIMITS
    return(ANOXIC_DEPTH_COLOR_LIMITS)
  }

  root <- Sys.getenv("COLOR_SCALE_ROOT", unset = getwd())
  depths <- read_field_depth_values(
    interpolated_root = file.path(root, "data", "interpolated")
  )

  if (length(depths) == 0) {
    limits <- ANOXIC_DEPTH_COLOR_LIMITS
  } else {
    limits <- c(0, max(depths))
  }

  .color_domain_cache$anoxic_depth_limits <- limits
  limits
}

anoxic_depth_color_domain <- function(field = NULL) {
  ANOXIC_DEPTH_COLOR_DOMAIN
}

anoxic_depth_color_limits <- function(field = NULL) {
  global_anoxic_depth_color_limits()
}

oxygen_color_domain <- function(field = NULL, observations = NULL) {
  global_oxygen_color_domain()
}

palette_color_for_values <- function(values, domain, palette) {
  cols <- if (is.function(palette)) palette(256) else palette
  n <- length(cols)
  t <- (values - domain[1]) / (domain[2] - domain[1])
  t <- pmax(0, pmin(1, t))
  idx <- round(t * (n - 1)) + 1
  cols[idx]
}

save_matrix_rect_thumbnail <- function(
    lons,
    lats,
    mat,
    output_file,
    palette,
    zlim,
    legend_title = "",
    cell_dx = NULL,
    cell_dy = NULL
) {
  vals <- mat[is.finite(mat)]
  if (length(vals) == 0) {
    return(invisible(FALSE))
  }

  cols <- if (is.function(palette)) palette(256) else palette
  dx <- if (!is.null(cell_dx)) {
    cell_dx
  } else if (length(lons) > 1) {
    diff(lons)[1]
  } else {
    0.02
  }
  dy <- if (!is.null(cell_dy)) {
    cell_dy
  } else if (length(lats) > 1) {
    diff(lats)[1]
  } else {
    0.02
  }

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  png(output_file, width = 480, height = 420, res = 96, bg = "white")
  on.exit(dev.off(), add = TRUE)

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)

  par(mar = c(0.2, 0.2, 0.2, 0.2), fig = c(0, 1, 0.16, 1))
  plot(
    NA,
    xlim = range(lons) + c(-dx / 2, dx / 2),
    ylim = range(lats) + c(-dy / 2, dy / 2),
    axes = FALSE,
    xlab = "",
    ylab = "",
    asp = 1
  )
  rect(
    par("usr")[1],
    par("usr")[3],
    par("usr")[2],
    par("usr")[4],
    col = "white",
    border = NA
  )

  for (i in seq_along(lons)) {
    for (j in seq_along(lats)) {
      value <- mat[i, j]
      if (!is.finite(value)) {
        next
      }
      rect(
        lons[i] - dx / 2,
        lats[j] - dy / 2,
        lons[i] + dx / 2,
        lats[j] + dy / 2,
        col = palette_color_for_values(value, zlim, cols),
        border = NA
      )
    }
  }

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
