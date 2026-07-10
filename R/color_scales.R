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
