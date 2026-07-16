DEFAULT_HYPOXIC_O2_THRESHOLD <- 2.0

source(here::here("R/depth_layers.R"))

list_cruises_with_clean_ctd <- function(
    clean_root = here::here("data", "02_clean")) {
  if (!dir.exists(clean_root)) {
    return(character())
  }

  if (!exists("list_cruise_ids_from_clean", mode = "function")) {
    source(here::here("scripts/read_ctd_mapping.R"), local = TRUE)
  }

  list_cruise_ids_from_clean(clean_root)
}

cruise_dates_from_mapping <- function(
    csv_path = here::here("data", "ctd_datasetid_cruisename_stationname_mapping.csv")) {
  if (!file.exists(csv_path)) {
    return(data.frame(
      cruise_id = character(),
      cruise_date = as.Date(character()),
      stringsAsFactors = FALSE
    ))
  }

  mapping <- read.csv(csv_path, stringsAsFactors = FALSE)
  if (!all(c("cruise_id", "date") %in% names(mapping))) {
    return(data.frame(
      cruise_id = character(),
      cruise_date = as.Date(character()),
      stringsAsFactors = FALSE
    ))
  }

  mapping$cruise_date <- as.Date(mapping$date, format = "%m/%d/%Y")
  mapping |>
    dplyr::group_by(.data$cruise_id) |>
    dplyr::summarize(
      cruise_date = min(.data$cruise_date, na.rm = TRUE),
      .groups = "drop"
    )
}

load_cruise_observations <- function(
    cruise_id,
    clean_root = here::here("data", "02_clean")) {
  prefix <- paste0("SFER_CTD_", cruise_id, "_")
  clean_files <- list.files(
    clean_root,
    pattern = paste0("^", prefix, ".*\\.csv$"),
    full.names = TRUE
  )
  clean_files <- clean_files[basename(clean_files) != "processing_log.csv"]
  if (length(clean_files) == 0) {
    return(NULL)
  }

  if (!exists("load_clean_ctd_observations", mode = "function")) {
    source(here::here("R/load_clean_ctd_observations.R"), local = TRUE)
  }

  obs <- load_clean_ctd_observations(clean_files)
  obs$cruise_id <- cruise_id
  obs
}

load_all_cruise_observations <- function(
    clean_root = here::here("data", "02_clean")) {
  cruises <- list_cruises_with_clean_ctd(clean_root)
  if (length(cruises) == 0) {
    return(data.frame())
  }

  rows <- lapply(cruises, function(cruise_id) {
    load_cruise_observations(cruise_id, clean_root)
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame())
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

depth_layers_for_observations <- function(observations) {
  obs <- valid_observations(observations)
  if (nrow(obs) == 0) {
    return(oxygen_depth_layers())
  }

  oxygen_depth_layers(max_depth = max(obs$depth, na.rm = TRUE))
}

valid_observations <- function(observations) {
  if (is.null(observations) || nrow(observations) == 0) {
    return(observations)
  }

  observations |>
    dplyr::filter(
      is.finite(.data$depth),
      is.finite(.data$dissolved_oxygen)
    )
}

summarize_hypoxic_casts <- function(observations, threshold) {
  obs <- valid_observations(observations)
  if (is.null(obs) || nrow(obs) == 0) {
    return(NULL)
  }

  obs |>
    dplyr::group_by(
      .data$cast_id,
      .data$station,
      .data$longitude,
      .data$latitude,
      .data$cruise_id
    ) |>
    dplyr::summarize(
      min_o2_mg_l = min(.data$dissolved_oxygen, na.rm = TRUE),
      shallowest_hypoxic_depth_m = {
        below <- .data$dissolved_oxygen < threshold
        if (!any(below)) {
          NA_real_
        } else {
          min(.data$depth[below], na.rm = TRUE)
        }
      },
      .groups = "drop"
    ) |>
    dplyr::filter(.data$min_o2_mg_l < threshold)
}

#' Fraction of CTD observations below threshold in each depth bin.
hypoxic_extent_by_depth <- function(
    observations,
    threshold,
    bin_m = 4) {
  obs <- valid_observations(observations)
  if (is.null(obs) || nrow(obs) == 0) {
    return(NULL)
  }

  obs |>
    dplyr::mutate(depth_bin = floor(.data$depth / bin_m) * bin_m) |>
    dplyr::group_by(.data$depth_bin) |>
    dplyr::summarize(
      depth_m = .data$depth_bin[1],
      n_obs = dplyr::n(),
      n_hypoxic = sum(.data$dissolved_oxygen < threshold),
      pct_hypoxic = 100 * sum(.data$dissolved_oxygen < threshold) / dplyr::n(),
      .groups = "drop"
    ) |>
    as.data.frame()
}

#' Prepare binned depth summaries for heatmap display.
prepare_hypoxic_depth_heatmap <- function(
    depth_extent,
    bin_m = 4) {
  if (is.null(depth_extent) || nrow(depth_extent) == 0) {
    return(depth_extent)
  }

  depth_extent |>
    dplyr::filter(!is.na(.data$cruise_date)) |>
    dplyr::transmute(
      .data$cruise_id,
      .data$cruise_date,
      depth_bin = .data$depth_m,
      .data$n_obs,
      .data$n_hypoxic,
      .data$pct_hypoxic
    ) |>
    dplyr::arrange(.data$cruise_date, .data$cruise_id, .data$depth_bin)
}

summarize_cruise_hypoxic <- function(
    cruise_id,
    threshold,
    observations = NULL,
    clean_root = here::here("data", "02_clean")) {
  if (is.null(observations)) {
    observations <- load_cruise_observations(cruise_id, clean_root)
  }

  obs <- valid_observations(observations)
  if (is.null(obs) || nrow(obs) == 0) {
    return(NULL)
  }

  cast_summary <- obs |>
    dplyr::group_by(.data$cast_id) |>
    dplyr::summarize(
      min_o2 = min(.data$dissolved_oxygen, na.rm = TRUE),
      shallowest_depth_m = min(.data$depth, na.rm = TRUE),
      shallowest_o2 = .data$dissolved_oxygen[which.min(.data$depth)],
      longitude = dplyr::first(.data$longitude),
      latitude = dplyr::first(.data$latitude),
      .groups = "drop"
    )

  n_casts <- nrow(cast_summary)
  n_hypoxic_casts <- sum(cast_summary$min_o2 < threshold, na.rm = TRUE)

  pct_casts_hypoxic <- if (n_casts > 0) {
    100 * n_hypoxic_casts / n_casts
  } else {
    NA_real_
  }

  pct_surface_hypoxic <- if (n_casts > 0) {
    100 * mean(cast_summary$shallowest_o2 < threshold, na.rm = TRUE)
  } else {
    NA_real_
  }

  pct_obs_hypoxic <- 100 * mean(obs$dissolved_oxygen < threshold, na.rm = TRUE)

  shallow_hypoxic_depths <- obs |>
    dplyr::group_by(.data$cast_id) |>
    dplyr::summarize(
      shallowest_hypoxic_depth_m = {
        below <- .data$dissolved_oxygen < threshold
        if (!any(below)) {
          NA_real_
        } else {
          min(.data$depth[below], na.rm = TRUE)
        }
      },
      .groups = "drop"
    ) |>
    dplyr::filter(is.finite(.data$shallowest_hypoxic_depth_m)) |>
    dplyr::pull(.data$shallowest_hypoxic_depth_m)

  data.frame(
    cruise_id = cruise_id,
    n_obs = nrow(obs),
    n_casts = n_casts,
    n_hypoxic_casts = n_hypoxic_casts,
    pct_casts_hypoxic = pct_casts_hypoxic,
    pct_surface_hypoxic = pct_surface_hypoxic,
    pct_obs_hypoxic = pct_obs_hypoxic,
    median_shallow_hypoxic_depth_m = if (length(shallow_hypoxic_depths) > 0) {
      stats::median(shallow_hypoxic_depths)
    } else {
      NA_real_
    },
    min_obs_o2_mg_l = min(obs$dissolved_oxygen, na.rm = TRUE),
    max_obs_o2_mg_l = max(obs$dissolved_oxygen, na.rm = TRUE),
    mean_lon = mean(cast_summary$longitude, na.rm = TRUE),
    mean_lat = mean(cast_summary$latitude, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

summarize_all_cruises_hypoxic <- function(
    threshold = DEFAULT_HYPOXIC_O2_THRESHOLD,
    clean_root = here::here("data", "02_clean"),
    observations = NULL) {
  if (is.null(observations)) {
    observations <- load_all_cruise_observations(clean_root)
  }
  if (nrow(observations) == 0) {
    return(data.frame())
  }

  cruises <- sort(unique(observations$cruise_id))
  rows <- lapply(cruises, function(cruise_id) {
    cruise_obs <- observations[observations$cruise_id == cruise_id, , drop = FALSE]
    summarize_cruise_hypoxic(
      cruise_id,
      threshold,
      observations = cruise_obs
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame())
  }

  summary <- do.call(rbind, rows)
  dates <- cruise_dates_from_mapping()
  summary <- dplyr::left_join(summary, dates, by = "cruise_id")
  summary <- summary[order(summary$cruise_date, summary$cruise_id), , drop = FALSE]
  rownames(summary) <- NULL
  summary
}

hypoxic_extent_by_depth_all_cruises <- function(
    threshold = DEFAULT_HYPOXIC_O2_THRESHOLD,
    clean_root = here::here("data", "02_clean"),
    observations = NULL,
    bin_m = 4) {
  if (is.null(observations)) {
    observations <- load_all_cruise_observations(clean_root)
  }
  if (nrow(observations) == 0) {
    return(data.frame())
  }

  dates <- cruise_dates_from_mapping()
  cruises <- sort(unique(observations$cruise_id))
  rows <- lapply(cruises, function(cruise_id) {
    cruise_obs <- observations[observations$cruise_id == cruise_id, , drop = FALSE]
    extent <- hypoxic_extent_by_depth(cruise_obs, threshold, bin_m = bin_m)
    if (is.null(extent)) {
      return(NULL)
    }
    extent$cruise_id <- cruise_id
    extent
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame())
  }

  out <- do.call(rbind, rows)
  out <- dplyr::left_join(out, dates, by = "cruise_id")
  out[order(out$cruise_date, out$cruise_id, out$depth_m), , drop = FALSE]
}

hypoxic_observations_all_cruises <- function(
    threshold = DEFAULT_HYPOXIC_O2_THRESHOLD,
    clean_root = here::here("data", "02_clean"),
    observations = NULL) {
  if (is.null(observations)) {
    observations <- load_all_cruise_observations(clean_root)
  }
  if (nrow(observations) == 0) {
    return(data.frame())
  }

  dates <- cruise_dates_from_mapping()
  cruises <- sort(unique(observations$cruise_id))
  rows <- lapply(cruises, function(cruise_id) {
    cruise_obs <- observations[observations$cruise_id == cruise_id, , drop = FALSE]
    cast_summary <- summarize_hypoxic_casts(cruise_obs, threshold)
    if (is.null(cast_summary) || nrow(cast_summary) == 0) {
      return(NULL)
    }
    cast_summary
  })

  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) {
    return(data.frame())
  }

  out <- do.call(rbind, rows)
  out <- dplyr::left_join(out, dates, by = "cruise_id")
  out[order(out$cruise_date, out$cruise_id, out$shallowest_hypoxic_depth_m), , drop = FALSE]
}

cruise_labels_ordered <- function(depth_extent) {
  depth_extent |>
    dplyr::filter(!is.na(.data$cruise_date)) |>
    dplyr::arrange(.data$cruise_date, .data$cruise_id) |>
    dplyr::distinct(.data$cruise_id, .data$cruise_date) |>
    dplyr::mutate(
      label = paste0(.data$cruise_id, " (", format(.data$cruise_date, "%Y-%m"), ")")
    ) |>
    dplyr::pull(.data$label)
}
