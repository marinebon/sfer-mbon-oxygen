# Station x cruise sampling design summaries, built directly from the
# dataset/cruise/station mapping table (independent of QC/interpolation
# status, so this works even before `make process`/`make interpolate`).

load_station_cruise_mapping <- function(
    csv_path = here::here("data", "ctd_datasetid_cruisename_stationname_mapping.csv")) {
  empty <- data.frame(
    cruise_id = character(),
    station = character(),
    cruise_date = as.Date(character()),
    longitude = double(),
    latitude = double(),
    stringsAsFactors = FALSE
  )

  if (!file.exists(csv_path)) {
    return(empty)
  }

  mapping <- read.csv(csv_path, stringsAsFactors = FALSE)
  required <- c("cruise_id", "station", "date", "lon", "lat")
  if (!all(required %in% names(mapping))) {
    return(empty)
  }

  mapping |>
    dplyr::transmute(
      cruise_id = as.character(.data$cruise_id),
      station = as.character(.data$station),
      cruise_date = as.Date(.data$date, format = "%m/%d/%Y"),
      longitude = as.numeric(.data$lon),
      latitude = as.numeric(.data$lat)
    ) |>
    dplyr::filter(!is.na(.data$cruise_date), nzchar(.data$station)) |>
    # a station can be re-occupied (repeat cast) within one cruise; presence
    # only cares whether it was visited, so keep the first occurrence
    dplyr::arrange(.data$cruise_id, .data$station) |>
    dplyr::distinct(.data$cruise_id, .data$station, .keep_all = TRUE)
}

# One row per station: representative location + how often/when it was used.
station_summary <- function(mapping) {
  if (nrow(mapping) == 0) {
    return(data.frame())
  }

  mapping |>
    dplyr::group_by(.data$station) |>
    dplyr::summarize(
      longitude = mean(.data$longitude, na.rm = TRUE),
      latitude = mean(.data$latitude, na.rm = TRUE),
      n_cruises = dplyr::n_distinct(.data$cruise_id),
      first_cruise_date = min(.data$cruise_date, na.rm = TRUE),
      last_cruise_date = max(.data$cruise_date, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$n_cruises), .data$station)
}

# One row per cruise: how many distinct stations were occupied.
cruise_station_counts <- function(mapping) {
  if (nrow(mapping) == 0) {
    return(data.frame())
  }

  mapping |>
    dplyr::group_by(.data$cruise_id) |>
    dplyr::summarize(
      cruise_date = min(.data$cruise_date, na.rm = TRUE),
      n_stations = dplyr::n_distinct(.data$station),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$cruise_date)
}

# Cumulative count of distinct stations ever visited, through each cruise
# in chronological order (growth of the sampling network over time).
cumulative_station_growth <- function(mapping) {
  counts <- cruise_station_counts(mapping)
  if (nrow(counts) == 0) {
    return(data.frame())
  }

  seen <- character()
  cumulative_n <- integer(nrow(counts))
  for (i in seq_len(nrow(counts))) {
    cruise <- counts$cruise_id[i]
    stations_this_cruise <- mapping$station[mapping$cruise_id == cruise]
    seen <- union(seen, stations_this_cruise)
    cumulative_n[i] <- length(seen)
  }

  counts$cumulative_stations <- cumulative_n
  counts
}

# Stations gained/dropped relative to the immediately preceding cruise
# (in chronological order). Positive = newly occupied, negative = not
# revisited this time.
station_turnover <- function(mapping) {
  counts <- cruise_station_counts(mapping)
  if (nrow(counts) < 2) {
    return(data.frame())
  }

  station_sets <- lapply(counts$cruise_id, function(cruise) {
    mapping$station[mapping$cruise_id == cruise]
  })

  rows <- lapply(seq(2, nrow(counts)), function(i) {
    prev <- station_sets[[i - 1]]
    curr <- station_sets[[i]]
    data.frame(
      cruise_id = counts$cruise_id[i],
      cruise_date = counts$cruise_date[i],
      added = length(setdiff(curr, prev)),
      dropped = -length(setdiff(prev, curr)),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

# Tidy presence matrix (long form): every cruise x station combination,
# with `present` TRUE/FALSE. Stations are ordered by first appearance so
# the presence heatmap reads as a staircase of the network growing.
station_presence_long <- function(mapping) {
  if (nrow(mapping) == 0) {
    return(data.frame())
  }

  stations <- station_summary(mapping) |>
    dplyr::arrange(.data$first_cruise_date, .data$station)
  cruises <- cruise_station_counts(mapping) |>
    dplyr::arrange(.data$cruise_date)

  grid <- expand.grid(
    station = stations$station,
    cruise_id = cruises$cruise_id,
    stringsAsFactors = FALSE
  )
  grid <- dplyr::left_join(grid, cruises, by = "cruise_id")

  present_pairs <- paste(mapping$cruise_id, mapping$station)
  grid$present <- paste(grid$cruise_id, grid$station) %in% present_pairs

  grid$station <- factor(grid$station, levels = rev(stations$station))
  grid$cruise_id <- factor(grid$cruise_id, levels = cruises$cruise_id)
  grid
}
