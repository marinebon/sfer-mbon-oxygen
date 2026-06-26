ERDDAP_BASE <- "https://gcoos5.geos.tamu.edu/erddap/"

normalize_station_suffix <- function(station) {
  gsub("[.]", "_", as.character(station))
}

erddap_dataset_id <- function(cruise_id, station) {
  paste0("SFER_CTD_", cruise_id, "_", normalize_station_suffix(station))
}

fetch_cruise_erddap_ids <- function(cruise_id, base_url = ERDDAP_BASE) {
  search_url <- paste0(
    base_url,
    "search/index.csv?searchFor=",
    utils::URLencode(paste0("SFER_CTD_", cruise_id), reserved = TRUE)
  )

  result <- tryCatch(
    read.csv(search_url, stringsAsFactors = FALSE),
    error = function(e) {
      warning("ERDDAP catalog search failed for ", cruise_id, ": ", conditionMessage(e))
      return(NULL)
    }
  )

  if (is.null(result) || nrow(result) == 0) {
    return(character())
  }

  id_col <- if ("Dataset.ID" %in% names(result)) "Dataset.ID" else names(result)[ncol(result)]
  ids <- result[[id_col]]
  ids <- unique(ids[ids != "Dataset ID"])
  ids[grepl(paste0("^SFER_CTD_", cruise_id, "_"), ids)]
}

resolve_erddap_dataset_id <- function(cruise_id, station, catalog_ids = character()) {
  expected <- erddap_dataset_id(cruise_id, station)

  if (length(catalog_ids) == 0) {
    return(expected)
  }

  if (expected %in% catalog_ids) {
    return(expected)
  }

  suffix <- normalize_station_suffix(station)
  pattern <- paste0("_", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", suffix), "$")
  matches <- catalog_ids[grepl(pattern, catalog_ids)]

  if (length(matches) == 1) {
    return(matches[[1]])
  }

  if (length(matches) > 1) {
    warning(
      "Multiple ERDDAP datasets match ", cruise_id, " station ", station,
      "; using ", matches[[1]]
    )
    return(matches[[1]])
  }

  expected
}

erddap_dataset_exists <- function(dataset_id, base_url = ERDDAP_BASE) {
  old_url <- Sys.getenv("RERDDAP_DEFAULT_URL")
  on.exit(Sys.setenv(RERDDAP_DEFAULT_URL = old_url), add = TRUE)
  Sys.setenv(RERDDAP_DEFAULT_URL = base_url)

  tryCatch(
    {
      info(dataset_id)
      TRUE
    },
    error = function(e) FALSE
  )
}
