MAPPING_CSV <- "data/ctd_datasetid_cruisename_stationname_mapping.csv"

read_ctd_mapping <- function(csv_path = here::here(MAPPING_CSV)) {
  if (!file.exists(csv_path)) {
    return(NULL)
  }
  read.csv(csv_path, stringsAsFactors = FALSE)
}

list_cruise_ids_from_raw <- function(raw_root = here::here("data", "01_raw")) {
  if (!dir.exists(raw_root)) {
    return(character())
  }
  dirs <- list.dirs(raw_root, full.names = FALSE, recursive = FALSE)
  dirs <- dirs[nzchar(dirs)]
  sort(dirs)
}

list_cruise_ids_from_clean <- function(clean_root = here::here("data", "02_clean")) {
  if (!dir.exists(clean_root)) {
    return(character())
  }

  files <- list.files(
    clean_root,
    pattern = "^SFER_CTD_.*\\.csv$",
    full.names = FALSE
  )
  files <- files[files != "processing_log.csv"]
  if (length(files) == 0) {
    return(character())
  }

  parsed <- lapply(sub("\\.csv$", "", files), function(cast_id) {
    if (!exists("parse_sfer_ctd_id", mode = "function")) {
      source(here::here("R/erddap_ctd_resolve.R"), local = TRUE)
    }
    parse_sfer_ctd_id(cast_id)
  })
  parsed <- Filter(Negate(is.null), parsed)
  if (length(parsed) == 0) {
    return(character())
  }

  sort(unique(vapply(parsed, function(x) x$cruise_id, character(1L))))
}

list_cruise_ids <- function() {
  ids <- list_cruise_ids_from_clean()
  if (length(ids) > 0) {
    return(ids)
  }

  ids <- list_cruise_ids_from_raw()
  if (length(ids) > 0) {
    return(ids)
  }

  mapping <- read_ctd_mapping()
  if (!is.null(mapping) && "cruise_id" %in% names(mapping)) {
    return(sort(unique(mapping$cruise_id)))
  }

  if (!exists("discover_sfer_cruise_ids", mode = "function")) {
    source(here::here("R/erddap_ctd_resolve.R"), local = TRUE)
  }
  discover_sfer_cruise_ids()
}

unique_cruise_ids <- function(mapping = NULL) {
  if (!is.null(mapping) && "cruise_id" %in% names(mapping)) {
    return(sort(unique(mapping$cruise_id)))
  }
  list_cruise_ids()
}

cruise_ids_for_run <- function(mapping = NULL) {
  filter <- if (length(commandArgs(trailingOnly = TRUE)) > 0) {
    commandArgs(trailingOnly = TRUE)[1]
  } else {
    Sys.getenv("CRUISE", unset = "")
  }

  ids <- unique_cruise_ids(mapping)

  if (nzchar(filter)) {
    if (!(filter %in% ids)) {
      ids <- sort(unique(c(ids, filter)))
    }
    return(filter)
  }

  ids
}
