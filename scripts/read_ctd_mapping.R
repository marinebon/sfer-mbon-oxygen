MAPPING_CSV <- "data/ctd_datasetid_cruisename_stationname_mapping.csv"

read_ctd_mapping <- function(csv_path = here::here(MAPPING_CSV)) {
  if (!file.exists(csv_path)) {
    stop("CTD mapping file not found: ", csv_path)
  }
  read.csv(csv_path, stringsAsFactors = FALSE)
}

unique_cruise_ids <- function(mapping) {
  sort(unique(mapping$cruise_id))
}

cruise_ids_for_run <- function(mapping) {
  ids <- unique_cruise_ids(mapping)
  filter <- if (length(commandArgs(trailingOnly = TRUE)) > 0) {
    commandArgs(trailingOnly = TRUE)[1]
  } else {
    Sys.getenv("CRUISE", unset = "")
  }
  if (nzchar(filter)) {
    if (!(filter %in% ids)) {
      stop("Cruise ", filter, " not found in ", MAPPING_CSV)
    }
    return(filter)
  }
  ids
}
