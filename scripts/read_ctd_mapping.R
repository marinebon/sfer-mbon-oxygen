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
