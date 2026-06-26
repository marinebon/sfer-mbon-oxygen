align_raw_ctd_filename <- function(rawFilename){
  # Map the rawFilename to a standard name using the mapping spreadsheet
  # Example: WS21093_WS21093_STN_WS.csv -> WS21093_WS21093_WS21093_WS.csv
  
  # Load the mapping file (cache it if called multiple times)
  if (!exists("ctd_mapping_cache", envir = .GlobalEnv)) {
    .GlobalEnv$ctd_mapping_cache <- read.csv(
      here::here("data", "ctd_datasetid_cruisename_stationname_mapping.csv"),
      stringsAsFactors = FALSE
    )
  }
  mapping <- .GlobalEnv$ctd_mapping_cache
  
  # Remove .csv extension
  base_name <- sub("\\.csv$", "", rawFilename)
  
  # Look up the dataset_id in the mapping
  match_row <- mapping[mapping$dataset_id == base_name, ]
  
  if (nrow(match_row) > 0) {
    # Use first match (they should all be the same anyway)
    cruise_id <- match_row$cruise_id[1]
    station <- match_row$station[1]
    
    # Construct standardized filename: {cruise_id}_{cruise_id}_{cruise_id}_{station}.csv
    aligned_filename <- paste0(cruise_id, "_", cruise_id, "_", cruise_id, "_", station, ".csv")
    return(aligned_filename)
  } else {
    # If no mapping found, return the original filename unchanged
    warning(paste("No mapping found for:", rawFilename))
    return(rawFilename)
  }
}
