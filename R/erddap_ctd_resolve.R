ERDDAP_BASE <- "https://gcoos5.geos.tamu.edu/erddap/"

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
