#!/usr/bin/env Rscript

# install & load packages using librarian package
lib_path <- "~/R/library"
if (!requireNamespace("librarian", quietly = TRUE)) {
  install.packages("librarian", lib = lib_path)
}
library(librarian, lib.loc = lib_path)
librarian::lib_startup(librarian, lib = lib_path, global = TRUE)
shelf(
  dplyr,
  here,
  glue,
  readr,
  rerddap
)

# this database contains CTD data 
database <- "https://gcoos5.geos.tamu.edu/erddap/"
Sys.setenv(RERDDAP_DEFAULT_URL = database)

# Source the filename alignment function
source(here::here("R/align_raw_ctd_filename.R"))

# Read the mapping CSV to get dataset IDs
mapping <- read_csv(here::here("data/ctd_datasetid_cruisename_stationname_mapping.csv"), 
                    show_col_types = FALSE)

# Set folder location
loc <- here("data", "01_raw")
dir.create(loc, recursive = TRUE, showWarnings = FALSE)

# Download each dataset from the mapping
for (i in seq_len(nrow(mapping))) {
  dataset_id <- mapping$dataset_id[i]
  
  # Get aligned filename
  raw_filename <- paste0(dataset_id, ".csv")
  aligned_filename <- align_raw_ctd_filename(raw_filename)
  
  # create file path with aligned filename (directly in 01_raw/ctd)
  file_path <- here(loc, aligned_filename)
  
  # skip already downloaded files
  if (file.exists(file_path)) {
    cat("Skipping:", aligned_filename, "\n")
    next
  }
  
  cat("Downloading:", dataset_id, "->", aligned_filename, "\n")
  
  # download data
  out <- info(dataset_id)
  ctd_data <- tabledap(out, url = eurl(), store = disk())
  write.csv(ctd_data, file_path, row.names = FALSE)
}

cat("\nDownload complete!\n")
