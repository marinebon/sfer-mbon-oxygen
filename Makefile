# SFER MBON Oxygen — data-to-report pipeline
#
# Prerequisites: R, Julia (1.6+), Quarto CLI
#   R packages: here, dplyr (installed automatically on first run)
#   Julia packages: DIVAnd, CSV, DataFrames (installed via julia/Project.toml)

SHELL := /bin/bash
.DEFAULT_GOAL := help

RAW_DIR := data/01_raw
# PROCESSED_DIR := data/processed
# INTERP_DIR := data/interpolated
# JULIA_PROJECT := julia

.PHONY: help download
# .PHONY: all process interpolate render publish clean

help:
	@echo "SFER MBON Oxygen pipeline"
	@echo ""
	@echo "Targets:"
	@echo "  make download     Download raw CTD data for each cruise in data/ctd_datasetid_cruisename_stationname_mapping.csv"
#	@echo "  make process      Clean and depth-bin CTD data (R)"
#	@echo "  make interpolate  Build gridded oxygen fields with DIVAnd.jl (Julia)"
#	@echo "  make render       Render Quarto reports for all cruises"
#	@echo "  make publish      Run full pipeline and publish site with quarto publish"
#	@echo "  make all          Alias for render"
#	@echo "  make clean        Remove generated data and rendered site output"

# all: render

download: data/ctd_datasetid_cruisename_stationname_mapping.csv
	Rscript scripts/download_cruises.R

# process: download
# 	Rscript scripts/clean_bin_ctd.R $(MAPPING_CSV)
#
# interpolate: process
# 	julia --project=$(JULIA_PROJECT) scripts/interpolate_cruise.jl $(MAPPING_CSV)
#
# render: interpolate
# 	quarto render
#
# publish: render
# 	quarto publish
#
# clean:
# 	rm -rf $(RAW_DIR) $(PROCESSED_DIR) $(INTERP_DIR) _site .quarto
# 	find . -path '*/batched_reports/*' -delete 2>/dev/null || true
