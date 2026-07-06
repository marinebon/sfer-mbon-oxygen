# SFER MBON Oxygen — data-to-report pipeline
#
# Prerequisites: R, Julia (1.6+), Quarto CLI
#   R packages: here, dplyr (installed automatically on first run)
#   Julia packages: DIVAnd, CSV, DataFrames (installed via julia/Project.toml)

SHELL := /bin/bash
.DEFAULT_GOAL := help

# MAPPING_CSV := data/ctd_datasetid_cruisename_stationname_mapping.csv
RAW_DIR := data/01_raw
PROCESSED_DIR := data/processed
INTERP_DIR := data/interpolated
# JULIA_PROJECT := julia

.PHONY: help download process report-process clean

help:
	@echo "SFER MBON Oxygen pipeline"
	@echo ""
	@echo "Targets:"
	@echo "  make download        Download raw CTD data for each cruise in data/ctd_datasetid_cruisename_stationname_mapping.csv"
	@echo "  make process         Clean raw CTD casts with oce into data/02_clean/"
	@echo "  make report-process  Render reports/processing_summary.qmd (run after make process)"
	@echo "  make clean           Remove generated data and rendered site output"

# all: render

download: data/ctd_datasetid_cruisename_stationname_mapping.csv
	Rscript scripts/download_cruises.R

process:
	Rscript scripts/clean_bin_ctd.R

report-process:
	quarto render reports/processing_summary.qmd
#
# interpolate: process
# 	julia --project=$(JULIA_PROJECT) scripts/interpolate_cruise.jl $(MAPPING_CSV)
#
# render: interpolate
# 	quarto render
#
# publish: render
# 	quarto publish

clean:
	rm -rf $(RAW_DIR) data/02_clean $(PROCESSED_DIR) $(INTERP_DIR) _site .quarto
	find . -path '*/batched_reports/*' -delete 2>/dev/null || true
