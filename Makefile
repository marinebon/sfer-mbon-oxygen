# SFER MBON Oxygen — data-to-report pipeline
#
# Prerequisites: R, Julia (1.6+), Quarto CLI
#   R packages: here, dplyr (installed automatically on first run)
#   Python (bathymetry): boto3, geopandas, rasterio (for scripts/download_bluetopo.py)

SHELL := /bin/bash
.DEFAULT_GOAL := help

# MAPPING_CSV := data/ctd_datasetid_cruisename_stationname_mapping.csv
RAW_DIR := data/01_raw
PROCESSED_DIR := data/processed
INTERP_DIR := data/interpolated
JULIA_PROJECT := julia
CRUISE ?= SV18067

.PHONY: help download process report-process interpolate interpolate-all render publish clean

help:
	@echo "SFER MBON Oxygen pipeline"
	@echo ""
	@echo "Targets:"
	@echo "  make download        Download raw CTD data for each cruise in data/ctd_datasetid_cruisename_stationname_mapping.csv"
	@echo "  make process         Clean raw CTD casts with oce into data/02_clean/ (skips up-to-date outputs)"
	@echo "  make report-process  Render reports/processing_summary.qmd (run after make process)"
	@echo "  make interpolate     Build DIVAnd oxygen fields for one cruise (CRUISE=$(CRUISE))"
	@echo "  make interpolate-all Build DIVAnd oxygen fields for every cruise with cleaned CTD data"
	@echo "  make render          Render the Quarto website locally"
	@echo "  make publish         Run full pipeline (download → process → interpolate), then quarto publish"
	@echo "  make clean           Remove generated data and rendered site output"

# all: render

download: data/ctd_datasetid_cruisename_stationname_mapping.csv
	Rscript scripts/download_cruises.R

process: download
	Rscript scripts/clean_bin_ctd.R

report-process:
	quarto render reports/processing_summary.qmd

interpolate: process
	julia --project=$(JULIA_PROJECT) scripts/interpolate_cruise.jl $(CRUISE)

interpolate-all: process
	julia --project=$(JULIA_PROJECT) scripts/interpolate_cruise.jl

render: interpolate-all
	quarto render

publish: interpolate-all
	quarto publish

clean:
	rm -rf $(RAW_DIR) data/02_clean $(PROCESSED_DIR) $(INTERP_DIR) _site .quarto
	find . -path '*/batched_reports/*' -delete 2>/dev/null || true
