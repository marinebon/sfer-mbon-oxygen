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

.PHONY: help download download-cruise process process-cruise report-process interpolate interpolate-cruise interpolate-all render render-cruise example-cruise publish clean

help:
	@echo "SFER MBON Oxygen pipeline"
	@echo ""
	@echo "Targets:"
	@echo "  make download        Download SFER CTD datasets from GCOOS ERDDAP"
	@echo "  make process         Clean raw CTD casts with oce into data/02_clean/ (skips up-to-date outputs)"
	@echo "  make report-process  Render reports/processing_summary.qmd (run after make process)"
	@echo "  make interpolate     Build DIVAnd oxygen fields for one cruise (CRUISE=$(CRUISE))"
	@echo "  make interpolate-all Build DIVAnd oxygen fields for every cruise with cleaned CTD data"
	@echo "  make render          Render the Quarto website locally"
	@echo "  make example-cruise  Run full pipeline for one cruise and render (CRUISE=$(CRUISE))"
	@echo "  make publish         Run full pipeline (download → process → interpolate), then quarto publish"
	@echo "  make clean           Remove generated data and rendered site output"

# all: render

download:
	Rscript scripts/download_cruises.R

download-cruise:
	Rscript scripts/download_cruises.R $(CRUISE)

process: download
	Rscript scripts/clean_bin_ctd.R

process-cruise: download-cruise
	Rscript scripts/clean_bin_ctd.R $(CRUISE)

report-process:
	quarto render reports/processing_summary.qmd

interpolate: process
	julia --project=$(JULIA_PROJECT) scripts/interpolate_cruise.jl $(CRUISE)

interpolate-cruise: process-cruise
	julia --project=$(JULIA_PROJECT) scripts/interpolate_cruise.jl $(CRUISE)

interpolate-all: process
	julia --project=$(JULIA_PROJECT) scripts/interpolate_cruise.jl

render: interpolate-all
	quarto render

render-cruise: interpolate-cruise
	CRUISE=$(CRUISE) quarto render

example-cruise: render-cruise
	@echo "Example cruise $(CRUISE) pipeline complete."

publish: interpolate-all
	quarto publish gh-pages --no-prompt

clean:
	rm -rf $(RAW_DIR) data/02_clean $(PROCESSED_DIR) $(INTERP_DIR) _site .quarto
	find . -path '*/batched_reports/*' -delete 2>/dev/null || true
