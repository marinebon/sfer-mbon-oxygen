:warning: NOTE: much of the below is not yet implemented!

# SFER MBON Oxygen

Batch-generate Quarto reports for SFER MBON research cruises, from raw CTD download through cleaned/binned data, DIVAnd interpolation, and website publication.

## Prerequisites

- [R](https://www.r-project.org/) (4.0+)
- [Julia](https://julialang.org/) (1.6+) with [DIVAnd.jl](https://github.com/gher-uliege/DIVAnd.jl)
- [Quarto CLI](https://quarto.org/docs/get-started/)

On first run, R and Julia dependencies are installed automatically from `scripts/` and `julia/Project.toml`.


## Pipeline overview

Cruises and stations are listed in [`data/ctd_datasetid_cruisename_stationname_mapping.csv`](data/ctd_datasetid_cruisename_stationname_mapping.csv). The Makefile runs these steps in order:

| Step | Target | What it does |
|------|--------|--------------|
| 1 | `make download` | Run [`scripts/download_cruises.R`](scripts/download_cruises.R) to fetch CTD datasets from GCOOS ERDDAP into `data/01_raw/{cruise_id}/` |
| 2 | `make process` | Run [`scripts/clean_bin_ctd.R`](scripts/clean_bin_ctd.R) to clean each cast with `oce::ctdTrim()` and `oce::ctdDecimate()` into `data/02_clean/` (skips casts whose output is already up to date) |
| 2b | `make report-process` | Render [`reports/processing_summary.qmd`](reports/processing_summary.qmd) summarizing raw vs cleaned row counts and example profiles |
| 3 | `make interpolate` | Run [`scripts/interpolate_cruise.jl`](scripts/interpolate_cruise.jl) to build gridded oxygen fields with [DIVAnd.jl](https://github.com/gher-uliege/DIVAnd.jl) for each cruise |
| 4 | `make render` | Render the Quarto website: expand [`example_batch/template.qmd`](example_batch/template.qmd) into per-cruise `.qmd` files and build HTML reports |
| 5 | `make publish` | Run the full pipeline above, then `quarto publish` to deploy the site (e.g. GitHub Pages) |

```bash
make publish
```

Individual steps can be run separately:

```bash
make download
make process
make report-process
make interpolate
make render
```

To remove generated data and site output:

```bash
make clean
```


### Data layout

```
data/
├── ctd_datasetid_cruisename_stationname_mapping.csv  # cruise inventory for reports (tracked in git)
├── 01_raw/                  # downloaded raw CTD files
│   ├── {cruise_id}/         # one folder per cruise
│   │   └── SFER_CTD_*.csv   # one file per ERDDAP dataset
│   └── download_log.csv     # per-dataset download status (generated)
├── 02_clean/                # oce-cleaned CTD files (one CSV per cast)
│   └── processing_log.csv   # per-cast processing status (generated)
reports/
└── processing_summary.qmd   # post-process QC report (render with make report-process)
├── processed/{cruise_id}/     # cleaned, depth-binned CTD (R output)
└── interpolated/{cruise_id}/  # gridded oxygen fields (DIVAnd.jl output)
```


### Cruise inventory

[`data/ctd_datasetid_cruisename_stationname_mapping.csv`](data/ctd_datasetid_cruisename_stationname_mapping.csv) lists cruises included in the pipeline. CTD dataset naming is handled upstream on GCOOS ERDDAP (`SFER_CTD_{cruise_id}_{station}`).

[`example_batch/getListOfValues.R`](example_batch/getListOfValues.R) reads unique `cruise_id` values from this file. [`example_batch/getData.R`](example_batch/getData.R) loads processed and interpolated outputs for each report.

### Downloading from ERDDAP

[`scripts/download_cruises.R`](scripts/download_cruises.R) searches ERDDAP for `SFER_CTD_{cruise_id}_*` datasets and saves each as `data/01_raw/{cruise_id}/{erddap_dataset_id}.csv`. Existing files are skipped; results are logged to `data/01_raw/download_log.csv`.


## Website deployment

After the data pipeline completes, publish the Quarto website:

```bash
quarto publish
```

Or run the full pipeline and publish in one command:

```bash
make publish
```


## Create a new batch

1. Use the `create_batch` R function:
   ```R
   source("create_batch.R")
   create_batch("testBatchName", "testExampleValue")
   ```
2. In the new `{batch_name}` folder, modify `getData.R` and `getListOfValues.R` to work with your data.
3. Modify the `{batch_name}/template.qmd` report template.


----------------------------------------------------------------------------

## Attribution

This project is powered by the [quartobatch template](https://github.com/7yl4r/quartobatch).

----------------------------------------------------------------------------

