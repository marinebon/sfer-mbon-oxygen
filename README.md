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
| 1 | `make download` | Run [`scripts/download_cruises.R`](scripts/download_cruises.R) to resolve live GCOOS ERDDAP dataset IDs from `cruise_id` + `station`, then download into `data/01_raw/` |
| 2 | `make process` | Run [`scripts/clean_bin_ctd.R`](scripts/clean_bin_ctd.R) to QC, clean, and depth-bin CTD observations |
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
├── ctd_datasetid_cruisename_stationname_mapping.csv  # station-to-cruise mapping (tracked in git)
├── 01_raw/                  # downloaded raw CTD files
│   ├── {cruise_id}/         # one folder per cruise
│   │   └── *.csv            # one file per station
│   └── download_log.csv     # per-station download status (generated)
├── processed/{cruise_id}/     # cleaned, depth-binned CTD (R output)
└── interpolated/{cruise_id}/  # gridded oxygen fields (DIVAnd.jl output)
```


### CTD mapping file

[`data/ctd_datasetid_cruisename_stationname_mapping.csv`](data/ctd_datasetid_cruisename_stationname_mapping.csv) maps each CTD dataset to a cruise and station:

| Column | Description |
|--------|-------------|
| `dataset_id` | Legacy ERDDAP identifier from when the mapping was built (not used directly for download) |
| `cruise_id_og` | Original cruise name from source data |
| `cruise_id` | Normalized cruise identifier; used as the batch report key |
| `date` | Cast date |
| `lon`, `lat` | Station coordinates |
| `station_og` | Original station name from source data |
| `station` | Normalized station identifier |

[`example_batch/getListOfValues.R`](example_batch/getListOfValues.R) reads unique `cruise_id` values from this file. [`example_batch/getData.R`](example_batch/getData.R) loads processed and interpolated outputs for each report.

### Downloading from ERDDAP

GCOOS ERDDAP dataset IDs change over time (for example `WS0603_WS0603_WS0603_058` is now `SFER_CTD_WS0603_58`). Rather than maintaining a static list of ERDDAP IDs, [`scripts/download_cruises.R`](scripts/download_cruises.R) uses [`R/erddap_ctd_resolve.R`](R/erddap_ctd_resolve.R) to:

1. Search ERDDAP once per `cruise_id` for datasets matching `SFER_CTD_{cruise_id}_*`
2. Resolve the live dataset ID from `cruise_id` + `station` (handling `.` vs `_` in station names)
3. Save files using the aligned filename convention from the mapping spreadsheet
4. Skip existing files, log missing/failed stations to `data/01_raw/download_log.csv`, and continue without aborting the full run


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

