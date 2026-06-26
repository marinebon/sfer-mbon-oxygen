# quartobatch
Batch generate quarto reports from a common template across a dataset.

# Usage
## Website Deployment
It is recommended to deploy your quarto website to GitHub pages using the quarto CLI:

```bash
quarto publish
```

## To Create a New Batch:
1. Use create_batch R function:
    ```R
    source("create_batch.R")
    create_batch("testBatchName", "testExampleValue")
    ```
2. in the new {batch_name} folder, modify getData & getListOfValues to work with your data.
3. modify the {batch_name}/template.yml

----------------------------------------------------------------------------

# Attribution
This project is powered by the [quartobatch template](https://github.com/7yl4r/quartobatch).

----------------------------------------------------------------------------

# Additional Notes
The quartobatch template is a generalized implementation inspired by the following projects:

* [FCRWQDC_data_ingest](https://github.com/USF-IMARS/FCRWQDC_data_ingest) : Applies a common template across water quality analytes & data providers from multiple data file sources
* [seus-mbon-cruise-ctd-processing](https://github.com/USF-IMARS/seus-mbon-cruise-ctd-processing) : Applies a common template across CTD casts and research cruises.
