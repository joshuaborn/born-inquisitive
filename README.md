This repository contains scripts and documents pertaining to analysis of reproduction, including phenomena such as use of contraception, unintended pregnancies, abortions, contraceptive failure, alloparenting, and sexual behavior.

## Data Sources

Currently, the analysis uses two data sources:
* the [National Survey of Family Growth (NSFG)](https://www.cdc.gov/nchs/nsfg/index.htm)
* the [National Vital Statistics System (NVSS)](https://www.cdc.gov/nchs/nvss/index.htm)


### National Survey of Family Growth

The strategy used to ingest NSFG data is to first load them using the provided [SAS program statements](https://www.cdc.gov/nchs/nsfg/nsfg_2017_2019_puf.htm#program) into a SAS programming environment, then to export data from the SAS programming environment via comma-separated values (CSV) files that are loaded in an R programming environment. The export from SAS includes labels and formats, the latter of which can be used to generate R factors as needed.


### National Vital Statistics System

All that is currently used from the NVSS are counts of live births. Currently, NVSS data on births is ingested by exporting counts via the [CDC WONDER](https://wonder.cdc.gov/) system into tab-separated values (TSV) files that are loaded in an R programming environment. Natality data is only available via CDC WONDER for 1995 and after.

For natality data before 1995, the [public use birth data files](https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm) can be used, but these files contain row per birth and thus are quite large.


## License

All work contained in this repository was created by Joshua Born and is licensed under the [Creative Commons Attribution 4.0 International (CC BY 4.0)](https://creativecommons.org/licenses/by/4.0/) license. Anyone is free to share and adapt the material contained here as long as appropriate credit is given, a link to the license is provided, and any changes made are indicated. See `LICENSE.txt` in this repository.


## Directory Structure

### SAS/

The `SAS/` subdirectory contains scripts written in the SAS statistical programming language. The `FORMAT` steps in the `DATA` steps of the program statements are commented out in order to make data exports to R compact and consistent.

### R/

The `R/` subdirectory contains scripts written in the R statistical programming language.

### data/

The `data/` subdirectory is the expected location of data files.

### eda/

The `eda/` subdirectory contains R Markdown files that summarize exploratory analysis. These are not formal reports as much as they are real-time records of analysis as the analysis is done.

### reports/

The `reports/` subdirectory contains R Markdown files that are intended for some form of publication.
