# "NSFG" Repository

This repository contains scripts and documents pertaining to analysis of the [National Survey of Family Growth (NSFG)](https://www.cdc.gov/nchs/nsfg/index.htm), mostly pertaining to contraception and related matters.

Currently, these scripts have been tested with the 2017-2019 and the 2015-2017 data sets.


## Directory Structure

### src/

The `src/` subdirectory contains source code, predominantly in the R statistical programming language.

### data/

The `data/` subdirectory is the expected location of data files. The NSFG distributes its data in ASCII-encoded text files suffixed with ".dat". These ".dat" suffixed files are expected to be in this directory by the `load_data.R` script. They are not checked into this repository, but can downloaded directly from the NSFG.

The ".dat" suffixed files use a fixed-width format that does not have a delimiter to specify where one value ends and another begins. The NSFG distributes scripts in the SAS, SPSS, and Stata programming languages to load these ".dat" suffixed files. However, the NSFG does not distribute such scripts in the R programming language, hence the creation of the `load_data.R` script. The files in the `data/` subdirectory suffixed with ".vars.txt" are necessary for the `load_data.R` script to load the ".dat" files. The content of the ".vars.txt" suffixed files are code fragments taken from the SAS scripts distributed by the NSFG to load the data. The code fragments consist of variable names and lists of offsets that define where the variable values are specified in the ".dat" suffixed files.

### eda/

The `eda/` subdirectory contains R Markdown files that summarize exploratory analysis. These are not formal reports as much as they are real-time records of analysis as the analysis is done.
