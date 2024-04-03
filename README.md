# Check RDBES files

Script to check all the data stored in SIRENO database which should be uploaded to RBDES database is properly exported.

## Files
The main files are:

- `report_RDBES_SIRENO_h1.R`: for check the H1 hierarchy.

- `report_RDBES_SIRENO_h5.R`: for check the H5 hierarchy.

Some functions and files used in these scripts are taken from [MI_RDBES_ExchangeFiles repository by David Currie](https://github.com/davidcurrie2001/MI_RDBES_ExchangeFiles), with minor changes.

## Requirements
This scripts requires the [sapmuebase library](https://github.com/Eucrow/sapmuebase). Other libraries are required but all are available in CRAN.

The files required to run the scripts are detailed in the `report_RDBES_SIRENO_h1.R` and `report_RDBES_SIRENO_h5.R` files.

## How to use & more info
See `report_RDBES_SIRENO_h1.R` and `report_RDBES_SIRENO_h5.R` files.