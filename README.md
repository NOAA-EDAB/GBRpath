# GBRpath

![gitleaks](https://github.com/NOAA-EDAB/GBRpath/workflows/gitleaks/badge.svg)


Rpath model of Georges Bank

## Contact

| [MGrezlik](https://github.com/MGrezlik)        
| ----------------------------------------------------------------------------------------------- 

## Order in which to run scripts

To gather input data
1. Go to GBRpath>data-raw>R
2. Run the following scripts in order:
  - `GBRpath_biomass_pull.R`
  - `GBRpath_biomass_accumulation.R`
  - `GBRpath_bioparams.R`
  - `GBRpath_catch_pull.R`
  - 'generic_scallop_clam_survey_pull.R'
  - `GBRpath_diet_pull.R`

*Note that catch data requires a login to the NOAA server. Resulting data pulls are not confidential and are found in the data folder*

To create initial model
1. Go to GBRpath>R
2. Run the following scripts in order:
  - `copepod_reorganization.R`
  - `GB_initial_model.R`


To recreate balancing process and pedigree creation
1. Go to GBRpath>R
2. Run the following scripts in order:
  - `GB_balancing.R`
  - `GBRpath_data_pedigree.R`

#### Legal disclaimer

*This repository is a scientific product and is not official
communication of the National Oceanic and Atmospheric Administration, or
the United States Department of Commerce. All NOAA GitHub project code
is provided on an ‘as is’ basis and the user assumes responsibility for
its use. Any claims against the Department of Commerce or Department of
Commerce bureaus stemming from the use of this GitHub project will be
governed by all applicable Federal law. Any reference to specific
commercial products, processes, or services by service mark, trademark,
manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce.
The Department of Commerce seal and logo, or the seal and logo of a DOC
bureau, shall not be used in any manner to imply endorsement of any
commercial product or activity by DOC or the United States Government.*
