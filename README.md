# GBRpath

![gitleaks](https://github.com/NOAA-EDAB/GBRpath/workflows/gitleaks/badge.svg)


Rpath model of Georges Bank

## Contact

| [slucey](https://github.com/slucey)        
| ----------------------------------------------------------------------------------------------- 
| [![](https://avatars.githubusercontent.com/u/5578254?s=100&u=cd59cd654cab73ea583c697145bfe062222355cd&v=4)](https://github.com/slucey) | 

## Order in which to run scripts

# To gather input data
# 1. Go to GBRpath>data-raw>R
# 2. Run the following scripts in order:
#   - `GBRpath_biomass_pull.R`
#   - `GBRpath_biomass_accumulation.R`
#   - `GBRpath_bioparams.R`
#   - `GBRpath_catch_pull.R`
#   - `GBRpath_diet_pull.R`
# All other scripts in the data-raw folder are out of date.
# Some rely on data pulls which were replaced by hard data copies when Sean left NOAA.

# To create initial model
# 1. Go to GBRpath>R
# 2. Run the following scripts in order:
#   - `copepod_reorganization.R`
#   - `GBRpath_initial_model.R`
#   - `GBRpath_data_pedigree.R`

# To recreate balancing process
# 1. Go to GBRpath>R
# 2. Run the following scripts in order:
#   - `GBRpath_alternate_balance.R`

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
