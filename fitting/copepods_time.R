# Title: Copepod Biomass Time Series
# Purpose: This script generates a time series of large and small copepod biomass to be
#           used for fitting the 1980-85 Gulf of Maine Rpath model to data.
#         
# Data files: (1) data/ZooData_GOM.csv, (2) data/EcoMon_Copepod size.csv from Harvey Walsh
#            (1) Ecomon data filtered for region, taxa of interest
#            (2) Parameters for converting abundance to biomass for taxa of interest
#
# Author: M.T. Grezlik
# Contact details: mgrezlik@umassd.edu
# following the example of S. Weisberg
# https://github.com/SarahJWeisberg/GOM-Rpath/blob/main/fitting/copepods_time.R


# https://github.com/NOAA-EDAB/benthosindex/blob/main/data/survdat_nobio.rds