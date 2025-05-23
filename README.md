# GBRpath

![gitleaks](https://github.com/NOAA-EDAB/GBRpath/workflows/gitleaks/badge.svg)

**Rpath model of Georges Bank**

---

## Contacts

| [MGrezlik](https://github.com/MGrezlik) | [slucey](https://github.com/slucey) |
|:--------------------------------------:|:----------------------------------:|
| <img src="https://github.com/MGrezlik.png" width="80"/> | <img src="https://github.com/slucey.png" width="80"/> |

---

## Order in Which to Run Scripts

### 1. Gather Input Data

- Navigate to: `GBRpath/data-raw/R`  
- Run the following scripts in order:

  ```r
  GBRpath_biomass_pull.R
  GBRpath_biomass_accumulation.R
  GBRpath_bioparams.R
  GBRpath_catch_pull.R
  generic_scallop_clam_survey_pull.R
  GBRpath_diet_pull.R
  ```
*Note that catch data requires a login to the NOAA server. Resulting data pulls are not confidential and are found in the data folder*
  
### 2. Create Initial Model

- Navigate to: `GBRpath/R`
- Run the following scripts in order:

  ```r
  copepod_reorganization.R
  GB_initial_model.R
  ```
  
### 3. Recreate Balancing Process and Pedigree Creation

- Navigate to: `GBRpath/R`
- Run the following scripts in order:

  ```r
  GB_balancing.R
  GBRpath_data_pedigree.R
  ```
  
### Legal Disclaimer
*This repository is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration, or the United States Department of Commerce. All NOAA GitHub project code is provided on an ‘as is’ basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this GitHub project will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a DOC bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by DOC or the United States Government.*