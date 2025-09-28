# Data_Management_Example

This repository contains **representative code samples** from household survey data management workflows developed in R. The code demonstrates skills in data cleaning and preparation, applying weights and creating survey objects, as well as indicator calculations such as SDG indicators. 
> **Note:** The data required to run these scripts cannot be shared due to confidentiality. The examples are provided to illustrate coding practices, documentation, and workflow design. Sensitive references have been removed while retaining the technical structure.  

## Workflow overview 

The scripts are organised sequentially:

1. **Workflow preparation**
The first two files of the script are preparatory files which enable the onward steps. 

- **1_params.R**
Defines Kobo asset names, and key survey variables. 

- **2_funs.R**
Utility functions for Kobo data ingestion and preparation. Includes retrieval of assets by name, flattening/merging of repeat groups, harmonising household roster structures, adding key variables for interview outcomes, and dynamically merging multi-asset datasets. Provides a single data_import() entry point that loads, processes, and caches Kobo survey data for downstream cleaning and QA.


2. **Data import and preparation** 

- **1_Data_import.Rmd**

  Handles raw data import from KoboToolbox.  
  - Requires `.Renviron` file with Kobo server URL and API token.  
  - Loads all required R packages and sources `params.R` and `funs.R` for configuration and utility functions.  
  - Imports multiple Kobo assets via `data_import()`, stores them in a cached environment, and saves as `.rds` (`df_list_ZAM_raw_0.rds`).  
  - Note: Raw datasets are never pushed to Git/GitHub to maintain confidentiality. 

- **2_Merge_kobo_assests.Rmd** 

  Flattens repeat groups for each Kobo project, propagating key indexes to child tables. Merges three projects (33 tables each) into one dataset. Saves outputs as flattened (`df_ZAM_raw_flat_1.rds`) and merged (`df_ZAM_raw_flat_merged_2.rds`).

- **3_Repeat_groups_HH_flat.Rmd**

  Cleans and reshapes repeat groups (household- and individual-level) into wide format for integration with main survey data. Removes redundant parent vars, renames columns systematically, and saves cleaned outputs (`df_ZAM_clean_repeat_HH_3.rds`).
  
- **4_Split_to_5_datasets.Rmd** 
 
  Splits main survey data into 4 respondent datasets (HoH, RA_adult, RA_woman, RA_caregiver) and integrates repeat groups into each. Builds a 5th dataset (`HHroster`) by merging household roster with member info, identification, education, language, and disability modules. Produces final list (`ZAM_5_datasets_5.rds`) plus unmatched rows and migrant repeat group.
 
- **6_Mobile member.Rmd** 
  
  Creates a 6th dataset which will be analyzed separately from the other 5. 
  
- **7_Final_6_datasets.Rmd** 

  Produces the 6 final datasets (HHroster, main, RA_adult, RA_woman, RA_caregiver, mobilemember optional) ready for cleaning. Filters roster to include only household members. Saves consolidated list

3. **Data Cleaning**

- **8_cleaning.Rmd**

  Applies cleaning and corrections to the 6 datasets. Merges manual and dashboard correction logs (with/without `_index`), handles anthropometry corrections, drops duplicates and incomplete cases, fixes sample IDs, and applies geo-checks (e.g., Mayukwayukwa buffer). Ensures consistency across datasets and assigns final stratum variable. Saves outputs

4. **Final data preparation**

- **9_Data_preparation_final_ZAM.R**

  Prepares additional disaggregation and derived variables. Adds household size, age categories, disability (WG-SS), country of origin, and crowding index. Recodes population groups (merging asylum into refugees) and settlement types (urban/rural). Constructs education completion (primary, lower, upper secondary), birth/marriage/refugee/ID certificates, and tenure/land rights indicators. Merges sampling weights, attaches labels, and restructures datasets. Creates national- and strata-level `srvyr::as_survey_design` objects for HH, roster, RA_adult, RA_woman, and RA_caregiver. Saves final `.rds` objects for downstream analysis.
 
5. **Indicator calculations** 

- **10_FDS_ZAM_SDG_Indicators.Rmd** & **11_FDS_ZAM_RBM_Indicators.Rmd**
  Calculates SDG indicators as well as Results Based Monitoring indicators. Saved the datasets with indicators for annonymization. 
  
- **WI_ZAM.R** 
  Calculates wealth index. 
  
  
## Script overview  

| Step | Script | Purpose |
|------|--------|---------|
| 0 | `1_params.R` | Defines Kobo asset names and key survey variables |
| 0 | `2_funs.R` | Utility functions for Kobo ingestion/flattening, roster harmonisation, merging multi-asset data |
| 1 | `1_Data_import.Rmd` | Imports Kobo raw data (via API token); caches and saves as `.rds` |
| 2 | `2_Merge_kobo_assests.Rmd` | Flattens repeat groups, propagates keys, merges 3 projects into one dataset |
| 3 | `3_Repeat_groups_HH_flat.Rmd` | Cleans/reshapes repeat groups into wide format for integration with main survey data |
| 4 | `4_Split_to_5_datasets.Rmd` | Splits into 4 respondent datasets + household roster; integrates roster modules |
| 5 | `6_Mobile member.Rmd` | Builds separate dataset for mobile members |
| 6 | `7_Final_6_datasets.Rmd` | Produces final 6 datasets (HHroster, main, RA_adult, RA_woman, RA_caregiver, mobilemember) |
| 7 | `8_cleaning.Rmd` | Applies cleaning, corrections, deduplication, anthropometry fixes, geo-checks |
| 8 | `9_Data_preparation_final_ZAM.R` | Creates derived/disaggregation variables, merges weights, attaches labels, builds survey objects |
| 9 | `10_FDS_ZAM_SDG_Indicators.Rmd` | Calculates SDG indicators |
| 9 | `11_FDS_ZAM_RBM_Indicators.Rmd` | Calculates Results Based Monitoring indicators |
| 9 | `WI_ZAM.R` | Calculates wealth index |

  
  