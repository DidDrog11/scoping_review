# This is the main script to replicate the analysis in the manuscript

# Load libraries and read in functions

source(here::here("scripts", "libraries.R"))

# The following scripts ensure local data is up to date

## create download folders
dir.create("data_download")
dir.create(here("data_download", "admin_spatial"))

## spatial data is obtained from GADM and is saved in the data_download/admin_spatial folder

source(here("scripts", "1_download_spatial.R"))

source(here("scripts", "1_update_data.R"))

# The following scripts clean and reshape the data

source(here("scripts", "2_1_cleaning.R"))
source(here("scripts", "2_2_clean_species.R"))
source(here("scripts", "2_3_clean_habitat.R"))
source(here("scripts", "2_4_clean_pathogen.R"))

# Some studies do not report trap night, for these studies we impute data based on trap success

source(here("scripts", "2_5_imputing_trapnights.R"))

# Descriptive analysis which mirrors the presentation of the manuscript

source(here("scripts", "3_1_descriptive.R"))

# Statistical analysis which mirrors the presentation of the manuscript

source(here("scripts", "3_2_statistical_analysis.R"))

# Finally figures are produced
