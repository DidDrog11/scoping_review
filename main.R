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

# Descriptive analysis which mirrors the presentation of the manuscript, mainly this is presented in the methods, data sources section.

source(here("scripts", "3_0_descriptive.R"))

# Statistical analysis for question 1.
# What is the extent of spatial bias in rodent trapping data?

source(here("scripts", "3_1_spatial_bias_analysis.R"))

# Analysis for question 2.
# What is the difference in rodent host distributions between curated datasets and rodent trapping studies?

source(here("scripts", "3_2_rodent_host_distributions.R"))

# Analysis for question 3.
# Are rodent trapping derived host-pathogen associations present in a consolidated zoonoses dataset?

source(here("scripts", "3_3_host_pathogen_associations.R"))

# Analysis for question 4.
# What is the spatial extent of pathogen testing within a host’s range?

source(here("scripts", "3_4_host_pathogen_distributions.R"))

# Supplementary material
source(here("scripts", "4_0_supplementary.R"))
