source(here::here("scripts", "libraries.R"))

drive_download("https://docs.google.com/spreadsheets/d/1rQYjHhk6uk1PoKZZVsgFlmuqWGicU2tTisk9ddfAwTM/edit#gid=0", path = here("data_download", "included_studies.xlsx"), overwrite = T)

studies <- read_xlsx(here("data_download", "included_studies.xlsx"), sheet = "study")
write_rds(studies, here("data_raw", "studies.rds"))

rodent_data <- read_xlsx(here("data_download", "included_studies.xlsx"), sheet = "trapping")
write_rds(rodent_data, here("data_raw", "rodent_data.rds"))

pathogen_data <- read_xlsx(here("data_download", "included_studies.xlsx"), sheet = "pathogen")
write_rds(pathogen_data, here("data_raw", "pathogen.rds"))
