
# Supplementary Table 1 ---------------------------------------------------

studies <- read_rds(here("data_raw", "studies.rds"))

study_vars <- names(studies)[1:22]
study_desc <- c("link to manuscript",
                "year of publication",
                "title of manuscript",
                "journal",
                "stated aim of study",
                "stated aim of study",
                "stated aim of study",
                "first author of the study",
                "DOI/ISSN/ISBN of the publication",
                "unique ID for the current study",
                "measurement of species presence abundance/presence",
                "type of rodent traps used",
                "construction of the sampling grid",
                "whether there were multiple study visits to the same sites",
                "the level of geolocation reported",
                "the level of speciation of trapped rodents",
                "aim of the study dichotomised to Ecology or Zoonotic risk",
                "categorisation of study aims",
                "whether a species accumulation curve to describe trapping effort is reported",
                "whether there is a measure of rodent species diversity reported",
                "whether pathogens are assayed",
                "completeness of reported trapping effort")

rodent_data <- read_rds(here("data_raw", "rodent_data.rds"))

rodent_data_vars <- names(rodent_data)
rodent_data_desc <- c("unique ID for the current study",
                      "year rodent trapping occurred (range)",
                      "months trapping occurred (range)",
                      "country trapping occurred within",
                      "region trapping occurred within",
                      "name of towns or villages trapping occurred within",
                      "latitude of trapping site in degrees minutes seconds (North)",
                      "longitude of trapping site in degrees minutes seconds (West)",
                      "latitude of trapping site in decimal degrees (North)",
                      "longitude of trapping site in decimal degrees (East)",
                      "location of trapping site in UTM coordinates",
                      "habitat type of trapping site",
                      "the intensity of human disturbance in the trapping site",
                      "reported genus of trapped rodent/small mammal species",
                      "reported species of trapped rodent/small mammal species",
                      "number of trapped individuals",
                      "number of trap nights reported",
                      "rate of capture if reported",
                      "the unit of trap night measurement",
                      "the number of study nights completed at the trap site")

pathogen_data <- read_rds(here("data_raw", "pathogen.rds"))
pathogen_data_vars <- tibble(names = names(pathogen_data)) %>%
  mutate(names = case_when(str_detect(names, "tested") ~ "path_x_tested",
                           str_detect(names, "^pcr") ~ "pcr_x_positive",
                           str_detect(names, "^ab_ag") ~ "ab_ag_x_positive",
                           str_detect(names, "^culture") ~ "culture_x_positive",
                           str_detect(names, "^histo") ~ "histo_x_positive",
                           str_detect(names, "^path") ~ "pathogen_x",
                           TRUE ~ names)) %>%
  distinct() %>%
  pull()
pathogen_data_desc <- c("unique ID for the current study",
                        "year rodent trapping occurred (range)",
                        "months trapping occurred (range)",
                        "country trapping occurred within",
                        "region trapping occurred within",
                        "name of towns or villages trapping occurred within",
                        "habitat type of trapping site",
                        "reported genus of trapped rodent/small mammal species",
                        "reported species of trapped rodent/small mammal species",
                        "pathogens tested for, 1-7 possible columns",
                        "latitude of trapping site in degrees minutes seconds (North)",
                        "longitude of trapping site in degrees minutes seconds (West)",
                        "latitude of trapping site in decimal degrees (North)",
                        "longitude of trapping site in decimal degrees (East)",
                        "location of trapping site in UTM coordinates",
                        "number of individuals assayed for the corresponding pathogen, 1-7 possible columns",
                        "number of individuals PCR positive for the corresponding pathogen, 1-7 possible columns",
                        "number of individuals with positive serological assays for the corresponding pathogen, 1-7 possible columns",
                        "number of individuals culture positive for the corresponding pathogen, 1-7 possible columns",
                        "number of individuals histologically/histopathologically positive for the corresponding pathogen, 1-7 possible columns")

tibble(
  `Extraction tool` = c(rep("Study data", length(study_vars)), rep("Rodent data", length(rodent_data_vars)), rep("Pathogen data", length(pathogen_data_vars))),
  Variable = c(study_vars, rodent_data_vars, pathogen_data_vars),
  Description = c(study_desc, rodent_data_desc, pathogen_data_desc)
) %>%
  as_grouped_data("Extraction tool") %>%
  flextable() %>%
  autofit() %>%
  set_caption(caption = "Supplementary Table 1: Data extraction tool and variable description") %>%
  write_rds(here("tables", "supplementary_table_1.rds"))
