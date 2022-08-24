source(here::here("scripts", "manuscript_library.R"))

# Supplementary 1
# Data extraction tool

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
  write_rds(here("tables", "Supplementary_Table_1.rds"))

# Supplementary 2
# Included studies
tbl_2 <- readRDS(here("data_clean", "studies.rds")) %>%
  filter(!is.na(reference_uid))

bib <- bib2df(here("citations", "include_final.bib")) %>%
  select(TITLE, JOURNALTITLE, SERIES, BIBTEXKEY, DOI, DOI.1, PMID, PMCID, ISSN, ISBN, URL) %>%
  mutate(reference = as.character(coalesce(DOI, PMID, ISSN, ISBN)))

matched_ref <- left_join(tbl_2, bib,
                         by = c("reference_uid" = "reference")) %>%
  select(year_publication, first_author, TITLE, journal_name) %>%
  mutate(year_publication = as_date(year_publication, format = "%Y")) %>%
  select(year_publication, first_author, TITLE, journal_name) %>%
  rename("Year publication" = year_publication,
         "Author" = first_author,
         "Title" = TITLE,
         "Journal/Publication" = journal_name) %>%
  arrange(`Year publication`, `Author`) %>%
  flextable() %>%
  autofit() %>%
  set_caption(caption = "Supplementary Table 2: Included studies") %>%
  colformat_date(j = 1, fmt_date = "%Y") %>%
  write_rds(here("tables", "Supplementary_Table_2.rds"))

# Supplementary 3
# Model tables
all_models <- read_rds(here("models", "all_models.rds"))

as_flextable(all_models$null_increased_k) %>%
  set_caption(caption = "Supplementary Table 3.1: GAM model Trap night density ~ Tweedie(Longitdue * Latitude)") %>%
  write_rds(here("tables", "supplementary_table_3_1.rds"))

as_flextable(all_models$pop_model_increased_k) %>%
  set_caption(caption = "Supplementary Table 3.2: GAM model Trap night density ~ Tweedie(Population density + (Longitdue * Latitude))") %>%
  write_rds(here("tables", "supplementary_table_3_2.rds"))

as_flextable(all_models$pop_area_model) %>%
  set_caption(caption = "Supplementary Table 3.3: GAM model Trap night density ~ Tweedie(Population density + Region area + (Longitdue * Latitude))") %>%
  write_rds(here("tables", "supplementary_table_3_3.rds"))

as_flextable(all_models$all_hab_model) %>%
  set_caption(caption = "Supplementary Table 3.4: GAM model Trap night density ~ Tweedie(Proportion cropland + Proportion shrubland + Proportion tree cover + Proportion urban + (Longitdue * Latitude))") %>%
  write_rds(here("tables", "supplementary_table_3_4.rds"))

as_flextable(all_models$combined_model_1) %>%
  set_caption(caption = "Supplementary Table 3.5: Final GAM model Trap night density ~ Tweedie(Population density + Region area + Proportion urban + (Longitdue * Latitude))") %>%
  write_rds(here("tables", "supplementary_table_3_5.rds"))


# Supplementary fig 1
# Study timelines
# This is produced in the descriptive script due to dependence on data processing
source(here("scripts", "3_0_descriptive.R"))

study_timings <- read_rds(here("plots", "study_timings.rds"))

# Supplementary fig 2
# Habitat bias

# Code for this plot needs to be updated

# Supplementary fig 4
# Group level H-P associations
pathogen_host <- read_rds(here("data_clean", "host_pathogen_family_all_plot_data.rds"))
clover <- read_rds(here("data_clean", "clover_cleaned.rds"))

supp_4_df <- pathogen_host %>%
  filter(!str_ends(classification, "sp.")) %>%
  select(classification, class, tested, positive, pathogen_family) %>%
  left_join(., clover$combined %>%
              distinct(classification, PathogenType, pathogen_family, source),
            by = c("classification", "pathogen_family")) %>%
  rowwise() %>%
  mutate(prop = positive/tested * 100) %>%
  select(classification, class, tested, positive, prop, pathogen_family, source) %>%
  pivot_longer(cols = c("prop")) %>%
  rowwise() %>%
  mutate(species = paste0(str_to_upper(str_sub(unlist(str_split(classification, " "))[1], 1, 1)), ". ", unlist(str_split(classification, " "))[2]),
         pathogen_family = str_to_sentence(pathogen_family),
         source = factor(source, labels = c("CLOVER")))

sum_species<- supp_4_df %>%
  group_by(species, class) %>%
  summarise(n_tested = sum(tested)) %>%
  arrange(n_tested) %>%
  ungroup() %>%
  mutate(species = fct_inorder(species)) %>%
  pull(species)

supp_4_df$species <- fct_relevel(supp_3_df$species, levels(sum_species))

plot_supp_4_acute <- supp_4_df %>%
  filter(tested > 5) %>%
  filter(class == "Acute infection") %>%
  mutate(lab = paste0(round(value, 1), "% \n N = ", tested)) %>%
  ggplot() +
  geom_tile(aes(x = pathogen_family, y = species, fill = value, colour = source, width = 0.9, height = 0.9), lwd = 1) +
  scale_fill_viridis_c(option = "viridis", direction = -1, begin = 0.3, end = 1) +
  scale_colour_manual(na.translate = FALSE, values = "black") +
  labs(title = "Acute infection",
       fill = "Infection (%)",
       x = element_blank(),
       y = element_blank(),
       colour = element_blank()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

plot_supp_4_serology <- supp_4_df %>%
  filter(tested > 5) %>%
  filter(class == "Serology") %>%
  mutate(lab = paste0(round(value, 1), "% N = ", tested)) %>%
  ggplot() +
  geom_tile(aes(x = pathogen_family, y = species, fill = value, colour = source, width = 0.9, height = 0.9), lwd = 1) +
  scale_fill_viridis_c(option = "viridis", direction = -1, begin = 0.3, end = 1) +
  scale_colour_manual(na.translate = FALSE, values = "black") +
  labs(title = "Serology",
       fill = "Infection (%)",
       x = element_blank(),
       y = element_blank(),
       colour = element_blank()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

save_plot(plot_grid(plot_supp_4_acute, labels = "A"), filename = here("figures", "Supp_4a_updated.png"), base_width = 12, base_height = 10)
save_plot(plot_grid(plot_supp_4_serology, labels = "B"), filename = here("figures", "Supp_4b_updated.png"), base_width = 12, base_height = 10)


