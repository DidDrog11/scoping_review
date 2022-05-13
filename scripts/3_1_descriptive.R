source(here::here("scripts", "libraries.R"))

# Data --------------------------------------------------------------------
# Load study details
studies <- read_rds(here("data_clean", "studies.rds")) %>%
  mutate(year_publication = as.numeric(year_publication))

# Load in the cleaned habitat types of each study
habitat_split <- c("habitat_1", "habitat_2", "habitat_3", "habitat_4", "habitat_5", "habitat_6", "habitat_7")
habitat_data <- read_rds(here("data_clean", "habitat_types.rds")) %>%
  tibble() %>%
  dplyr::select(record_id, all_of(habitat_split), intensity_use)

# Load the number of trap nights for studies not presenting this data
# These values have been previously imputed
imputed_tn <- read_rds(here("data_clean", "imputed_trap_nights.rds"))

# Load the cleaned rodent data, each line represents a species within a trap site within a study
# Join this data to the cleaned habitats and imputed trap night data
rodent_data <- read_rds(here("data_clean", "rodent_df.rds")) %>%
  tibble()  %>%
  dplyr::select(-all_of(c("intensity_use", "geometry", "trap_nights"))) %>%
  left_join(.,
            habitat_data ,
            by = "record_id") %>%
  left_join(., imputed_tn,
            by = c("unique_id", "year_trapping", "month_trapping",
                   "region", "town_village", "habitat")) %>%
  mutate(unique_id = as_factor(unique_id))

# Load in the cleaned pathogen data in both wide and long formats
pathogen <- read_rds(here("data_clean", "pathogen.rds"))
wide_pathogen <- read_rds(here("data_clean", "wide_pathogen.rds"))
long_pathogen <- read_rds(here("data_clean", "long_pathogen.rds"))

# Load in the cleaned species and gbif associations
# This includes missnamed species and those with uncertainty
species_gbif <- read_rds(here("data_clean", "species_data.rds")) %>%
  distinct(genus, species, gbif_id, genus_gbif, species_gbif)


# Summary table of included studies ---------------------------------------
# This is not presented in the manuscript but is included here for interest
studies %>%
  mutate(decade_publication = case_when(year_publication < 1980 ~ "1970's",
                                        year_publication < 1990 ~ "1980's",
                                        year_publication < 2000 ~ "1990's",
                                        year_publication < 2010 ~ "2000's",
                                        year_publication < 2020 ~ "2010's",
                                        year_publication < 2030 ~ "2020's",
                                        TRUE ~ "other"),
         geolocation_level = case_when(geolocation_level == "island" ~ "Study site",
                                       geolocation_level == "none" ~ "No geolocation",
                                       geolocation_level == "study" ~ "Study",
                                       geolocation_level == "study_region" ~ "Study",
                                       geolocation_level == "study_site" ~ "Study site",
                                       geolocation_level == "trap_line" ~ "Specific trap site",
                                       geolocation_level == "trap_site" ~ "Specific trap site",
                                       geolocation_level == "trap_site, study_site" ~ "Specific trap site",
                                       TRUE ~ "other"),
         speciation = case_when(speciation == "molecular" ~ "Molecular",
                                speciation == "morphological" ~ "Morphological",
                                speciation == "morphological, molecular" ~ "Morphological and molecular",
                                speciation == "not_stated" ~ "Not stated",
                                TRUE ~ "other"),
         pathogen = case_when(pathogen == "Yes, second paper" ~ "Yes",
                              TRUE ~ pathogen),
         country = sub('(^[^_]+_[^_]+)_(.*)$', '\\2', unique_id),
         country = case_when(country == "multiple" ~ "Multiple",
                             TRUE ~ countryname(sub("_2", "", country), destination = "un.name.en"))) %>%
  dplyr::select(country, decade_publication, repeated_visit, geolocation_level,
                speciation, species_accumulation, diversity_measurement, trapping_effort, pathogen) %>%
  tbl_summary(label = c(country ~ "Country",
                        decade_publication ~ "Publication year",
                        repeated_visit ~ "Repeat study visits",
                        geolocation_level ~ "Level of geolocation",
                        speciation ~ "Method of speciation",
                        species_accumulation ~ "Use of a species accumulation curve",
                        diversity_measurement ~ "Use of a measure of species diversity",
                        trapping_effort ~ "Reporting of trapping effort",
                        pathogen ~ "Reporting of potential pathogens")) %>%
  as_gt() %>%
  gt::tab_header(title = "Table 1: Characteristics of included studies") %>%
  tab_options(table.font.size = px(12),
              data_row.padding = px(2)) %>%
  saveRDS(file = here("tables", "study_table.rds"))


# Supplementary figure 1. Study timings -----------------------------------
# This produces a figure showing the start and end date of each study and when it was published

study_start <- rodent_data %>%
  distinct(unique_id, year_trapping) %>%
  separate(year_trapping, into = c("year_start", "year_end"), sep = "-") %>%
  group_by(unique_id) %>%
  mutate(year_start = as.numeric(year_start),
         year_end = case_when(is.na(year_end) ~ max(year_start),
                              TRUE ~ as.numeric(year_end))) %>%
  summarise(year_start = min(year_start, na.rm = T),
            year_end = max(year_end, na.rm = T)) %>%
  mutate(year_publication = as.numeric(substring(unique_id, 4, 7)),
         year_end = case_when(is.infinite(year_end) & !is.na(year_start) ~ year_start,
                              TRUE ~ year_end)) %>%
  mutate(reported = factor(case_when(is.infinite(year_start) ~ "No",
                                     TRUE ~ "Yes"), levels = c("No", "Yes")),
         year_start = case_when(is.infinite(year_start) ~ as.numeric(substring(unique_id, 4, 7)),
                                TRUE ~ year_start),
         year_end = case_when(is.infinite(year_end) ~ as.numeric(substring(unique_id, 4, 7)),
                              TRUE ~ year_end),
         publishing_delay = year_publication - year_end) %>%
  arrange(year_start) %>%
  mutate(unique_id = as_factor(fct_inorder(unique_id)))

study_timings <- ggplot(study_start) +
  geom_segment(aes(x = unique_id, xend = unique_id, y = year_start, yend = year_end), colour = "grey") +
  geom_point(aes(x = unique_id, y = year_start, alpha = reported), size = 3, colour = "#008b46") +
  geom_point(aes(x = unique_id, y = year_end, alpha = reported), size = 3, colour = "#00468b") +
  geom_point(aes(x = unique_id, y = year_publication), size = 1, colour = "#8B0000") +
  coord_flip() +
  theme_minimal() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.ticks.x = element_line(colour = "black")) +
  scale_y_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020),
                     limits = c(1960, 2022)) +
  labs(y = "Year",
       x = element_blank(),
       alpha = "Study dates reported")

ggsave(plot = study_timings, filename = here("figures", "Fig_1_Panel_A.png"), dpi = 300, height = 8, width = 10)
write_rds(study_timings, here("plots", "study_timings.rds"))

# Median year of study starts

# Length of studies

# Number of countries

# Number of trap nights for actual/estimated and imputed

# Number of trap sites
rodent_data %>%
  dplyr::select(unique_id, country, region, town_village, all_of(habitat_split)) %>%
  distinct()

rodent_data %>%
  dplyr::select(unique_id, country, region, town_village, all_of(habitat_split)) %>%
  distinct() %>%
  group_by(unique_id) %>%
  summarise(n = n()) %$%
  table(n)

# Number of small rodents

# Number of each species

# Number of assays for pathogens

# Pathogen species assayed


# Map of study sites ------------------------------------------------------


