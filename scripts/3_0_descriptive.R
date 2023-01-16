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

# This is the list of ISO3 codes for the countries of West Africa
wa_countries <- c("BEN", "BFA", "CIV", "CPV", "ESH", "GHA",
                  "GIN", "GMB", "GNB", "LBR", "MLI", "MRT",
                  "NER", "NGA", "SEN", "SLE", "TGO")

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

save_plot(plot = study_timings, filename = here("figures", "Supplementary_Figure_1.pdf"), base_height = 8, base_width = 10)
save_plot(plot = study_timings, filename = here("figures", "Supplementary_Figure_1.png"), base_height = 8, base_width = 10)
write_rds(study_timings, here("plots", "study_timings.rds"))

# Median year of study starts
median(study_start$year_start)
summary(study_start$year_start)

# Length of studies
summary(study_start$year_end - study_start$year_start)

# Number of countries
rodent_data %>%
  distinct(unique_id, iso3c) %>%
  filter(iso3c %in% wa_countries) %>%
  tabyl(iso3c)

# Number of trapping sites based on timing, habitat and coordinates
rodent_data %>%
  distinct(unique_id, year_trapping, month_trapping, town_village, habitat, longitude, latitude)

# Number of trap nights by classification
rodent_data %>%
  group_by(trap_night_data) %>%
  distinct(unique_id, year_trapping, month_trapping, town_village, habitat, trap_nights, trap_night_unit) %>%
  summarise(total_trap_nights = sum(trap_nights))

# Number of trap sites by study
rodent_data %>%
  dplyr::select(unique_id, country, region, town_village, all_of(habitat_split)) %>%
  distinct() %>%
  group_by(unique_id) %>%
  summarise(n = n()) %$%
  table(n)

# Number of small rodents
rodent_data %>%
  summarise(n_small_mammals = sum(number))

# Number of each species
genus_data <- read_rds(here("data_clean", "genus_hierarchy.rds"))
species_data <- read_rds(here("data_clean", "species_data.rds"))
species <- read_rds(here("data_clean", "species_gbif.rds"))

sum(species_data$number)

count_species <- species_data %>%
  filter(iso3c %in% wa_countries) %>%
  group_by(species_gbif) %>%
  drop_na(species_gbif) %>%
  summarise(number = sum(number)) %>%
  mutate(percent = round(number/sum(.$number)*100, 2)) %>%
  arrange(-percent) %>%
  left_join(., species %>%
              drop_na(gbif_id) %>%
              filter(!str_detect(classification, "/")),
            by = c("species_gbif" = "gbif_id")) %>%
  rename(`GBIF ID` = "species_gbif",
         "Classification" = "classification",
         "Number of individuals" = "number",
         "Percent (%)" = "percent") %>%
  filter(`Number of individuals` > 0) %>%
  distinct(`GBIF ID`, `Number of individuals`, .keep_all = T) %>%
  mutate(Classification = snakecase::to_sentence_case(Classification)) # the number of individuals trapped identified to species level

total_species_level <- sum(count_species$`Number of individuals`)

# Number identified to genus level
total_genus_level <- sum(species_data$number[is.na(species_data$species_gbif) & !str_detect(species_data$classification, "rodent*")])

# How many species were identified within each order
species_identification <- count_species %>%
  mutate(`GBIF ID` = as.character(`GBIF ID`)) %>%
  left_join(.,
            species_data %>%
              rename(`GBIF ID` = "gbif_id") %>%
              dplyr::select(`GBIF ID`, genus),
            by = "GBIF ID") %>%
  distinct() %>%
  mutate(genus = snakecase::to_sentence_case(genus)) %>%
  left_join(., genus_data, by = "genus")

species_identification %>%
  group_by(order, family) %>%
  count() %>%
  arrange(order, -n) %>%
  drop_na()

table(species_identification$order)

# Number of studies investigating pathogens
length(unique(long_pathogen$unique_id))

# Number of unique pathogens tested for
length(unique(long_pathogen$pathogen_tested))

# Number of pathogens tested for within a study
long_pathogen %>%
  distinct(unique_id, pathogen_tested) %>%
  group_by(unique_id) %>%
  summarise(n_pathogens = n()) %>%
  tabyl(n_pathogens)

# Number of studies by pathogen
long_pathogen %>%
  distinct(pathogen_tested, unique_id) %>%
  group_by(pathogen_tested) %>%
  summarise(n_studies = n()) %>%
  arrange(-n_studies)

# Number of studies by method
pathogen_tested <- c("path_1", "path_2", "path_3", "path_4", "path_5", "path_6")
pcr_test <- c("pcr_path_1_positive", "pcr_path_2_positive", "pcr_path_3_positive", "pcr_path_4_positive", "pcr_path_5_positive", "pcr_path_6_positive")
ab_ag_test <- c("ab_ag_path_1_positive", "ab_ag_path_2_positive", "ab_ag_path_3_positive", "ab_ag_path_4_positive", "ab_ag_path_5_positive")
culture_test <- c("culture_path_1_positive", "culture_path_1_positive", "culture_path_1_positive")
direct_visualisation <- c("histo_path_1_positive", "histo_path_2_positive", "histo_path_3_positive", "histo_path_4_positive", "histo_path_5_positive", "histo_path_6_positive")

# PCR
pathogen %>%
  distinct(unique_id, across(all_of(pathogen_tested)), across(all_of(pcr_test))) %>%
  drop_na(pcr_path_1_positive) %>%
  distinct(unique_id) %>% # studies using PCR
  nrow(.)

# serology
pathogen %>%
  distinct(unique_id, across(all_of(pathogen_tested)), across(all_of(ab_ag_test))) %>%
  drop_na(ab_ag_path_1_positive) %>%
  distinct(unique_id) %>%
  nrow(.)

bind_rows(pathogen %>%
  distinct(unique_id, across(all_of(pathogen_tested)), across(all_of(culture_test))) %>%
  drop_na(culture_path_1_positive) %>%
  distinct(unique_id),
  pathogen %>%
  distinct(unique_id, across(all_of(pathogen_tested)), across(all_of(direct_visualisation))) %>%
  drop_na(histo_path_1_positive) %>%
  distinct(unique_id)) %>%
  nrow(.)

# Number individuals tested
long_pathogen %>%
  filter(str_detect(assay, regex("path_1_tested"))) %>%
  summarise(individuals_tested = sum(number))


# Host-pathogen pairs assessed
long_pathogen %>%
  filter(str_detect(assay, regex("tested"))) %>%
  group_by(classification, pathogen_tested) %>%
  summarise(n_pathogen_tested = sum(number)) %T>%
  assign(x = "tested", value = ., pos = 1) %>%
  summarise(n_pathogen_tested = n()) %T>%
  ungroup() %>%
  arrange(-n_pathogen_tested) %>%
  summarise(n_host_pathogen = sum(n_pathogen_tested))

# The number of distinct pathogens tested for by rodent species
long_pathogen %>%
  filter(str_detect(assay, regex("tested"))) %>%
  group_by(classification, pathogen_tested) %>%
  summarise(n_pathogen_tested = sum(number)) %T>%
  assign(x = "tested", value = ., pos = 1) %>%
  summarise(n_pathogen_tested = n()) %T>%
  ungroup() %>%
  arrange(-n_pathogen_tested)

# Map of study sites ------------------------------------------------------
all_countries <- c("BEN", "BFA", "CIV", "CMR", "CPV", "DZA", "ESH", "GHA",
                   "GIN", "GMB", "GNB", "LBR", "MAR", "MLI", "MRT", "NER",
                   "NGA", "SEN", "SLE", "TCD", "TGO")
continental_countries <- c("BEN", "BFA", "CIV", "ESH", "GHA",
                           "GIN", "GMB", "GNB", "LBR", "MLI", "MRT",
                           "NER", "NGA", "SEN", "SLE", "TGO")
no_data_countries <- c("GMB", "TGO")

level_0 <- read_rds(here("data_download", "admin_spatial", "level_0_admin.rds"))

level_1 <- read_rds(here("data_download", "admin_spatial", "level_1_admin.rds"))

level_2 <- read_rds(here("data_download", "admin_spatial", "level_2_admin.rds"))

non_trapped <- read_rds(here("data_download", "admin_spatial", "level_2_TGOGMB.rds"))

included_countries <- level_0 %>%
  filter(GID_0 %in% continental_countries)

contiguous_boundary <- included_countries %>%
  filter(!GID_0 == "CPV") %>%
  st_union()

rodent_spatial <- rodent_data %>%
  drop_na(longitude, latitude) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = crs(contiguous_boundary))

summary(trap_site_mapping$trap_nights)

# Figure 1 ----------------------------------------------------------------

trap_site_mapping <- rodent_spatial[st_within(rodent_spatial, included_countries) %>% lengths > 0,]  %>%
  select(unique_id, year_trapping, month_trapping, region, town_village, habitat, geometry) %>%
  distinct() %>%
  left_join(., imputed_tn) %>%
  mutate(trap_nights_cat = cut(trap_nights, c(0, 100, 300, 500, 1000, 2000, 5000, 60000)))

fig_1a_updated <- trap_site_mapping %>%
  ggplot() +
  geom_sf(data = level_0 %>%
            filter(GID_0 %in% wa_countries) %>%
            filter(GID_0 != "CPV"), alpha = 0) +
  geom_sf_text(data = level_0 %>%
                  filter(GID_0 %in% wa_countries) %>%
                  filter(GID_0 != "CPV"), aes(label = NAME_0), alpha = 0.5) +
  geom_sf(aes(colour = trap_nights_cat)) +
  scale_colour_viridis_d(direction = -1) +
  labs(colour = "Trap nights",
       x = element_blank(),
       y = element_blank()) +
  theme_minimal() +
  annotation_north_arrow(height = unit(2, "cm"),
                         style = north_arrow_minimal(text_size = 10)) +
  annotation_scale(height = unit(0.1, "cm"),
                   location = "tr") +
  guides(colour = guide_coloursteps(show.limits = TRUE, ticks = TRUE))

fig_1b_updated <- trap_site_mapping %>%
  drop_na(trap_nights_cat) %>%
  ggplot() +
  geom_bar(aes(x = trap_nights_cat, fill = trap_nights_cat)) +
  scale_fill_viridis_d(direction = -1) +
  scale_x_discrete(labels = c("0-100", "101-300", "301-500", "501-1,000", "1,000-2,000", "2,001-5000", "5,001-50,320")) +
  theme_minimal() +
  labs(x = "Trap nights",
       y = "Sites (n)") +
  guides(fill = "none")


save_plot(plot_grid(plotlist = list(fig_1a_updated, fig_1b_updated),
                    ncol = 1, rel_heights = c(1, 0.2), labels = c("A", "B")),
          filename = here("figures", "Figure_1_updated.pdf"), base_height = 9, base_width = 10)
save_plot(plot_grid(plotlist = list(fig_1a_updated, fig_1b_updated),
                    ncol = 1, rel_heights = c(1, 0.2), labels = c("A", "B")),
          filename = here("figures", "Figure_1_updated.png"), base_height = 9, base_width = 10)

