source(here::here("scripts", "libraries.r"))


# Data --------------------------------------------------------------------
studies <- read_rds(here("data_clean", "studies.rds"))
rodent_data <- read_rds(here("data_clean", "rodent_df.rds")) %>%
  tibble()  %>%
  dplyr::select(-all_of(c("habitat", "intensity_use", "geometry")))

habitat_split <- c("habitat_1", "habitat_2", "habitat_3", "habitat_4", "habitat_5", "habitat_6", "habitat_7")
habitat_data <- read_rds(here("data_clean", "habitat_types.rds")) %>%
  tibble() %>%
  dplyr::select(record_id, all_of(habitat_split), intensity_use)

rodent_data %<>%
  left_join(.,
            habitat_data ,
            by = "record_id")

pathogen <- read_rds(here("data_clean", "pathogen.rds"))
wide_pathogen <- read_rds(here("data_clean", "wide_pathogen.rds"))
species_gbif <- read_rds(here("data_clean", "species_data.rds")) %>%
  distinct(genus, species, gbif_id, genus_gbif, species_gbif)

long_pathogen <- read_rds(here("data_clean", "long_pathogen.rds"))

# Publication year --------------------------------------------------------
ggplot(studies) +
  geom_bar(aes(x = year_publication)) +
  theme_minimal() +
  labs(x = "Publication year",
       y = "Number of publications",
       title = "Studies reporting rodent trapping in West African countries",
       caption = paste("N =", length(unique(studies$unique_id)), sep = " "))

# Study aim ---------------------------------------------------------------
table(studies$aim)
ecology_studies <- studies %>% filter(aim == "Ecology")
zoonoses_studies <- studies %>% filter(aim == "Zoonoses risk")

aim_detail_eco <- studies %>% filter(aim == "Ecology") %>% dplyr::select(unique_id, aim, aim_detail_1, aim_detail_2)
aim_detail_zoo <- studies %>% filter(aim == "Zoonoses risk") %>%  dplyr::select(unique_id, aim, aim_detail_1, aim_detail_2)

# Study location ----------------------------------------------------------
countries <- studies %>%
  full_join(., rodent_data %>%
              distinct(unique_id, country),
            by = "unique_id") %>%
  distinct(unique_id, country)

countries$iso3 <- countrycode(as.character(countries$country), "country.name", "iso3c")

countries %>%
  group_by(country, iso3) %>%
  summarise(n = n()) %>%
  drop_na(country) %>%
  ggplot(aes(x = reorder(country, n), y = n)) +
  geom_col() +
  #geom_flag(aes(image = iso3, y = -2)) +
  coord_flip() +
  theme_minimal() +
  labs(
    x = "Number of studies",
    y = "Country",
    title = "Location of trapping activities for included studies",
    caption = paste("N =", length(unique(studies$unique_id)), sep = " ")
  )

#ggsave(plot = last_plot(),filename = here("figures", "studies_country.png"), device = "png")

countries %>%
  filter(!country %in% c("Cameroon", "Chad", "Morocco")) %>%
  group_by(unique_id) %>%
  summarise(n = n()) %$%
  table(n) # calculate the number of countries trapped in by each study

# number of trap sites
rodent_data %>%
  dplyr::select(unique_id, country, region, town_village, all_of(habitat_split)) %>%
  distinct()

rodent_data %>%
  dplyr::select(unique_id, country, region, town_village, all_of(habitat_split)) %>%
  distinct() %>%
  group_by(unique_id) %>%
  summarise(n = n()) %$%
  table(n)

# Trap type and setup -----------------------------------------------------
trap_type <- studies %>%
  separate(col = trap_types, into = c("trap_1", "trap_2", "trap_3", "trap_4"), sep = ", ", remove = T) %>%
  pivot_longer(cols = c("trap_1", "trap_2", "trap_3", "trap_4"), values_to = "trap_type") %>%
  drop_na(trap_type)

trap_type %>% filter(!trap_type %in% c("hand", "not_stated")) %>% group_by(unique_id) %>% summarise(n = n()) %>% count(n)

trap_technique <- studies %>%
  separate(col = trapping_method, into = c("method_1", "method_2", "method_3"), sep = ", ", remove = T) %>%
  pivot_longer(cols = c("method_1", "method_2", "method_3"), values_to = "trap_method") %>%
  drop_na(trap_method)

trap_technique %>% filter(aim == "Zoonoses risk") %$%
  table(trap_method)

# Trapping effort ---------------------------------------------------------

table(studies$trapping_effort)
table(ecology_studies$trapping_effort)
table(zoonoses_studies$trapping_effort)

t_effort <- studies %>% filter(trapping_effort == "Yes") %>% distinct(unique_id)

s_effort <- rodent_data %>% filter(unique_id %in% t_effort$unique_id) %>%
  group_by(unique_id, year_trapping, month_trapping, region, town_village, habitat_1) %>%
  summarise(trap_nights = unique(trap_nights)) %>%
  group_by(unique_id) %>%
  summarise(trap_nights = sum(trap_nights))

summary(s_effort$trap_nights) # summary for studies with complete reporting

t_effort <- rodent_data %>% filter(unique_id %in% t_effort$unique_id) %>%
  group_by(unique_id, year_trapping, month_trapping, region, town_village, habitat_1) %>%
  summarise(trap_nights = unique(trap_nights))

summary(t_effort$trap_nights) # trap nights for studies with complete recording

inc_effort <- studies %>% filter(trapping_effort == "Incomplete")

inc_effort <- rodent_data %>% filter(unique_id %in% inc_effort$unique_id & !is.na(trap_nights)) %>%
  group_by(unique_id, year_trapping, month_trapping, region, town_village, habitat_1) %>%
  summarise(trap_nights = unique(trap_nights)) %>%
  group_by(unique_id) %>%
  summarise(trap_nights = unique(trap_nights)) %>%
  summarise(trap_nights = sum(trap_nights))

summary(inc_effort$trap_nights) # trap nights for studies with incomplete recording

# Habitat classification --------------------------------------------------
habitat_types <- rodent_data %>%
  dplyr::select(unique_id, country, region, town_village, all_of(habitat_split), trap_night_unit, trap_nights) %>%
  pivot_longer(cols = all_of(habitat_split), values_to = "habitat_type") %>%
  drop_na(habitat_type)

habitat_types <- habitat_types %>%
  left_join(., studies %>%
              dplyr::select(unique_id, aim),
            by = "unique_id")

habitat_freq <-  habitat_types %>%
  count(habitat_type) %>%
  arrange(-n) # count of number of habitats trapped

zoo_habitat_freq <- habitat_types %>%
  filter(aim == "Zoonoses risk") %>%
  count(habitat_type) %>%
  arrange(-n)

eco_habitat_freq <- habitat_types %>%
  filter(aim == "Ecology") %>%
  count(habitat_type) %>%
  arrange(-n)

# Species identification --------------------------------------------------

table(studies$speciation)

studies %>%
  filter(year_publication >= 2010) %>%
  count(speciation)

# Rodents -----------------------------------------------------------------
wa_countries <- c("BEN", "BFA", "CIV", "CPV", "ESH", "GHA",
                  "GIN", "GMB", "GNB", "LBR", "MLI", "MRT",
                  "NER", "NGA", "SEN", "SLE", "TGO")

genus_data <- read_rds(here("data_clean", "genus_hierarchy.rds"))
species_data <- read_rds(here("data_clean", "species_data.rds"))

count_species <- species_data %>%
  filter(iso3c %in% wa_countries) %>%
  group_by(species_gbif, classification) %>%
  drop_na(species_gbif) %>%
  summarise(number = sum(number)) %>%
  mutate(percent = round(number/sum(.$number)*100, 2)) %>%
  arrange(-percent) %>%
  rename(`GBIF ID` = "species_gbif",
         "Classification" = "classification",
         "Number of individuals" = "number",
         "Percent (%)" = "percent") %>%
  mutate(Classification = snakecase::to_sentence_case(Classification)) # the number of individuals trapped identified to species level

speciation <- count_species %>%
  mutate(`GBIF ID` = as.character(`GBIF ID`)) %>%
  left_join(.,
            species_data %>%
              rename(`GBIF ID` = "gbif_id") %>%
              dplyr::select(`GBIF ID`, genus),
            by = "GBIF ID") %>%
  distinct() %>%
  mutate(genus = snakecase::to_sentence_case(genus)) %>%
  left_join(., genus_data, by = "genus")

speciation %>%
  group_by(order, family) %>%
  count() %>%
  arrange(order, -n)

write_rds(count_species, here("tables", "sup_table3.rds"))

species_data %>%
  left_join(., studies %>%
              dplyr::select(unique_id, aim),
            by = "unique_id") %>%
  filter(iso3c %in% wa_countries) %>%
  group_by(aim) %>%
  summarise(n = sum(number)) # number of rodents trapped

species_data %>%
  left_join(., studies %>%
                         dplyr::select(unique_id, aim),
                       by = "unique_id") %>%
  filter(iso3c %in% wa_countries & !is.na(species_gbif)) %>%
  group_by(aim) %>%
  summarise(n = sum(number)) # number of rodents identified to species level

count_genus <- species_data %>%
  group_by(genus_gbif, genus) %>%
  filter(genus != "rodentia") %>%
  summarise(number = sum(number)) %>%
  mutate(percent = round(number/sum(.$number)*100, 2)) %>%
  arrange(-percent)

# Trap success ------------------------------------------------------------

single_site <- species_data %>%
  filter(unique_id %in% c(t_effort$unique_id, inc_effort$unique_id)) %>%
  drop_na(trap_nights) %>%
  distinct(unique_id, year_trapping, month_trapping, town_village, habitat, .keep_all = T) %>%
  filter(trap_night_unit %in% c("habitat", "study_site", "trap_site", "study_habitat", NA))

single_site_n <- single_site %>%
  summarise(trap_nights = sum(trap_nights))

single_site_c <- species_data %>%
  filter(unique_id %in% single_site$unique_id) %>%
  summarise(captures = sum(number))

single_site_c/single_site_n*100

study_site <- species_data %>%
  filter(unique_id %in% c(t_effort$unique_id, inc_effort$unique_id)) %>%
  drop_na(trap_nights) %>%
  distinct(unique_id, year_trapping, month_trapping, town_village, .keep_all = T) %>%
  filter(trap_night_unit %in% c("village", "site", "visit", "trap_session"))

study_site_n <- study_site %>%
  summarise(trap_nights = sum(trap_nights))

study_site_c <- species_data %>%
  filter(unique_id %in% study_site$unique_id) %>%
  summarise(captures = sum(number))

study_site_c/study_site_n*100

study <- species_data %>%
  filter(unique_id %in% c(t_effort$unique_id, inc_effort$unique_id)) %>%
  drop_na(trap_nights) %>%
  distinct(unique_id, .keep_all = T) %>%
  filter(trap_night_unit %in% c("study"))

study_n <- study %>%
  summarise(trap_nights = sum(trap_nights))

study_c <- species_data %>%
  filter(unique_id %in% study$unique_id) %>%
  summarise(captures = sum(number))

study_c/study_n*100

bind_rows(single_site, study_site, study) %>%
  distinct(unique_id)
trap_nights <- sum(single_site_n, study_site_n, study_n)
captures <- sum(single_site_c, study_site_c, study_c)
captures/trap_nights*100


# Rodent biodiversity -----------------------------------------------------

table(studies$diversity_measurement)
studies %>%
  filter(diversity_measurement == "Yes") %$%
  table(species_accumulation)

table(studies$species_accumulation)

# Pathogen testing ----------------------------------------------------------------

pathogen_tested <- c("path_1", "path_2", "path_3", "path_4", "path_5", "path_6")
pcr_test <- c("pcr_path_1_positive", "pcr_path_2_positive", "pcr_path_3_positive", "pcr_path_4_positive", "pcr_path_5_positive", "pcr_path_6_positive")
ab_ag_test <- c("ab_ag_path_1_positive", "ab_ag_path_2_positive", "ab_ag_path_3_positive", "ab_ag_path_4_positive", "ab_ag_path_5_positive")
culture_test <- c("culture_path_1_positive", "culture_path_1_positive", "culture_path_1_positive")
direct_visualisation <- c("histo_path_1_positive", "histo_path_2_positive", "histo_path_3_positive", "histo_path_4_positive", "histo_path_5_positive", "histo_path_6_positive")

group_pathogens <- as.list(c("borrelia_species", "amr_bacteria", "amr_bacteria", "arenaviridae_species", "leishmania_species", "mammarenavirus_species", "schistosoma_species"))
names(group_pathogens) <- c("borrelia_crocidurae", "e_coli_esbl", "k_pneumoniae_esbl", "lassa_mammarenavirus", "leishmania_major", "mammarenavirus_species",
                             "schistosoma_mansoni")

pathogen_data <- tibble(pathogen) %>%
  dplyr::select(-geometry) %>% # speed up processing by removing sf structure
  left_join(., studies %>%
              dplyr::select(unique_id, pathogen),
            by = "unique_id")

table(studies$pathogen)

pathogen_data %>%
  filter(pathogen == "Rodent pathogen") %>%
  distinct(unique_id, across(all_of(pathogen_tested))) # rodent pathogens

pcr <- pathogen_data %>%
  filter(pathogen %in% c("Yes", "Yes, second paper")) %>%
  distinct(unique_id, across(all_of(pathogen_tested)), across(all_of(pcr_test))) %>%
  drop_na(pcr_path_1_positive)

pcr %>%
  distinct(unique_id) # studies using PCR

pcr %>%
  pivot_longer(cols = all_of(pathogen_tested), values_to = "pathogen") %>%
  drop_na(pathogen) %>%
  distinct(pathogen) # pathogens tested for using PCR

ab_ag_test <- pathogen_data %>%
  filter(pathogen %in% c("Yes", "Yes, second paper")) %>%
  distinct(unique_id, across(all_of(pathogen_tested)), across(all_of(ab_ag_test))) %>%
  drop_na(ab_ag_path_1_positive)

ab_ag_test %>%
  distinct(unique_id)

ab_ag_test %>%
  pivot_longer(cols = all_of(pathogen_tested), values_to = "pathogen") %>%
  drop_na(pathogen) %>%
  distinct(pathogen) # pathogens tested for using ab/ag

culture <- pathogen_data %>%
  filter(pathogen %in% c("Yes", "Yes, second paper")) %>%
  distinct(unique_id, across(all_of(pathogen_tested)), across(all_of(culture_test))) %>%
  drop_na(culture_path_1_positive)

culture %>%
  distinct(unique_id)

culture %>%
  pivot_longer(cols = all_of(pathogen_tested), values_to = "pathogen") %>%
  drop_na(pathogen) %>%
  distinct(pathogen) # pathogens tested for using culture

histopath <- pathogen_data %>%
  filter(pathogen %in% c("Yes", "Yes, second paper")) %>%
  distinct(unique_id, across(all_of(pathogen_tested)), across(all_of(direct_visualisation))) %>%
  drop_na(histo_path_1_positive)

histopath %>%
  distinct(unique_id)

histopath %>%
  pivot_longer(cols = all_of(pathogen_tested), values_to = "pathogen") %>%
  drop_na(pathogen) %>%
  distinct(pathogen) # pathogens tested for using direct visualisation

pathogen_data %>%
  filter(pathogen %in% c("Yes", "Yes, second paper")) %>%
  distinct(unique_id, across(all_of(pathogen_tested))) %>%
  pivot_longer(cols = all_of(pathogen_tested), values_to = "pathogen") %>%
  drop_na() %$%
  table(name)

pathogen_data %>%
  filter(pathogen %in% c("Yes", "Yes, second paper")) %>%
  distinct(unique_id, across(all_of(pathogen_tested))) %>%
  pivot_longer(cols = all_of(pathogen_tested), values_to = "pathogen") %>%
  drop_na() %$%
  table(pathogen)

long_pathogen %>%
  filter(str_detect(assay, regex("path_1_tested"))) %>%
  summarise(individuals_tested = sum(number)) # Number individuals tested

long_pathogen %>%
  filter(str_detect(assay, regex("tested"))) %>%
  summarise(number_tested = sum(number)) # Number of tests performed

long_pathogen %>%
  filter(str_detect(assay, regex("tested"))) %>%
  group_by(classification, pathogen_tested) %>%
  summarise(n_pathogen_tested = sum(number)) %T>%
  assign(x = "tested", value = ., pos = 1) %>%
  summarise(n_pathogen_tested = n()) %T>%
  arrange(-n_pathogen_tested) # The number of distinct pathogens tested for by rodent species

long_pathogen %>%
  filter(str_detect(assay, regex("path_1_tested"))) %>%
  group_by(classification, pathogen_tested) %>%
  summarise(n_pathogen_tested = sum(number)) %>%
  summarise(n_tested = sum(n_pathogen_tested)) %>%
  arrange(-n_tested) # The number of individuals tested of each species

commonly_tested <- tested %>%
  filter(n_pathogen_tested > 100) %>%
  ungroup() %>%
  distinct(classification) %>%
  pull() # List of the most commonly tested rodent/shrew species

pathogen_groups <- c("Lassa mammarenavirus", "Borrelia species", "Other arenaviruses", "Toxoplasma gondii", "Schistosoma species", "Leptospirosis species",
                     "Other viruses", "Bartonella species", "Other viruses", "Other parasites", "Other parasites", "Other parasites", "Other parasites",
                     "Other parasites", "Other parasites", "Other bacteria", "Other bacteria", "Other bacteria", "Other bacteria", "Other bacteria",
                     "Other arenaviruses", "Other viruses", "Mycobacteria species", "Other parasites", "Other bacteria",
                     "Other viruses", "Other viruses", "Other viruses", "Other viruses", "Other bacteria", "Other bacteria", "Other parasites")
names(pathogen_groups) <- tested %>%
  arrange(-n_pathogen_tested) %>%
  filter(classification %in% commonly_tested) %>%
  ungroup() %>%
  distinct(pathogen_tested) %>%
  pull()

pal <- c("Arvicanthis niloticus" = "#bdbdbd",
         "Crocidura foxi" = "#e6550d",
         "Crocidura olivieri" = "#fdae6b",
         "Crocidura sp" = "#fee6ce",
         "Gerbilliscus gambiana" = "#bdbdbd",
         "Gerbillus tarabuli" = "#bdbdbd",
         "Lophuromys sikapusi" = "#bdbdbd",
         "Mastomys erythroleucus" = "#edf8e9",
         "Mastomys huberti" = "#bae4b3",
         "Mastomys natalensis" = "#74c476",
         "Mastomys sp" = "#238b45",
         "Mus mattheyi" = "#f2f0f7",
         "Mus minutoides" = "#cbc9e2",
         "Mus minutoides/mattheyi" = "#9e9ac8",
         "Mus musculus" = "#6a51a3",
         "Praomys daltoni" = "#fc9272",
         "Praomys rostratus" = "#de2d26",
         "Rattus norvegicus" = "#9ecae1",
         "Rattus rattus" = "#3182bd")

tested %>%
  arrange(-n_pathogen_tested) %>%
  filter(classification %in%  commonly_tested) %>%
  mutate(pathogen_tested = recode(pathogen_tested, !!!pathogen_groups),
         classification = snakecase::to_sentence_case(classification),
         classification = recode(classification,
                                 "Mus minutoides mattheyi" = "Mus minutoides/mattheyi"),
         genus = str_split(classification, " ", simplify = T)[1]) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(x = fct_rev(fct_inorder(pathogen_tested)), y = n_pathogen_tested,
           fill = classification)) +
  coord_flip() +
  facet_wrap(~ genus, scales = "free_x") +
  theme_minimal() +
  scale_fill_manual(values = pal) +
  labs(x = element_blank(),
       y = "Number of individuals tested",
       fill = "Species")

ggsave(filename = "rodent_testing.png", path = here("figures"), plot = last_plot(), device = "png", dpi = 300)

long_pathogen %>%
  filter(str_detect(assay, regex("positive"))) %>%
  group_by(classification, pathogen_tested) %>%
  summarise(n_pathogen_positive = sum(number)) %T>%
  assign(x = "positive", value = ., pos = 1) %>%
  summarise(n_pathogen_positive = sum(n_pathogen_positive)) %>%
  arrange(-n_pathogen_positive) # The number of distinct positive assays by rodent species

pcr_positive <- long_pathogen %>%
  filter(str_detect(assay, regex("positive")) & str_detect(assay, regex("pcr")))

pcr_tested <-  long_pathogen %>%
  filter(str_detect(assay, regex("tested")) & record_id %in% pcr_positive$record_id)

sum(pcr_tested$number) # The number of rodents investigated with PCR
pcr_tested %>%
  group_by(pathogen_tested) %>%
  summarise(n = sum(number)) %>%
  arrange(-n) %>%
  print(n = 24)

ab_ag_positive <- long_pathogen %>%
  filter(str_detect(assay, regex("positive")) & str_detect(assay, regex("ab_ag")))

ab_ag_tested <-  long_pathogen %>%
  filter(str_detect(assay, regex("tested")) & record_id %in% ab_ag_positive$record_id)

sum(ab_ag_tested$number)
ab_ag_tested %>%
  group_by(pathogen_tested) %>%
  summarise(n = sum(number)) %>%
  arrange(-n)

histo_path_positive <- long_pathogen %>%
  filter(str_detect(assay, regex("positive")) & str_detect(assay, regex("histo_path")))

histo_path_tested <-  long_pathogen %>%
  filter(str_detect(assay, regex("tested")) & record_id %in% histo_path_positive$record_id)

sum(histo_path_tested$number)
histo_path_tested %>%
  group_by(pathogen_tested) %>%
  summarise(n = sum(number)) %>%
  arrange(-n)

culture_positive <- long_pathogen %>%
  filter(str_detect(assay, regex("positive")) & str_detect(assay, regex("culture")))

culture_tested <-  long_pathogen %>%
  filter(str_detect(assay, regex("tested")) & record_id %in% culture_positive$record_id)

sum(culture_tested$number)
culture_tested %>%
  group_by(pathogen_tested) %>%
  summarise(n = sum(number)) %>%
  arrange(-n)

pcr_plot <- pcr_tested %>%
  group_by(classification, pathogen_tested) %>%
  summarise(n_pathogen_tested = sum(number))  %>%
  arrange(-n_pathogen_tested) %>%
  filter(classification %in%  commonly_tested) %>%
  mutate(pathogen_tested = recode(pathogen_tested, !!!pathogen_groups),
         classification = snakecase::to_sentence_case(classification),
         classification = recode(classification,
                                 "Mus minutoides mattheyi" = "Mus minutoides/mattheyi"),
         genus = str_split(classification, " ", simplify = T)[1]) %>%
  left_join(.,
            pcr_positive %>%
              group_by(classification, pathogen_tested) %>%
              summarise(n_pathogen_positive = sum(number))  %>%
              arrange(-n_pathogen_positive) %>%
              filter(classification %in%  commonly_tested) %>%
              mutate(pathogen_tested = recode(pathogen_tested, !!!pathogen_groups),
                     classification = snakecase::to_sentence_case(classification),
                     classification = recode(classification,
                                             "Mus minutoides mattheyi" = "Mus minutoides/mattheyi"),
                     genus = str_split(classification, " ", simplify = T)[1]),
            by = c("classification", "pathogen_tested", "genus")) %>%
  ungroup() %>%
  group_by(classification, pathogen_tested, genus) %>%
  summarise(n_tested = sum(n_pathogen_tested),
            n_positive = sum(n_pathogen_positive),
            prop_positive = round(n_positive/n_tested*100, 1),
            .groups = "keep") %>%
  mutate(prop_positive = ifelse(is.nan(prop_positive), 0, prop_positive)) %>%
  filter(!genus %in% c("Gerbilliscus", "Lophuromys", "Gerbillus")) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(x = fct_rev(fct_inorder(pathogen_tested)), y = prop_positive,
               fill = classification), position = position_dodge2(preserve = "single")) +
  coord_flip() +
  facet_wrap(~ genus) +
  theme_minimal() +
  scale_fill_manual(values = pal) +
  guides(fill=guide_legend(ncol=2)) +
  labs(x = element_blank(),
       y = "Percentage of individuals testing positive by PCR",
       fill = "Species")

sero_plot <- ab_ag_tested %>%
  group_by(classification, pathogen_tested) %>%
  summarise(n_pathogen_tested = sum(number))  %>%
  arrange(-n_pathogen_tested) %>%
  filter(classification %in%  commonly_tested) %>%
  mutate(pathogen_tested = recode(pathogen_tested, !!!pathogen_groups),
         classification = snakecase::to_sentence_case(classification),
         classification = recode(classification,
                                 "Mus minutoides mattheyi" = "Mus minutoides/mattheyi"),
         genus = str_split(classification, " ", simplify = T)[1]) %>%
  left_join(.,
            ab_ag_positive %>%
              group_by(classification, pathogen_tested) %>%
              summarise(n_pathogen_positive = sum(number))  %>%
              arrange(-n_pathogen_positive) %>%
              filter(classification %in%  commonly_tested) %>%
              mutate(pathogen_tested = recode(pathogen_tested, !!!pathogen_groups),
                     classification = snakecase::to_sentence_case(classification),
                     classification = recode(classification,
                                             "Mus minutoides mattheyi" = "Mus minutoides/mattheyi"),
                     genus = str_split(classification, " ", simplify = T)[1]),
            by = c("classification", "pathogen_tested", "genus")) %>%
  ungroup() %>%
  group_by(classification, pathogen_tested, genus) %>%
  summarise(n_tested = sum(n_pathogen_tested),
            n_positive = sum(n_pathogen_positive),
            prop_positive = round(n_positive/n_tested*100, 1),
            .groups = "keep") %>%
  mutate(prop_positive = ifelse(is.nan(prop_positive), 0, prop_positive)) %>%
  filter(!genus %in% c("Gerbilliscus", "Lophuromys", "Gerbillus")) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(x = fct_rev(fct_inorder(pathogen_tested)), y = prop_positive,
               fill = classification), position = position_dodge2(preserve = "single")) +
  coord_flip() +
  facet_wrap(~ genus) +
  theme_minimal() +
  scale_fill_manual(values = pal) +
  labs(x = element_blank(),
       y = "Percentage of individuals testing positive by serology",
       fill = "Species")

histo_plot <- histo_path_tested %>%
  group_by(classification, pathogen_tested) %>%
  summarise(n_pathogen_tested = sum(number))  %>%
  arrange(-n_pathogen_tested) %>%
  filter(classification %in%  commonly_tested) %>%
  mutate(pathogen_tested = recode(pathogen_tested, !!!pathogen_groups),
         classification = snakecase::to_sentence_case(classification),
         classification = recode(classification,
                                 "Mus minutoides mattheyi" = "Mus minutoides/mattheyi"),
         genus = str_split(classification, " ", simplify = T)[1]) %>%
  left_join(.,
            histo_path_positive %>%
              group_by(classification, pathogen_tested) %>%
              summarise(n_pathogen_positive = sum(number))  %>%
              arrange(-n_pathogen_positive) %>%
              filter(classification %in%  commonly_tested) %>%
              mutate(pathogen_tested = recode(pathogen_tested, !!!pathogen_groups),
                     classification = snakecase::to_sentence_case(classification),
                     classification = recode(classification,
                                             "Mus minutoides mattheyi" = "Mus minutoides/mattheyi"),
                     genus = str_split(classification, " ", simplify = T)[1]),
            by = c("classification", "pathogen_tested", "genus")) %>%
  ungroup() %>%
  group_by(classification, pathogen_tested, genus) %>%
  summarise(n_tested = sum(n_pathogen_tested),
            n_positive = sum(n_pathogen_positive),
            prop_positive = round(n_positive/n_tested*100, 1),
            .groups = "keep") %>%
  mutate(prop_positive = ifelse(is.nan(prop_positive), 0, prop_positive)) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(x = fct_rev(fct_inorder(pathogen_tested)), y = prop_positive,
               fill = classification), position = position_dodge2(preserve = "single")) +
  coord_flip() +
  facet_wrap(~ genus) +
  theme_minimal() +
  scale_fill_manual(values = pal) +
  labs(x = element_blank(),
       y = "Percentage of individuals positive by histology/direct visualisation",
       fill = "Species")

legend <- cowplot::get_legend(pcr_plot)

cowplot::plot_grid(pcr_plot +
                     theme(legend.position = "none"),
                   sero_plot +
                     theme(legend.position = "none"),
                   histo_plot +
                     theme(legend.position = "none"),
                   legend)
cowplot::save_plot(filename = "all_assays.png", plot = last_plot(), ncol = 2, nrow = 2, path = here("figures"))

# Infected rodents --------------------------------------------------------

# Lassa

pcr_positive %>%
  group_by(classification, pathogen_tested) %>%
  summarise(n_pathogen_positive = sum(number))  %>%
  arrange(-n_pathogen_positive) %>%
  mutate(pathogen_tested = recode(pathogen_tested, !!!pathogen_groups),
         classification = snakecase::to_sentence_case(classification),
         classification = recode(classification,
                                 "Mus minutoides mattheyi" = "Mus minutoides/mattheyi"),
         genus = str_split(classification, " ", simplify = T)[1]) %>%
  filter(pathogen_tested == "Lassa mammarenavirus")

ab_ag_positive %>%
  group_by(classification, pathogen_tested) %>%
  summarise(n_pathogen_positive = sum(number))  %>%
  arrange(-n_pathogen_positive) %>%
  mutate(pathogen_tested = recode(pathogen_tested, !!!pathogen_groups),
         classification = snakecase::to_sentence_case(classification),
         classification = recode(classification,
                                 "Mus minutoides mattheyi" = "Mus minutoides/mattheyi"),
         genus = str_split(classification, " ", simplify = T)[1]) %>%
  filter(pathogen_tested == "Lassa mammarenavirus")

histo_path_positive %>%
  group_by(classification, pathogen_tested) %>%
  summarise(n_pathogen_positive = sum(number))  %>%
  arrange(-n_pathogen_positive) %>%
  mutate(pathogen_tested = recode(pathogen_tested, !!!pathogen_groups),
         classification = snakecase::to_sentence_case(classification),
         classification = recode(classification,
                                 "Mus minutoides mattheyi" = "Mus minutoides/mattheyi"),
         genus = str_split(classification, " ", simplify = T)[1]) %>%
  filter(pathogen_tested == "Lassa mammarenavirus")

culture_positive %>%
  group_by(classification, pathogen_tested) %>%
  summarise(n_pathogen_positive = sum(number))  %>%
  arrange(-n_pathogen_positive) %>%
  mutate(pathogen_tested = recode(pathogen_tested, !!!pathogen_groups),
         classification = snakecase::to_sentence_case(classification),
         classification = recode(classification,
                                 "Mus minutoides mattheyi" = "Mus minutoides/mattheyi"),
         genus = str_split(classification, " ", simplify = T)[1]) %>%
  filter(pathogen_tested == "Lassa mammarenavirus")

# Bartonella

pcr_positive %>%
  group_by(classification, pathogen_tested) %>%
  summarise(n_pathogen_positive = sum(number))  %>%
  arrange(-n_pathogen_positive) %>%
  mutate(pathogen_tested = recode(pathogen_tested, !!!pathogen_groups),
         classification = snakecase::to_sentence_case(classification),
         classification = recode(classification,
                                 "Mus minutoides mattheyi" = "Mus minutoides/mattheyi"),
         genus = str_split(classification, " ", simplify = T)[1]) %>%
  filter(pathogen_tested == "Bartonella species")

# Borrelia

pcr_positive %>%
  group_by(classification, pathogen_tested) %>%
  summarise(n_pathogen_positive = sum(number))  %>%
  arrange(-n_pathogen_positive) %>%
  mutate(pathogen_tested = recode(pathogen_tested, !!!pathogen_groups),
         classification = snakecase::to_sentence_case(classification),
         classification = recode(classification,
                                 "Mus minutoides mattheyi" = "Mus minutoides/mattheyi"),
         genus = str_split(classification, " ", simplify = T)[1]) %>%
  filter(pathogen_tested == "Borrelia species")

ab_ag_positive %>%
  group_by(classification, pathogen_tested) %>%
  summarise(n_pathogen_positive = sum(number))  %>%
  arrange(-n_pathogen_positive) %>%
  filter(classification %in%  commonly_tested) %>%
  mutate(pathogen_tested = recode(pathogen_tested, !!!pathogen_groups),
         classification = snakecase::to_sentence_case(classification),
         classification = recode(classification,
                                 "Mus minutoides mattheyi" = "Mus minutoides/mattheyi"),
         genus = str_split(classification, " ", simplify = T)[1]) %>%
  filter(pathogen_tested == "Borrelia species")

histo_path_positive %>%
  group_by(classification, pathogen_tested) %>%
  summarise(n_pathogen_positive = sum(number))  %>%
  arrange(-n_pathogen_positive) %>%
  filter(classification %in%  commonly_tested) %>%
  mutate(pathogen_tested = recode(pathogen_tested, !!!pathogen_groups),
         classification = snakecase::to_sentence_case(classification),
         classification = recode(classification,
                                 "Mus minutoides mattheyi" = "Mus minutoides/mattheyi"),
         genus = str_split(classification, " ", simplify = T)[1]) %>%
  filter(pathogen_tested == "Borrelia species")


