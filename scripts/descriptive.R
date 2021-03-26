source(here::here("scripts", "libraries.r"))
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

a <- countries %>%
  filter(!country %in% c("Cameroon", "Chad", "Morocco")) %>%
  group_by(unique_id) %>%
  summarise(n = n())
table(a$n) # calculate the number of countries trapped in by each study

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

table(trap_technique$trap_method)

# Trapping effort ---------------------------------------------------------

table(studies$trapping_effort)

t_effort <- studies %>% filter(trapping_effort == "Yes") %>% distinct(unique_id)

s_effort <- rodent_data %>% filter(unique_id %in% t_effort$unique_id) %>%
  group_by(unique_id, year_trapping, month_trapping, region, town_village, habitat_1) %>%
  summarise(trap_nights = unique(trap_nights)) %>%
  group_by(unique_id) %>%
  summarise(trap_nights = sum(trap_nights))

summary(s_effort$trap_nights)

t_effort <- rodent_data %>% filter(unique_id %in% t_effort$unique_id) %>%
  group_by(unique_id, year_trapping, month_trapping, region, town_village, habitat_1) %>%
  summarise(trap_nights = unique(trap_nights))

summary(t_effort$trap_nights)

inc_effort <- studies %>% filter(trapping_effort == "Incomplete")

inc_effort <- rodent_data %>% filter(unique_id %in% inc_effort$unique_id & !is.na(trap_nights)) %>%
  group_by(unique_id, year_trapping, month_trapping, region, town_village, habitat_1) %>%
  summarise(trap_nights = unique(trap_nights)) %>%
  group_by(unique_id) %>%
  summarise(trap_nights = unique(trap_nights)) %>%
  summarise(trap_nights = sum(trap_nights))

summary(inc_effort$trap_nights)

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
  filter(year_publication < 2010) %>%
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

write_rds(count_species, here("tables", "sup_table1.rds"))

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

# Pathogen ----------------------------------------------------------------


