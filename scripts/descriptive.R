source(here::here("scripts", "libraries.r"))
studies <- read_rds(here("data_clean", "studies.rds"))
rodent_data <- read_rds(here("data_clean", "rodent_df.rds"))

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

table(studies$trapping_effort)

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

sites <- studies %>%
  full_join(., rodent_data  %>%
              filter(!country %in% c("Cameroon", "Chad", "Morocco")) %>%
              distinct(unique_id, region, town_village, habitat, geometry),
            by = "unique_id")

a <- sites %>%
  group_by(unique_id) %>%
  summarise(n = n())
table(a$n)

# Species identification --------------------------------------------------

table(studies$speciation)

studies %>%
  filter(year_publication < 2010) %>%
  count(speciation)

# Rodents -----------------------------------------------------------------
wa_countries <- c("BEN", "BFA", "CIV", "CPV", "ESH", "GHA",
                  "GIN", "GMB", "GNB", "LBR", "MLI", "MRT",
                  "NER", "NGA", "SEN", "SLE", "TGO")

genus_data <- read_rds(here("data_clean", "trapped_genera.rds"))
species_data <- read_rds(here("data_clean", "species_data.rds"))

genus_data %>%
  count(order)

speciation <- species_data %>%
  filter(!is.na(species_gbif) & iso3c %in% wa_countries) %>%
  mutate(species_gbif = as.character(species_gbif)) %>%
  distinct(species_gbif, .keep_all = T) %>%
  dplyr::select(classification, species_gbif, genus) %>%
  mutate(genus = snakecase::to_sentence_case(genus)) %>%
  arrange(classification) %>%
  left_join(., genus_data %>%
              distinct(genus, .keep_all = T), by = "genus")

count_genus <- species_data %>%
  group_by(genus_gbif, genus) %>%
  filter(genus != "rodentia") %>%
  summarise(number = sum(number)) %>%
  mutate(percent = round(number/sum(.$number)*100, 2)) %>%
  arrange(-percent)

count_species <- species_data %>%
  group_by(species_gbif) %>%
  summarise(number = sum(number))
