source(here::here("scripts", "libraries.R"))

# Data --------------------------------------------------------------------
studies <- read_rds(here("data_clean", "studies.rds"))
rodent_data <- read_rds(here("data_clean", "rodent_df.rds")) %>%
  tibble()  %>%
  dplyr::select(-all_of(c("intensity_use", "geometry", "trap_nights")))

habitat_split <- c("habitat_1", "habitat_2", "habitat_3", "habitat_4", "habitat_5", "habitat_6", "habitat_7")
habitat_data <- read_rds(here("data_clean", "habitat_types.rds")) %>%
  tibble() %>%
  dplyr::select(record_id, all_of(habitat_split), intensity_use)

rodent_data %<>%
  left_join(.,
            habitat_data ,
            by = "record_id")

imputed_tn <- read_rds(here("data_clean", "imputed_trap_nights.rds"))

rodent_data <- full_join(rodent_data, imputed_tn,
                  by = c("unique_id", "year_trapping", "month_trapping",
                         "region", "town_village", "habitat"))

pathogen <- read_rds(here("data_clean", "pathogen.rds"))
wide_pathogen <- read_rds(here("data_clean", "wide_pathogen.rds"))
species_gbif <- read_rds(here("data_clean", "species_data.rds")) %>%
  distinct(genus, species, gbif_id, genus_gbif, species_gbif)

long_pathogen <- read_rds(here("data_clean", "long_pathogen.rds"))


# Summary table -----------------------------------------------------------

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


# Publication year --------------------------------------------------------
ggplot(studies) +
  geom_bar(aes(x = year_publication)) +
  theme_minimal() +
  labs(x = "Publication year",
       y = "Number of publications",
       title = "Studies reporting rodent trapping in West African countries",
       caption = paste("N =", length(unique(studies$unique_id)), sep = " "))


# Year trapping started -----------------------------------------------------------

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

<<<<<<< HEAD
ggsave(plot = study_timings, filename = here("figures", "Fig_1_Panel_A.png"), dpi = 300, height = 8, width = 10)
=======
ggsave(plot = study_timings, filename = here("figures", "Fig_1_Panel_A.png"), dpi = 300, height = 8)
>>>>>>> 55927033cfb2829238efa72cd4a7eeb6ca51203d
write_rds(study_timings, here("plots", "study_timings.rds"))

described_studies <- study_start %>%
  filter(reported == "Yes")

summary(described_studies$year_end - described_studies$year_start)

summary(described_studies$publishing_delay, na.rm = T)

# Study aims ---------------------------------------------------------------
# Exploring the different aims of studies

# ### No longer being included
# table(studies$aim)
# ecology_studies <- studies %>% filter(aim == "Ecology")
# zoonoses_studies <- studies %>% filter(aim == "Zoonoses risk")
#
# aim_detail_eco <- studies %>% filter(aim == "Ecology") %>% dplyr::select(unique_id, aim, aim_detail_1, aim_detail_2)
# aim_detail_zoo <- studies %>% filter(aim == "Zoonoses risk") %>%  dplyr::select(unique_id, aim, aim_detail_1, aim_detail_2)

# Study location ----------------------------------------------------------
# Identifying countries trapping occured in and the number of different trap sites used

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


# Study methodology -------------------------------------------------------
# Investigating the set up of different studies

# ### No longer being included
# studies %>%
#   filter(trap_types != "not_stated" & geolocation_level %in% c("study_site", "trap_site") & trapping_effort == "Yes") %>%
#   nrow()
#
# table(studies$pathogen)

# Trap type and setup -----------------------------------------------------
# ### No longer being included
#
# trap_type <- studies %>%
#   separate(col = trap_types, into = c("trap_1", "trap_2", "trap_3", "trap_4"), sep = ", ", remove = T) %>%
#   pivot_longer(cols = c("trap_1", "trap_2", "trap_3", "trap_4"), values_to = "trap_type") %>%
#   drop_na(trap_type)
#
# trap_type %>% filter(!trap_type %in% c("hand", "not_stated")) %>% group_by(unique_id) %>% summarise(n = n()) %>% count(n)
#
# trap_technique <- studies %>%
#   separate(col = trapping_method, into = c("method_1", "method_2", "method_3"), sep = ", ", remove = T) %>%
#   pivot_longer(cols = c("method_1", "method_2", "method_3"), values_to = "trap_method") %>%
#   drop_na(trap_method)
#
# trap_technique %>% filter(aim == "Zoonoses risk") %$%
#   table(trap_method)

# Trapping effort and trap success---------------------------------------------------------

table(studies$trapping_effort)
#table(ecology_studies$trapping_effort)
#table(zoonoses_studies$trapping_effort)

rodent_data %>%
  distinct(unique_id, region, town_village, habitat, trap_night_data, trap_nights) %>%
  group_by(unique_id, trap_night_data) %>%
  summarise(total_trap_nights = sum(trap_nights))

ggplot(sense_check %>%
         arrange(-total_trap_nights)) +
  geom_histogram(aes(x = total_trap_nights, fill = trap_night_data)) +
  theme_minimal()

trap_nights_complete <- rodent_data %>%
  filter(trap_night_data == "Actual")

buildings_tn <- trap_nights_complete  %>%
  filter(str_detect(habitat_1, "building|urban|village|Urban|Rural|airport") |
           str_detect(habitat_2, "building|urban|village|Urban|Rural|airport") |
           str_detect(habitat_3, "building|urban|village|Urban|Rural|airport") |
           str_detect(habitat_4, "building|urban|village|Urban|Rural|airport") |
           str_detect(habitat_5, "building|urban|village|Urban|Rural|airport") |
           str_detect(habitat_6, "building|urban|village|Urban|Rural|airport") |
           str_detect(habitat_7, "building|urban|village|Urban|Rural|airport")) %>%
  group_by(across(all_of(c("unique_id", "year_trapping", "month_trapping", "region", "town_village", habitat_split)))) %>%
  summarise(total_captures = sum(number),
            total_trapnights = round(mean(trap_nights), 0),
            trap_success = round(total_captures/total_trapnights, 4))

summary(buildings_tn$trap_success)

non_buildings_tn <- trap_nights_complete %>%
  group_by(across(all_of(c("unique_id", "year_trapping", "month_trapping", "region", "town_village", habitat_split)))) %>%
  summarise(total_captures = sum(number),
            total_trapnights = round(mean(trap_nights), 0),
            trap_success = round(total_captures/total_trapnights, 4)) %>%
  anti_join(., buildings_tn)

summary(non_buildings_tn$trap_success)

# Trap night and population -----------------------------------------------
if(!file.exists(here("data_clean", "pop_tn_analysis.rds"))) {
  rodent_spatial <- read_rds(here("data_clean", "rodent_spatial.rds")) %>%
    dplyr::select(-trap_nights)

  imputed_tn <- read_rds(here("data_clean", "imputed_trap_nights.rds"))

  rodent_spatial <- rodent_spatial %>%
    left_join(., imputed_tn,
              by = c("unique_id", "year_trapping", "month_trapping",
                     "region", "town_village", "habitat"))

  level_2 <- read_rds(here("data_download", "admin_spatial", "level_2_admin.rds"))
  non_trapped <- read_rds(here("data_download", "admin_spatial", "level_2_TGOGMB.rds"))

  level_2 <- bind_rows(level_2, non_trapped)

  zoonotic_studies <- studies %>%
    filter(str_detect(aim, "Zoonoses")) %>%
    pull(unique_id)

  trapping_effort <- rodent_spatial %>%
    filter(unique_id %in% zoonotic_studies) %>%
    distinct(unique_id, year_trapping, month_trapping, country, region, town_village, habitat, trap_nights, geometry)

  sites_2 <- st_intersection(x = level_2, y = trapping_effort)

  trap_nights_region <- tibble(sites_2) %>%
    group_by(NAME_2, GID_2, unique_id, year_trapping, month_trapping, region, town_village, habitat) %>%
    summarise(total_trap_nights = sum(trap_nights)) %>%
    group_by(GID_2, NAME_2) %>%
    summarise(region_trap_nights = sum(total_trap_nights))

  sites_2 <- level_2 %>%
    left_join(., trap_nights_region, by = c("GID_2", "NAME_2")) %>%
    mutate(region_trap_nights = case_when(is.na(region_trap_nights) ~ 0,
                                          TRUE ~ region_trap_nights))  %>%
    mutate(area_m2 = st_area(.),
           tn_density = region_trap_nights/(as.numeric(area_m2)/1000000),
           tn_density = ifelse(is.na(tn_density), NA, tn_density))

  write_rds(sites_2, here("data_clean", "traps_level_2_zoonoses.rds"))
  human_pop <- rast(here("data_download", "pop_2005","pop_2005.tif"))

  vect_sites <- vect(sites_2)

  crop_pop <- crop(human_pop, vect_sites)
  wa_pop <- writeRaster(crop_pop, here("data_download", "pop_2005","wa_pop_2005.tif"), overwrite = TRUE)
  region_pop <- terra::extract(crop_pop, vect_sites, fun = "median", method = "simple", na.rm = TRUE, touches = TRUE)
  region_pop_sf <- cbind(vect_sites, region_pop) %>%
    st_as_sf() %>%
    st_centroid() %>%
    mutate(x = st_coordinates(.$geometry)[,1],
           y = st_coordinates(.$geometry)[,2],
           tn_density = case_when(is.na(tn_density) ~ 0,
                                  TRUE ~ tn_density)) %>%
    tibble() %>%
    dplyr::select(-geometry)

  write_rds(region_pop_sf, here("data_clean", "pop_tn_analysis.rds"))
} else {
  region_pop_sf <- read_rds(here("data_clean", "pop_tn_analysis.rds"))
  human_pop <- rast(here("data_download", "pop_2005","wa_pop_2005.tif"))
}

tn_pop_model <- gam(tn_density ~ s(x, y, k = 480) + s(log(pop_2005), k = 9),
                    family = "tw",
                    data = region_pop_sf %>% filter(GID_0 != "CPV"))

summary(tn_pop_model)
gam.check(tn_pop_model)
plot(tn_pop_model, all.terms = TRUE)

write_rds(tn_pop_model, here("data_clean", "tn_pop_model.rds"))

# all_cells <- rasterToPoints(raster(human_pop)) %>%
#   tibble(x = .[,1],
#          y = .[,2],
#          pop_2005 = .[,3]) %>%
#   dplyr::select(x, y, pop_2005)
#
# prediction_space <- tibble(x = all_cells$x,
#                            y = all_cells$y,
#                            pop_2005 = all_cells$pop_2005)
# prediction_space <- prediction_space %>%
#   mutate(predict = as.numeric(predict.gam(tn_pop_model,
#                                 prediction_space)))
#
# prediction_raster <- vect(prediction_space, geom = c("x", "y"))
#
# prediction_raster <- terra::rasterize(., human_pop,
#                    field = "predict")
# names(prediction_raster) <- "predictor"
#
# writeRaster(prediction_raster, here("data_clean", "prediction_space.tif")) # This then gets mapped in the mapping_traps.R script

# Habitat classification --------------------------------------------------
habitat_2005 <- raster(here("data_download", "habitat_2005", "habitat_2005.nc"))
crop_habitat <- crop(habitat_2005, raster(human_pop)) %>%
  rast()

sites_2 <- read_rds(here("data_clean", "traps_level_2_zoonoses.rds")) %>%
  vect()

all_regions <- lapply(1:nrow(sites_2), function(x) crop(crop_habitat, sites_2[x,]))
all_regions_hab <- lapply(1:length(all_regions), function(x) as.data.frame(freq(all_regions[[x]])) %>%
                            select(-layer))
names(all_regions_hab) <- sites_2$GID_2
habitats <- as.data.frame(data.table::rbindlist(all_regions_hab, idcol = TRUE)) %>%
  rename("GID_2" = ".id")

land_type_classification <- as.list(c("cropland",
                                      "cropland",
                                      "cropland",
                                      "cropland",
                                      "mosaic_cropland",
                                      "mosaic_cropland",
                                      "tree_cover",
                                      "tree_cover",
                                      "tree_cover",
                                      "tree_cover",
                                      "mosaic_vegetation",
                                      "mosaic_vegetation",
                                      "shrubland",
                                      "shrubland",
                                      "grassland",
                                      "sparse_vegetation",
                                      "sparse_vegetation",
                                      "sparse_vegetation",
                                      "flooded",
                                      "flooded",
                                      "flooded",
                                      "urban",
                                      "bare",
                                      "bare",
                                      "bare",
                                      "water"))
names(land_type_classification) <- as.list(c(10, 11, 12, 20, 30, 40, 50, 60, 61, 62, 100, 110, 120, 122, 130, 150, 152, 153, 160, 170, 180, 190, 200, 201, 202, 210))

wa_habitats <- habitats %>%
  mutate(habitat = recode(value, !!!land_type_classification)) %>%
  group_by(GID_2, habitat) %>%
  summarise(count = sum(count))

plot_wa_habitats <- wa_habitats %>%
  group_by(habitat) %>%
  filter(habitat != "water") %>%
  summarise(count = sum(count)) %>%
  mutate(data = "all",
         proportion = count/sum(count))

zoonotic_regions <- as.data.frame(sites_2) %>%
  filter(tn_density > 0) %>%
  distinct(GID_2)

plot_trap_habitat <- wa_habitats %>%
  filter(GID_2 %in% zoonotic_regions$GID_2) %>%
  group_by(habitat) %>%
  filter(habitat != "water") %>%
  summarise(count = sum(count)) %>%
  mutate(data = "Zoonotic",
         proportion = count/sum(count))

plot_habitats <- bind_rows(plot_wa_habitats,
                           plot_trap_habitat) %>%
  arrange(-proportion) %>%
  mutate(habitat = fct_inorder(snakecase::to_sentence_case(habitat)),
         data = str_to_sentence(data)) %>%
  ggplot() +
  geom_col(aes(x = fct_rev(habitat), y = proportion, fill = data), position = position_dodge2()) +
  coord_flip() +
  labs(x = "Land cover classification",
       y = "Proportion of region",
       fill = "Region") +
  scale_fill_manual(values = c("#fde725", "#440154")) +
  theme_minimal()

write_rds(plot_habitats, here("plots", "trap_habitats.rds"))

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
t_effort <- imputed_tn %>%
  filter(trap_night_data == "Actual")

inc_effort <- imputed_tn %>%
  filter(trap_night_data == "Estimated" | trap_night_data == "Imputed")

single_site <- species_data %>%
  filter(unique_id %in% c(t_effort$unique_id, inc_effort$unique_id)) %>%
  drop_na(trap_nights) %>%
  mutate(trap_nights = as.numeric(trap_nights)) %>%
  distinct(unique_id, year_trapping, month_trapping, town_village, habitat, .keep_all = T) %>%
  filter(trap_night_unit %in% c("habitat", "study_site", "trap_site", "study_habitat", NA))

single_site_n <- single_site %>%
  summarise(trap_nights = sum(trap_nights, na.rm = TRUE))

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
  mutate(trap_nights = as.numeric(trap_nights)) %>%
  summarise(trap_nights = sum(trap_nights, na.rm = TRUE))

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
  mutate(trap_nights = as.numeric(trap_nights)) %>%
  summarise(trap_nights = sum(trap_nights, na.rm = TRUE))

study_c <- species_data %>%
  filter(unique_id %in% study$unique_id) %>%
  summarise(captures = sum(number))

study_c/study_n*100

bind_rows(single_site, study_site %>%
            mutate(trap_nights = as.numeric(trap_nights)),
          study %>%
            mutate(trap_nights = as.numeric(trap_nights))) %>%
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
  filter(str_detect(assay, regex("tested"))) %>%
  group_by(pathogen_tested) %>%
  summarise(n_pathogen_tested = sum(number)) %>%
  arrange(-n_pathogen_tested) %>% # The pathogens tested for
  print(n = 32)

long_pathogen %>%
  filter(str_detect(assay, regex("positive"))) %>%
  mutate(assay = case_when(str_detect(assay, regex("pcr")) ~ "PCR",
                           str_detect(assay, regex("ab_ag")) ~ "Antibody/antigen",
                           str_detect(assay, regex("histo")) ~ "Histology/direct visualisation",
                           str_detect(assay, regex("culture")) ~ "Culture",
                           TRUE ~ "Error")) %>%
  dplyr::select(unique_id, assay, pathogen_tested) %>%
  distinct(unique_id, assay, pathogen_tested) %>%
  group_by(pathogen_tested, assay) %>%
  summarise(studies = n()) %>%
  pivot_wider(id_cols = pathogen_tested, names_from = assay, values_from = studies) %>%
  mutate(pathogen_tested = snakecase::to_any_case(pathogen_tested, case = "sentence"),
         pathogen_tested = case_when(str_detect(pathogen_tested, regex("esbl")) ~ str_replace(pathogen_tested, "esbl", "ESBL"),
                                     TRUE ~ pathogen_tested)) %>%
  rename("Pathogen tested" = pathogen_tested) %>%
  saveRDS(file = here("tables", "pathogen_table.rds"))

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


# Top four pathogens, rodents tested --------------------------------------
arenaviridae = c("arenaviridae_species", "lassa_mammarenavirus", "mammarenavirus_species")
borrelia = c("borrelia_species", "borrelia")
bartonella = c("bartonella_species")
toxoplasma = c("toxoplasma_gondii")

four_paths <- vctrs::vec_c(arenaviridae, borrelia, bartonella, toxoplasma)

four_paths_wide <- wide_pathogen %>%
  tibble() %>%
  dplyr::select(1:16, matches(four_paths)) %>%
  left_join(., species_data,
            by = c("gbif_id", "classification")) %>%
  distinct(record_id.x, .keep_all = T)

pathogen_table <- function(pathogen_genus) {
  pathogen_groups = list(arenaviridae = c("arenaviridae_species", "lassa_mammarenavirus", "mammarenavirus_species"),
                         borrelia = c("borrelia_species", "borrelia"),
                         bartonella = c("bartonella_species"),
                         toxoplasma = c("toxoplasma_gondii"))

  four_paths_wide %>%
    dplyr::select(1:16, matches(c(pathogen_groups[[pathogen_genus]])),
                  genus, species, genus_gbif, species_gbif) %>%
    janitor::remove_empty("cols")  %>%
    mutate(number_tested = rowSums(.[grep("tested", names(.))], na.rm = T)) %>%
    filter(number_tested != 0) %>%
    mutate(pcr_positive = rowSums(.[grep("pcr", names(.))], na.rm = T),
           ab_ag_positive = rowSums(.[grep("ab_ag", names(.))], na.rm = T),
           culture_positive = rowSums(.[grep("culture", names(.))], na.rm = T),
           pos_neg = case_when(pcr_positive + ab_ag_positive + culture_positive > 0 ~ "Positive",
                               TRUE ~ "Negative")) %>%
    dplyr::select(1:15, all_of(c("number_tested", "pcr_positive", "ab_ag_positive", "culture_positive", "pos_neg")),
                  genus, species, genus_gbif, species_gbif) %>%
    tibble() %>%
    mutate(genus = snakecase::to_sentence_case(genus),
           `Species` = snakecase::to_sentence_case(classification),
           pos_neg = case_when(pos_neg == "Positive" ~ 1,
                               TRUE ~ 0)) %>%
    mutate(unique_positive = case_when(pcr_positive == 0 ~ ab_ag_positive + culture_positive,
                                pcr_positive != 0 & ab_ag_positive != 0 ~ ab_ag_positive + culture_positive,
                                pcr_positive != 0 & ab_ag_positive == 0 ~ pcr_positive + culture_positive,
                                TRUE ~ ab_ag_positive + culture_positive)) %>%
    group_by(genus, `Species`) %>%
    summarise(`Tested` = sum(number_tested),
              `Positive` = sum(unique_positive, na.rm = TRUE),
              `Negative` = `Tested`-`Positive`) %>%
    mutate(`Prop. positive` = round(`Positive`/`Tested`, 3)) %>%
    ungroup() %>%
    arrange(-`Positive`, -`Tested`) %>%
    dplyr::select(-genus)
}

areanaviridae_species <- pathogen_table("arenaviridae")
areanviridae_positive <- areanaviridae_species %>%
  filter(Positive >= 1)
borrelia_species <- pathogen_table("borrelia")
borrelia_positive <- borrelia_species %>%
  filter(Positive >= 1)
bartonella_species <- pathogen_table("bartonella")
bartonella_positive <- bartonella_species %>%
  filter(Positive >= 1)
toxoplasma_species <- pathogen_table("toxoplasma")
toxoplasma_positive <- toxoplasma_species %>%
  filter(Positive >= 1)

# Host-pathogen -----------------------------------------------------------
genus_gbif <- read_rds(here("data_clean", "genus_gbif.rds"))

speciation <- speciation %>%
  rename("gbif_id" = 1)
genus_gbif <- genus_gbif %>%
  mutate(gbif_id = as.character(gbif_id),
         genus = str_to_sentence(genus)) %>%
  filter(!gbif_id %in% speciation[,1])

genus_data <- bind_rows(speciation %>%
                          select(gbif_id, genus),
                        genus_gbif) %>%
  ungroup() %>%
  left_join(., speciation %>%
              select(genus, family),
            by = "genus") %>%
  distinct(gbif_id.x, genus, family) %>%
  rename("gbif_id" = 1)


matrix_hp <- long_pathogen %>%
  left_join(., genus_data,
            by = "gbif_id") %>%
  group_by(classification, gbif_id, family, assay, pathogen_tested) %>%
  summarise(number = sum(number)) %>%
  mutate(assay = case_when(str_detect(assay, "tested") ~ "Tested",
                           TRUE ~ "Prop. positive")) %>%
  group_by(classification, gbif_id, family, pathogen_tested, assay) %>%
  summarise(number = sum(number)) %>%
  group_by(classification, gbif_id, family, pathogen_tested) %>%
  mutate(number = round(case_when(assay == "Prop. positive" & number == 0 ~ 0,
                                  assay == "Prop. positive" & number != 0 ~ number/max(number, na.rm = TRUE),
                            TRUE ~ number), 4)) %>%
  pivot_wider(names_from = assay, values_from = number) %>%
  mutate(Tested = sqrt(Tested)) %>%
  mutate(Percent_positive = factor(case_when(`Prop. positive` == 0 ~ "0%",
                                         `Prop. positive` > 0 & `Prop. positive` <= 0.005 ~ "0 - 0.5%",
                                         `Prop. positive` > 0.005 & `Prop. positive` <= 0.01 ~ "0.5% - 1%",
                                         `Prop. positive` > 0.01 & `Prop. positive` <= 0.05 ~ "1% - 5%",
                                         `Prop. positive` > 0.05 & `Prop. positive` <= 0.1 ~ "5% - 10%",
                                         `Prop. positive` > 0.1 & `Prop. positive` <= 0.2 ~ "10% - 20%",
                                         `Prop. positive` > 0.2 & `Prop. positive` <= 0.5 ~ "20% - 50%",
                                         `Prop. positive` > 0.5 & `Prop. positive` <= 1 ~ "50% - 100%",
                                         TRUE ~ "Missing"),
                                   levels = c("0%", "0 - 0.5%", "0.5% - 1%", "1% - 5%", "5% - 10%",
                                              "10% - 20%", "20% - 50%", "50% - 100%")),
         classification = str_to_sentence(classification),
         pathogen_tested = str_to_sentence(pathogen_tested),
         pathogen_tested = str_replace_all(pathogen_tested, "_", " "))

greater_4 <- matrix_hp %>%
  group_by(classification) %>%
  add_count() %>%
  filter(n >= 4)

less_4 <- matrix_hp %>%
  group_by(classification) %>%
  add_count() %>%
  filter(n < 4)

#   pivot_wider(names_from = pathogen_tested, values_from = number)
#
# corplot_mat <- matrix_hp %>%
#   filter(assay == "Positive") %>%
#   ungroup() %>%
#   select(1, 4:10) %>%
#   head(30) %>%
#   pivot_longer(cols = c(2:8),
#                names_to = "Microorganism",
#                values_to = "Number")

hp_g4 <- ggplot(greater_4,
       aes(x = pathogen_tested, y = fct_rev(classification), colour = Percent_positive, size = Tested)) +
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_colour_viridis_d() +
  scale_x_discrete(position = "top") +
  labs(x = element_blank(),
       y = element_blank(),
       colour = element_blank()) +
  guides(size = "none")

hp_l4 <- ggplot(less_4,
                aes(x = pathogen_tested, y = fct_rev(classification), colour = Percent_positive, size = Tested)) +
  geom_point() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_colour_viridis_d() +
  scale_x_discrete(position = "top") +
  labs(x = element_blank(),
       y = element_blank(),
       colour = element_blank()) +
  guides(size = "none",
         colour = "none")

save_plot(here("figures", "Figure_5.png"), plot_grid(hp_g4, hp_l4),
          base_height = 12,
          base_width = 18)

pal <- c("Arvicanthis niloticus" = "#bdbdbd",
         "Crocidura foxi" = "#e6550d",
         "Crocidura olivieri" = "#fdae6b",
         "Crocidura sp" = "#fee6ce",
         "Gerbilliscus gambiana" = "#bdbdbd",
         "Gerbillus tarabuli" = "#bdbdbd",
         "Lophuromys sikapusi" = "#bdbdbd",
         "Mastomys erythroleucus" = "#238b45",
         "Mastomys huberti" = "#74c476",
         "Mastomys natalensis" = "#bae4b3",
         "Mastomys sp" = "#edf8e9",
         "Mus mattheyi" = "#6a51a3",
         "Mus minutoides" = "#9e9ac8",
         "Mus minutoides/mattheyi" = "#cbc9e2",
         "Mus musculus" = "#f2f0f7",
         "Praomys daltoni" = "#de2d26",
         "Praomys rostratus" = "#fc9272",
         "Rattus norvegicus" = "#3182bd",
         "Rattus rattus" = "#9ecae1")

pathogen_groups = c("arenaviridae" = "arenaviridae_species",
                    "arenaviridae" = "lassa_mammarenavirus",
                    "arenaviridae" = "mammarenavirus_species",
                    "borrelia" = "borrelia_species",
                    "borrelia" = "borrelia",
                    "bartonella" = "bartonella_species",
                    "toxoplasma" = "toxoplasma_gondii")

rodent_testing <- tested %>%
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

ggsave(filename = "rodent_testing.png", plot = rodent_testing, path = here("figures"), height = 21, width = 19, units = "cm")

positive_assays <- long_pathogen %>%
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

# Uninfected rodents --------------------------------------------------------

uninfected_pcr <- pcr_positive %>%
  group_by(classification, pathogen_tested) %>%
  summarise(n_pathogen_positive = sum(number))  %>%
  arrange(-n_pathogen_positive) %>%
  mutate(pathogen_tested = recode(pathogen_tested, !!!pathogen_groups),
         classification = snakecase::to_sentence_case(classification),
         classification = recode(classification,
                                 "Mus minutoides mattheyi" = "Mus minutoides/mattheyi"),
         genus = str_split(classification, " ", simplify = T)[1]) %>%
  filter(n_pathogen_positive == 0)

uninfected_ab <- ab_ag_positive %>%
  group_by(classification, pathogen_tested) %>%
  summarise(n_pathogen_positive = sum(number))  %>%
  arrange(-n_pathogen_positive) %>%
  filter(classification %in%  commonly_tested) %>%
  mutate(pathogen_tested = recode(pathogen_tested, !!!pathogen_groups),
         classification = snakecase::to_sentence_case(classification),
         classification = recode(classification,
                                 "Mus minutoides mattheyi" = "Mus minutoides/mattheyi"),
         genus = str_split(classification, " ", simplify = T)[1]) %>%
  filter(n_pathogen_positive == 0)

uninfected_hist <- histo_path_positive %>%
  group_by(classification, pathogen_tested) %>%
  summarise(n_pathogen_positive = sum(number))  %>%
  arrange(-n_pathogen_positive) %>%
  filter(classification %in%  commonly_tested) %>%
  mutate(pathogen_tested = recode(pathogen_tested, !!!pathogen_groups),
         classification = snakecase::to_sentence_case(classification),
         classification = recode(classification,
                                 "Mus minutoides mattheyi" = "Mus minutoides/mattheyi"),
         genus = str_split(classification, " ", simplify = T)[1]) %>%
  filter(n_pathogen_positive == 0)

uninfected_culture <- culture_positive %>%
  group_by(classification, pathogen_tested) %>%
  summarise(n_pathogen_positive = sum(number))  %>%
  arrange(-n_pathogen_positive) %>%
  mutate(pathogen_tested = recode(pathogen_tested, !!!pathogen_groups),
         classification = snakecase::to_sentence_case(classification),
         classification = recode(classification,
                                 "Mus minutoides mattheyi" = "Mus minutoides/mattheyi"),
         genus = str_split(classification, " ", simplify = T)[1]) %>%
  filter(n_pathogen_positive == 0)

positive_assays <- positive_assays %>%
  filter(n_pathogen_positive == 0) %>%
  mutate(classification = snakecase::to_sentence_case(classification),
         classification = recode(classification,
                                 "Mus minutoides mattheyi" = "Mus minutoides/mattheyi"))

negative_assays <- bind_rows(uninfected_pcr, uninfected_ab, uninfected_hist, uninfected_culture) %>%
  ungroup() %>%
  distinct(classification) %>%
  filter(classification %in% positive_assays$classification & !str_detect(classification, regex("sp"))) %>%
  add_case(classification = "Mastomys kollmannspergeri") %>%
  mutate(genus = str_split(classification, " ", simplify = T)[,1]) %>%
  left_join(., genus_data %>%
              distinct(genus, .keep_all = T),
            by = "genus")
