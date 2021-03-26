source(here::here("scripts", "libraries.r"))

rodent_data <- read_rds(here("data_clean", "rodent_df.rds")) %>%
  tibble()
habitat_split <- c("habitat_1", "habitat_2", "habitat_3", "habitat_4", "habitat_5", "habitat_6", "habitat_7")

rodent_data %<>%
  separate(habitat, into = c(habitat_split), sep = ", ", remove = F)

habitat_types <- rodent_data %>%
  dplyr::select(unique_id, country, region, town_village, all_of(habitat_split), trap_night_unit, trap_nights) %>%
  pivot_longer(cols = all_of(habitat_split), values_to = "habitat_type") %>%
  drop_na(habitat_type)

if(!file.exists(here("data_clean", "habitat_dictionary.rds"))){
sort(unique(habitat_types$habitat_type))
cleaned_habitat <- as.list(c("agriculture", "agriculture", "airport", "alluvial_plain", "buildings_periphery", "nature_reserve", "botanical_garden", "buildings_periphery",
                             "burned", "vegetation", "vegetation", "clay_soil", "buildings_periphery", "agriculture", "waterway", "waterway", "agriculture", "agriculture",
                             "agriculture", "agriculture", "agriculture", "degraded_forest", "agriculture", "agriculture", "degraded_forest", "forest", "waterway", "waterway",
                             "waterway", "forest", "sand_dunes", "vegetation", "fallow", "fallow", "fallow", "agriculture", "agriculture", "agriculture", "agriculture",
                             "alluvial_plain", "forest", "forest", "degraded_forest", "degraded_forest", "gardens", "vegetation", "buildings_periphery", "vegetation", "forest",
                             "vegetation", "vegetation", "buildings", "buildings", "agriculture", "buildings", "buildings_periphery", "buildings_periphery", "forest", "waterway",
                             "island", "lacustrian", "waterway", "vegetation", "building_periphery", "building_periphery", "building_periphery", "marsh", "agriculture",
                             "lateritic soil", "agriculture", "multiple", "nature_reserve", "forest_clearing", "natural_habitat", "natural_habitat", "forest", "agriculture",
                             "forest", "natural_habitat", "agriculture", "village", "oasis", "agriculture", "agriculture", "natural_habitat", "agriculture", "agriculture",
                             "agriculture", "agriculture", "village", "peri-urban", "village", "peri-urban", "peri-urban", "peri-urban", "peri-urban", "agriculture", "natural_habitat",
                             "natural_habitat", "forest", "agriculture", "raphial", "forest","forest", "village", "forest", "agriculture", "agriculture", "village", "agriculture",
                             "waterway", "waterway", "waterway", "waterway", "waterway", "waterway", "waterway", "village", "buildings", "degraded_forest", "forest", "sand_dune",
                             "sand_dune", "sand_dune", "sand_dune", "sand_dune", "sand_dune", "savannah", "savannah", "vegetation", "waterway", "vegetation", "forest", "vegetation",
                             "vegetation", "natural_habitat", "savannah", "buildings_periphery", "marsh", "forest", "forest", "vegetation", "agriculture", "peri-urban", "unburned",
                             "buildings_periphery", "urban", "urban", "urban", "buildings", "village", "village", "village", "village", "village", "natural_habitat", "waterway",
                             "forest", "building_periphery", "sand_dune"))
names(cleaned_habitat) <- sort(unique(habitat_types$habitat_type))
write_rds(cleaned_habitat, here("data_clean", "habitat_dictionary.rds"))
}

cleaned_habitat <- read_rds(here("data_clean", "habitat_dictionary.rds"))

habitat_types %<>%
  mutate(habitat_type = recode(habitat_type, !!!cleaned_habitat))

data.frame(Obs=unlist(cleaned_habitat)) %>%
  write_rds(here("data_clean", "habitat_suplementary.rds"))

rodent_data %<>%
  mutate(habitat_1 = recode(habitat_1, !!!cleaned_habitat),
         habitat_2 = recode(habitat_2, !!!cleaned_habitat),
         habitat_3 = recode(habitat_3, !!!cleaned_habitat),
         habitat_4 = recode(habitat_4, !!!cleaned_habitat),
         habitat_5 = recode(habitat_5, !!!cleaned_habitat),
         habitat_6 = recode(habitat_6, !!!cleaned_habitat),
         habitat_7 = recode(habitat_7, !!!cleaned_habitat))

write_rds(rodent_data, here("data_clean", "habitat_types.rds"))
