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

source(here("scripts", "habitat_dictionary.r"))
cleaned_habitat <- read_rds(here("data_clean", "habitat_dictionary.rds"))  # we have created the dictionary and read it in

habitat_types %<>%
  mutate(habitat_type = recode(habitat_type, !!!cleaned_habitat)) # we apply it to the long form dataframe to ensure we have captured all the categories

data.frame(reported = names(cleaned_habitat), categorised = cleaned_habitat, row.names = NULL) %>%
  write_rds(here("data_clean", "habitat_suplementary.rds")) # this is the mapping that can be produced as supplementary information

rodent_data %<>%
  mutate(habitat_1 = recode(habitat_1, !!!cleaned_habitat),
         habitat_2 = recode(habitat_2, !!!cleaned_habitat),
         habitat_3 = recode(habitat_3, !!!cleaned_habitat),
         habitat_4 = recode(habitat_4, !!!cleaned_habitat),
         habitat_5 = recode(habitat_5, !!!cleaned_habitat),
         habitat_6 = recode(habitat_6, !!!cleaned_habitat),
         habitat_7 = recode(habitat_7, !!!cleaned_habitat)) # the mapping is then applied to all the reported habitats

write_rds(rodent_data, here("data_clean", "habitat_types.rds"))
