source(here::here("scripts", "libraries.R"))

# Data --------------------------------------------------------------------
rodent_data <- read_rds(here("data_clean", "rodent_df.rds")) %>%
  tibble()
habitat_split <- c("habitat_1", "habitat_2", "habitat_3", "habitat_4", "habitat_5", "habitat_6", "habitat_7")

rodent_data %<>%
  separate(habitat, into = c(habitat_split), sep = ", ", remove = F)

source(here("scripts", "habitat_dictionary.R"))
cleaned_habitat <- read_rds(here("data_clean", "habitat_dictionary.rds"))

rodent_data %<>%
  mutate(habitat_1 = recode(habitat_1, !!!cleaned_habitat),
         habitat_2 = recode(habitat_2, !!!cleaned_habitat),
         habitat_3 = recode(habitat_3, !!!cleaned_habitat),
         habitat_4 = recode(habitat_4, !!!cleaned_habitat),
         habitat_5 = recode(habitat_5, !!!cleaned_habitat),
         habitat_6 = recode(habitat_6, !!!cleaned_habitat),
         habitat_7 = recode(habitat_7, !!!cleaned_habitat)) # the mapping is then applied to all the reported habitats
# Imputing trapping effort -------------------------------------------------

study_site <- tibble(rodent_data) %>%
  mutate(trap_night_unit = case_when(trap_night_unit %in% c("trap_site", "habitat", "study_site", "village", "study_visit", "study_habitat", "site", "trap_session") ~ "study_site",
                                     TRUE ~ trap_night_unit)) %>%
  filter(trap_night_unit == "study_site" & is.na(study_nights)) %>%
  distinct(unique_id, year_trapping, month_trapping, iso3c, region, town_village, habitat,
           habitat_1, habitat_2, habitat_3, habitat_4, habitat_5, habitat_6, habitat_7,
           classification, number, trap_nights, capture_rate) %>%
  mutate(trap_nights = case_when(trap_nights == "50-200" ~ as.numeric(125),
                                 TRUE ~ as.numeric(trap_nights))) %>%
  group_by(unique_id, year_trapping, month_trapping, iso3c, region, town_village, habitat, habitat_1,
           habitat_2, habitat_3, habitat_4, habitat_5, habitat_6, habitat_7, trap_nights, capture_rate) %>%
  summarise(total_captures = sum(number))

buildings_study_site <- study_site %>%
  ungroup() %>%
  filter(str_detect(habitat_1, "building|urban|village|Urban|Rural|airport") |
           str_detect(habitat_2, "building|urban|village|Urban|Rural|airport") |
           str_detect(habitat_3, "building|urban|village|Urban|Rural|airport") |
           str_detect(habitat_4, "building|urban|village|Urban|Rural|airport") |
           str_detect(habitat_5, "building|urban|village|Urban|Rural|airport") |
           str_detect(habitat_6, "building|urban|village|Urban|Rural|airport") |
           str_detect(habitat_7, "building|urban|village|Urban|Rural|airport")) %>%
  mutate(trap_nights = round(case_when(is.na(trap_nights) ~ total_captures/as.numeric(capture_rate),
                                       TRUE ~ trap_nights), 0),
         trap_success = round(total_captures/trap_nights, 2))

summary(buildings_study_site$trap_success)
building_ts <- median(buildings_study_site$trap_success, na.rm = T)

non_building_study_site <- study_site %>%
  ungroup() %>%
  anti_join(., buildings_study_site,
            by = c("unique_id", "year_trapping", "month_trapping", "iso3c", "region", "town_village", "habitat", "habitat_1",
                   "habitat_2", "habitat_3", "habitat_4", "habitat_5", "habitat_6", "habitat_7", "trap_nights")) %>%
  mutate(trap_success = round(total_captures/trap_nights, 2))

summary(non_building_study_site$trap_success)
general_ts <- median(non_building_study_site$trap_success, na.rm = T)

complete_tn <- tibble(rodent_data) %>%
  mutate(trap_night_unit = case_when(trap_night_unit %in% c("trap_site", "habitat", "study_site", "village", "study_visit", "study_habitat", "site", "trap_session") ~ "study_site",
                                     TRUE ~ trap_night_unit)) %>%
  filter(trap_night_unit == "study_site" & is.na(study_nights)) %>%
  mutate(trap_nights = case_when(trap_nights == "50-200" ~ as.numeric(125),
                                 TRUE ~ as.numeric(trap_nights))) %>%
  group_by(unique_id, year_trapping, month_trapping, region, town_village, habitat, trap_nights, capture_rate) %>%
  summarise(total_captures = sum(number)) %>%
  mutate(trap_nights = round(case_when(is.na(trap_nights) ~ total_captures/as.numeric(capture_rate),
                                       TRUE ~ trap_nights), 0),
         trap_night_data = "Actual")

missing_tn <- tibble(rodent_data) %>%
  mutate(trap_night_unit = case_when(trap_night_unit %in% c("trap_site", "habitat", "study_site", "village", "study_visit", "study_habitat", "site", "trap_session") ~ "study_site",
                                     TRUE ~ trap_night_unit)) %>%
  filter(trap_night_unit != "study_site") %>%
  distinct(unique_id, year_trapping, month_trapping, iso3c, region, town_village, habitat,
           habitat_1, habitat_2, habitat_3, habitat_4, habitat_5, habitat_6, habitat_7,
           classification, number, trap_nights, capture_rate, trap_night_unit) %>%
  mutate(trap_nights = as.numeric(trap_nights)) %>%
  filter(!is.na(trap_nights)) %>%
  distinct(unique_id, year_trapping, month_trapping, region, town_village, habitat, trap_nights) %>%
  group_by(unique_id) %>%
  mutate(n_sites = n(),
         trap_nights = round(trap_nights/n_sites, 0),
         trap_night_data = "Estimated")

trap_nights <- bind_rows(complete_tn, missing_tn) %>%
  ungroup() %>%
  distinct(unique_id)

no_data <- rodent_data %>%
  anti_join(., trap_nights) %>%
  distinct(unique_id, year_trapping, month_trapping, iso3c, region, town_village, habitat,
           habitat_1, habitat_2, habitat_3, habitat_4, habitat_5, habitat_6, habitat_7,
           classification, number, trap_nights, capture_rate, trap_night_unit)

no_data_buildings <- no_data %>%
  ungroup() %>%
  filter(str_detect(habitat_1, "building|urban|village|Urban|Rural|airport") |
           str_detect(habitat_2, "building|urban|village|Urban|Rural|airport") |
           str_detect(habitat_3, "building|urban|village|Urban|Rural|airport") |
           str_detect(habitat_4, "building|urban|village|Urban|Rural|airport") |
           str_detect(habitat_5, "building|urban|village|Urban|Rural|airport") |
           str_detect(habitat_6, "building|urban|village|Urban|Rural|airport") |
           str_detect(habitat_7, "building|urban|village|Urban|Rural|airport")) %>%
  group_by(unique_id, year_trapping, month_trapping, iso3c, region, town_village, habitat, habitat_1,
           habitat_2, habitat_3, habitat_4, habitat_5, habitat_6, habitat_7) %>%
  summarise(total_captures = sum(number)) %>%
  mutate(trap_nights = case_when(round(total_captures/building_ts, 0) == 0 ~ 20,
                                 TRUE ~ round(total_captures/building_ts, 0)),
         trap_night_data = "Imputed")

no_data_other <- no_data %>%
  anti_join(., no_data_buildings,
            by = c("unique_id", "year_trapping", "month_trapping", "iso3c", "region", "town_village", "habitat", "habitat_1",
                   "habitat_2", "habitat_3", "habitat_4", "habitat_5", "habitat_6", "habitat_7")) %>%
  group_by(unique_id, year_trapping, month_trapping, iso3c, region, town_village, habitat, habitat_1,
           habitat_2, habitat_3, habitat_4, habitat_5, habitat_6, habitat_7) %>%
  summarise(total_captures = sum(number)) %>%
  mutate(trap_nights = case_when(round(total_captures/general_ts, 0) == 0 ~ 20,
                                 TRUE ~ round(total_captures/general_ts, 0)),
         trap_night_data = "Imputed")

imputed_trap_nights <- complete_tn %>%
  ungroup() %>%
  dplyr::select(unique_id, year_trapping, month_trapping, region, town_village, habitat, trap_nights, trap_night_data) %>%
  bind_rows(missing_tn %>%
              ungroup() %>%
              dplyr::select(unique_id, year_trapping, month_trapping, region, town_village, habitat, trap_nights, trap_night_data),
            no_data_buildings %>%
              ungroup() %>%
              dplyr::select(unique_id, year_trapping, month_trapping, region, town_village, habitat, trap_nights, trap_night_data),
            no_data_other %>%
              ungroup() %>%
              dplyr::select(unique_id, year_trapping, month_trapping, region, town_village, habitat, trap_nights, trap_night_data))

sense_check <- imputed_trap_nights %>%
  distinct(unique_id, region, town_village, habitat, trap_night_data, trap_nights) %>%
  group_by(unique_id, trap_night_data) %>%
  summarise(total_trap_nights = sum(trap_nights))

ggplot(sense_check %>%
         arrange(-total_trap_nights)) +
  geom_histogram(aes(x = total_trap_nights, fill = trap_night_data)) +
  theme_minimal()

write_rds(imputed_trap_nights, here("data_clean", "imputed_trap_nights.rds"))

