detection_non_detection <- rodent_data %>%
  select(unique_id, year_trapping, month_trapping, country, iso3c, region, town_village, habitat, classification, number, trap_nights, longitude, latitude) %>%
  mutate(detection = case_when(number > 0 ~ "Detected",
                              TRUE ~ "Not detected")) %>%
  select(- number)

abundance <- rodent_data %>%
  select(unique_id, year_trapping, month_trapping, country, iso3c, region, town_village, habitat, classification, number, trap_nights, longitude, latitude)

pathogen <- long_pathogen %>%
  select(unique_id, year_trapping, month_trapping = month, country, iso3c, region, town_village, classification, record_id, pathogen_tested, assay, number, geometry) %>%
  mutate(longitude = st_coordinates(geometry)[, 1],
         latitude = st_coordinates(geometry)[, 2])

write_csv(detection_non_detection, here("detection_non_detection.csv"))
write_csv(abundance, here("abundance.csv"))
write_csv(pathogen, here("pathogen.csv"))
