
# Load data ---------------------------------------------------------------

all_countries <- c("BEN", "BFA", "CIV", "CMR", "CPV", "DZA", "ESH", "GHA",
                   "GIN", "GMB", "GNB", "LBR", "MAR", "MLI", "MRT", "NER",
                   "NGA", "SEN", "SLE", "TCD", "TGO")
wa_countries <- c("BEN", "BFA", "CIV", "CPV", "ESH", "GHA",
                  "GIN", "GMB", "GNB", "LBR", "MLI", "MRT",
                  "NER", "NGA", "SEN", "SLE", "TGO")
continental_countries <- c("BEN", "BFA", "CIV", "ESH", "GHA",
                           "GIN", "GMB", "GNB", "LBR", "MLI", "MRT",
                           "NER", "NGA", "SEN", "SLE", "TGO")
no_data_countries <- c("GMB", "TGO")

level_0 <- read_rds(here("data_download", "admin_spatial", "level_0_admin.rds"))

level_1 <- read_rds(here("data_download", "admin_spatial", "level_1_admin.rds"))

level_2 <- read_rds(here("data_download", "admin_spatial", "level_2_admin.rds"))

non_trapped <- read_rds(here("data_download", "admin_spatial", "level_2_TGOGMB.rds"))

level_2_all <- bind_rows(level_2, non_trapped)

included_countries <- level_0 %>%
  filter(GID_0 %in% continental_countries)

contiguous_boundary <- included_countries %>%
  filter(!GID_0 == "CPV") %>%
  st_union()

studies <- read_rds(here("data_clean", "studies.rds"))

rodent_spatial <- read_rds(here("data_clean", "rodent_spatial.rds")) %>%
  dplyr::select(-trap_nights)

imputed_tn <- read_rds(here("data_clean", "imputed_trap_nights.rds"))

rodent_spatial <- rodent_spatial %>%
  left_join(., imputed_tn,
            by = c("unique_id", "year_trapping", "month_trapping",
                   "region", "town_village", "habitat"))

bbox_rodent <- st_bbox(rodent_spatial)

# IUCN data
# As some of these ranges cross the dateline we use S1 methods
sf::sf_use_s2(FALSE)


rodent_iucn <- st_read(here("data_download", "iucn_data", "data_0.shp")) %>%
  bind_rows(st_read(here("data_download", "iucn_data", "data_1.shp"))) %>%
  mutate(classification = str_to_lower(BINOMIAL)) %>%
  select(classification, geometry) %>%
  filter(classification %in% rodent_spatial$classification) %>%
  group_by(classification) %>%
  summarise(geometry = st_union(geometry))

# GBIF data
# We limit the figures to the top 7 most commonly trapped species

species_names <- rodent_spatial %>%
  filter(!str_detect(classification, ".sp|.spp")) %>%
  distinct(classification, geometry) %>%
  tibble() %>%
  group_by(classification) %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  head(7) %>%
  pull(classification)

if(!file.exists(here("data_clean", "rodent_gbif_spatial.rds"))) {
  rodent_gbif <- list()

  for(i in 1:length(species_names)) {

    rodent_gbif[[i]] <- read_tsv(here("data_download", "gbif_species", paste0(gsub(" ", "_", species_names[[i]]), "_gbif.csv"))) %>%
      select(gbifID, species, countryCode, decimalLatitude, decimalLongitude)

  }

  names(rodent_gbif) <- c(species_names)

  rodent_gbif <- lapply(rodent_gbif, function(x) {

    x %>%
      drop_na(decimalLongitude, decimalLatitude) %>%
      st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = crs(included_countries)) %>%
      mutate(classification = str_to_lower(species)) %>%
      distinct(classification, geometry) %>%
      st_join(included_countries, st_within) %>%
      filter(!is.na(GID_0))

  })

  write_rds(rodent_gbif, here("data_clean", "rodent_gbif_spatial.rds"))

} else {

  rodent_gbif <- read_rds(here("data_clean", "rodent_gbif_spatial.rds"))

}


# Analysis of distribution coverage ---------------------------------------

analysis_proportion <- function(species_name, iucn = rodent_iucn, gbif = rodent_gbif, rodents = rodent_spatial) {

  iucn <- iucn %>%
    filter(classification == species_name) %>%
    st_intersection(contiguous_boundary) %>%
    st_union()

  gbif <- gbif[[species_name]] %>%
    select(classification, geometry)

  level_2_gbif <- st_join(level_2_all, gbif, st_contains) %>%
    drop_na(classification) %>%
    distinct(classification, geometry) %>%
    summarise(geometry = st_union(geometry))

  level_2_gbif_within_iucn <- level_2_gbif %>%
    st_intersection(iucn)

  rodents <- rodents %>%
    filter(classification == species_name) %>%
    select(classification, number, geometry) %>%
    mutate(pres_abs = case_when(number == 0 ~ factor("Non-detection", levels = c("Detection", "Non-detection")),
                                TRUE ~ factor("Detection", levels = c("Detection", "Non-detection")))) %>%
    st_intersection(contiguous_boundary) %>%
    distinct(classification, pres_abs, geometry)

  level_2_rodents <- st_join(level_2_all, rodents, st_contains) %>%
    drop_na(classification) %>%
    distinct(GID_2, classification, pres_abs, geometry) %>%
    arrange(GID_2, pres_abs) %>%
    group_by(GID_2) %>%
    slice(1) %>%
    group_by(pres_abs) %>%
    summarise(geometry = st_union(geometry))

  level_2_rodent_presence <- level_2_rodents %>%
    filter(pres_abs == "Detection")

  level_2_rodent_presence_within_iucn <- level_2_rodents %>%
    filter(pres_abs == "Detection") %>%
    st_intersection(iucn)

  level_2_rodent_absence <- level_2_rodents %>%
    filter(pres_abs == "Non-detection")

  level_2_rodent_absence_within_iucn <- level_2_rodents %>%
    filter(pres_abs == "Non-detection") %>%
    st_intersection(iucn)

  combined_gbif_rodent <- bind_rows(level_2_gbif_within_iucn, level_2_rodent_presence_within_iucn) %>%
    st_union()

  area_iucn <- set_units(st_area(iucn), km^2)

  if(length(st_area(iucn)) > 0) {

    area_gbif_in_iucn <- set_units(st_area(level_2_gbif_within_iucn), km^2)

    area_gbif_outside_iucn <- set_units(st_area(level_2_gbif), km^2) - area_gbif_in_iucn

    prop_gbif_coverage <- area_gbif_in_iucn/area_iucn

    area_trapping_in_iucn <- set_units(st_area(level_2_rodent_presence_within_iucn), km^2)

    area_trapping_outside_iucn <- set_units(st_area(level_2_rodent_presence), km^2) - area_trapping_in_iucn

    prop_trapping_coverage <- area_trapping_in_iucn/area_iucn

    area_combined_in_iucn <- set_units(st_area(combined_gbif_rodent), km^2)

    prop_combined_coverage <- area_combined_in_iucn/area_iucn

    area_non_detection_in_iucn <- set_units(st_area(level_2_rodent_absence_within_iucn), km^2)

    prop_non_detection <- area_non_detection_in_iucn/area_iucn

    results = tibble(species = str_to_sentence(species_name), range_area = area_iucn,
                     gbif_perc = prop_gbif_coverage * 100, gbif_outside_range = area_gbif_outside_iucn,
                     detection_perc = prop_trapping_coverage * 100, trapping_outside_range = area_trapping_outside_iucn,
                     combined_perc = prop_combined_coverage * 100,
                     non_detection_perc = prop_non_detection * 100)

  } else {

    area_gbif_outside_iucn <- set_units(st_area(level_2_gbif), km^2)

    area_trapping_outside_iucn <- set_units(st_area(level_2_rodent_presence), km^2)

    results = tibble(species = str_to_sentence(species_name), range_area = set_units(NA, km^2),
                     gbif_perc = set_units(NA, km^2), gbif_outside_range = area_gbif_outside_iucn,
                     detection_perc = set_units(NA, km^2), trapping_outside_range = area_trapping_outside_iucn,
                     combined_perc = set_units(NA, km^2),
                     non_detection_perc = set_units(NA, km^2))

  }

  return(results)
}

testing_coverage <- list()

for(i in 1:length(species_names)) {

  testing_coverage[[i]] <- analysis_proportion(species_name = species_names[[i]])

}

testing_coverage <- lapply(testing_coverage, function(x) x %>%
                             mutate(across(.cols = any_of(2:8), .fns = as.numeric)))
table_1 <-  bind_rows(testing_coverage) %>%
  group_by(species) %>%
  mutate(range_area = round(as.numeric(range_area)/1000, 0),
         gbif_perc = round(as.numeric(gbif_perc), 1),
         gbif_outside_range = round(as.numeric(gbif_outside_range)/1000, 0),
         detection_perc = round(as.numeric(detection_perc), 1),
         trapping_outside_range = round(as.numeric(trapping_outside_range)/1000, 0),
         non_detection_perc = round(as.numeric(non_detection_perc), 1),
         combined_perc = round(as.numeric(combined_perc), 0)) %>%
  select(species, range_area, gbif_perc, gbif_outside_range, detection_perc, trapping_outside_range, non_detection_perc, combined_perc)

Table_1 <- flextable(table_1) %>%
  bg(j = 2, bg = "grey", part = "all") %>%
  bg(j = 5:7, bg = "grey", part = "all") %>%
  set_header_labels(values = list(species = "Species",
                                  range_area = "Range \n(1,000 km_2_)",
                                  gbif_perc = "Range covered \n(%)",
                                  gbif_outside_range = "Area outside range \n(1,000 km_2_)",
                                  detection_perc = "Range covered \ndetection (%)",
                                  trapping_outside_range = "Area outside range \n(1,000 km_2_)",
                                  non_detection_perc = "Range covered \nnon-detection (%)",
                                  combined_perc = "Range covered \n(%)")) %>%
  italic(j = "species", italic = TRUE, part = "body") %>%
  compose(part = "header", j = 2, value = as_paragraph("Range \n (1,000 km", as_sup("2"), ")")) %>%
  compose(part = "header", j = 4, value = as_paragraph("Area outside range \n (1,000 km", as_sup("2"), ")")) %>%
  compose(part = "header", j = 6, value = as_paragraph("Area outside range \n (1,000 km", as_sup("2"), ")")) %>%
  add_header_row(top = TRUE, values = c("", "IUCN", "GBIF", "Trapping studies", "Combined"), colwidths = c(1, 1, 2, 3, 1)) %>%
  align(part = "header", align = "center")

write_rds(Table_1, here("tables", "Table_1_updated.rds"))
