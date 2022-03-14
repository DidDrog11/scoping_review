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

# Reference raster from pop_density

ref_rast <- rast(here("data_download", "pop_2005", "wa_pop_2005.tif")) %>%
  cellSize(unit = "km")

analysis_proportion_pixel <- function(species_name, trap_data = rodent_spatial, iucn_data = rodent_iucn, gbif_data = rodent_gbif) {

  trap_v <- rodent_spatial %>%
    filter(classification == species_name) %>%
    select(number, geometry) %>%
    mutate(pres_abs = case_when(number > 0 ~ 1,
                                TRUE ~ 0)) %>%
    select(pres_abs, geometry) %>%
    vect()

  trap_r_detection <- rasterize(subset(trap_v, trap_v$pres_abs == 1), ref_rast, fun = "max", field = "pres_abs")

  trap_r_non_detection <- rasterize(subset(trap_v, trap_v$pres_abs == 0), ref_rast, fun = "max", field = "pres_abs")

  iucn_v <- iucn_data %>%
    filter(classification == species_name) %>%
    mutate(pres_abs = 1) %>%
    st_intersection(contiguous_boundary) %>%
    select(pres_abs, geometry)

  gbif_v <- rodent_gbif[[species_name]] %>%
    mutate(pres_abs = 1) %>%
    select(pres_abs, geometry) %>%
    vect()

  gbif_r <- rasterize(gbif_v, ref_rast, fun = "max", field = "pres_abs")

  combined_v <- rbind(trap_v, gbif_v)

  combined_r <- rasterize(combined_v, ref_rast, fun = "max", field = "pres_abs")

  if(nrow(iucn_v) > 0) {

    iucn_v <- iucn_v %>%
      vect()

    iucn_r <- rasterize(iucn_v, ref_rast, fun = "max", field = "pres_abs")

    area_gbif_in_iucn <- mask(gbif_r, iucn_v) %>%
      expanse(unit = "km")

    area_gbif_outside_iucn <- mask(gbif_r, iucn_v, inverse = TRUE) %>%
      expanse(unit = "km")

    prop_gbif_coverage <- (mask(gbif_r, iucn_v) %>%
                             expanse(unit = "km")/expanse(iucn_r, unit = "km")) * 100

    area_trapping_detection_in_iucn <- mask(trap_r_detection, iucn_v) %>%
      expanse(unit = "km")

    prop_detection_coverage <- (mask(trap_r_detection, iucn_v) %>%
                                  expanse(unit = "km")/expanse(iucn_r, unit = "km")) * 100

    area_trapping_non_detection_in_iucn <- mask(trap_r_non_detection, iucn_v) %>%
      expanse(unit = "km")

    prop_non_detection_coverage <- (mask(trap_r_non_detection, iucn_v) %>%
                                      expanse(unit = "km")/expanse(iucn_r, unit = "km")) * 100

    area_detection_outside_iucn <- mask(trap_r_detection, iucn_v, inverse = TRUE) %>%
      expanse(unit = "km")

    area_combined_in_iucn <- mask(combined_r, iucn_v) %>%
      expanse(unit = "km")

    prop_combined_coverage <- (mask(combined_r, iucn_v) %>%
                                 expanse(unit = "km")/expanse(iucn_r, unit = "km")) * 100

    results = tibble(species = str_to_sentence(species_name), range_area = expanse(iucn_r, unit = "km")/1000,
                     gbif_perc = prop_gbif_coverage, gbif_outside_range = area_gbif_outside_iucn/1000,
                     detection_perc = prop_detection_coverage, trapping_outside_range = area_detection_outside_iucn/1000,
                     non_detection_perc = prop_non_detection_coverage,
                     combined_perc = prop_combined_coverage)

  } else {

    area_gbif_outside_iucn <- expanse(gbif_r, unit = "km")

    area_detection_outside_iucn <- expanse(trap_r_detection, unit = "km")

    results = tibble(species = str_to_sentence(species_name), range_area = NA,
                     gbif_perc = NA, gbif_outside_range = area_gbif_outside_iucn/1000,
                     detection_perc = NA, trapping_outside_range = area_detection_outside_iucn/1000,
                     combined_perc = NA,
                     non_detection_perc = NA)

  }

  return(results)
}

testing_coverage_pixel <- list()

for(i in 1:length(species_names)) {

  testing_coverage_pixel[[i]] <- analysis_proportion_pixel(species_name = species_names[[i]])

}

table_1_pixel <- bind_rows(testing_coverage_pixel) %>%
  group_by(species) %>%
  mutate(range_area = round(as.numeric(range_area), 0),
         gbif_perc = round(as.numeric(gbif_perc), 2),
         gbif_outside_range = round(as.numeric(gbif_outside_range), 2),
         detection_perc = round(as.numeric(detection_perc), 2),
         trapping_outside_range = round(as.numeric(trapping_outside_range), 2),
         non_detection_perc = round(as.numeric(non_detection_perc), 2),
         combined_perc = round(as.numeric(combined_perc), 2)) %>%
  select(species, range_area, gbif_perc, gbif_outside_range, detection_perc, trapping_outside_range, non_detection_perc, combined_perc)

Table_1_pixel <- flextable(table_1_pixel) %>%
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

write_rds(Table_1_pixel, here("tables", "Table_1_updated_pixel.rds"))
