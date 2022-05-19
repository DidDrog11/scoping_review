# Load data ---------------------------------------------------------------

contiguous_boundary <- read_rds(here("data_clean", "WA_continental_boundary.rds"))

rodent_spatial <- read_rds(here("data_clean", "rodent_spatial.rds")) %>%
  dplyr::select(-trap_nights)

tested_pcr <- tibble(read_rds(here("data_clean", "long_pathogen.rds"))) %>%
  select(-any_of(contains("habitat"))) %>%
  drop_na(geometry) %>%
  filter(!geometry %in% "c(NA, NA)") %>%
  pivot_wider(names_from = assay, values_from = number) %>%
  filter(!is.na(pcr_path_1_positive) | !is.na(pcr_path_2_positive) | !is.na(pcr_path_3_positive) |
           !is.na(pcr_path_4_positive) | !is.na(pcr_path_5_positive) | !is.na(pcr_path_6_positive)) %>%
  select(-any_of(c(contains("ab_ag"), contains("culture"), contains("histo")))) %>%
  mutate(record_id = row_number(),
         assay = "PCR") %>%
  group_by(record_id) %>% # use the following to coalesce row-wise
  mutate(tested = case_when(!is.na(pcr_path_1_positive) ~ path_1_tested,
                            !is.na(pcr_path_2_positive) ~ path_2_tested,
                            !is.na(pcr_path_3_positive) ~ path_3_tested,
                            !is.na(pcr_path_4_positive) ~ path_4_tested,
                            !is.na(pcr_path_5_positive) ~ path_5_tested,
                            !is.na(pcr_path_6_positive) ~ path_6_tested),
         positive = case_when(!is.na(path_1_tested) ~ pcr_path_1_positive,
                              !is.na(path_2_tested) ~ pcr_path_2_positive,
                              !is.na(path_3_tested) ~ pcr_path_3_positive,
                              !is.na(path_4_tested) ~ pcr_path_4_positive,
                              !is.na(path_5_tested) ~ pcr_path_5_positive,
                              !is.na(path_6_tested) ~ pcr_path_6_positive)) %>%
  select(-any_of(contains("path_"))) %>%
  filter(tested > 0) %>%
  st_as_sf(crs = crs(rodent_spatial))

tested_serology <- tibble(read_rds(here("data_clean", "long_pathogen.rds"))) %>%
  select(-any_of(contains("habitat"))) %>%
  drop_na(geometry) %>%
  filter(!geometry %in% "c(NA, NA)") %>%
  pivot_wider(names_from = assay, values_from = number) %>%
  filter(!is.na(ab_ag_path_1_positive) | !is.na(ab_ag_path_2_positive) | !is.na(ab_ag_path_3_positive) |
           !is.na(ab_ag_path_4_positive) | !is.na(ab_ag_path_5_positive)) %>%
  select(-any_of(c(contains("pcr"), contains("culture"), contains("histo")))) %>%
  mutate(record_id = row_number(),
         assay = "Serology") %>%
  group_by(record_id) %>%
  mutate(tested = case_when(!is.na(ab_ag_path_1_positive) ~ path_1_tested,
                            !is.na(ab_ag_path_2_positive) ~ path_2_tested,
                            !is.na(ab_ag_path_3_positive) ~ path_3_tested,
                            !is.na(ab_ag_path_4_positive) ~ path_4_tested,
                            !is.na(ab_ag_path_5_positive) ~ path_5_tested),
         positive = case_when(!is.na(path_1_tested) ~ ab_ag_path_1_positive,
                              !is.na(path_2_tested) ~ ab_ag_path_2_positive,
                              !is.na(path_3_tested) ~ ab_ag_path_3_positive,
                              !is.na(path_4_tested) ~ ab_ag_path_4_positive,
                              !is.na(path_5_tested) ~ ab_ag_path_5_positive)) %>%
  select(-any_of(contains("path_"))) %>%
  filter(tested > 0) %>%
  st_as_sf(crs = crs(rodent_spatial))

tested_culture <- tibble(read_rds(here("data_clean", "long_pathogen.rds"))) %>%
  select(-any_of(contains("habitat"))) %>%
  drop_na(geometry) %>%
  pivot_wider(names_from = assay, values_from = number) %>%
  filter(!is.na(culture_path_1_positive)) %>%
  select(-any_of(c(contains("pcr"), contains("ab_ag"), contains("histo")))) %>%
  mutate(record_id = row_number(),
         assay = "Culture") %>%
  group_by(record_id) %>%
  mutate(tested = case_when(!is.na(culture_path_1_positive) ~ path_1_tested),
         positive = case_when(!is.na(path_1_tested) ~ culture_path_1_positive)) %>%
  select(-any_of(contains("path_"))) %>%
  filter(tested > 0) %>%
  st_as_sf(crs = crs(rodent_spatial))

tested_histo <- tibble(read_rds(here("data_clean", "long_pathogen.rds"))) %>%
  select(-any_of(contains("habitat"))) %>%
  drop_na(geometry) %>%
  pivot_wider(names_from = assay, values_from = number) %>%
  filter(!is.na(histo_path_1_positive) | !is.na(histo_path_2_positive) | !is.na(histo_path_3_positive) |
           !is.na(histo_path_4_positive) | !is.na(histo_path_5_positive) | !is.na(histo_path_6_positive)) %>%
  select(-any_of(c(contains("pcr"), contains("culture"), contains("ab_ag")))) %>%
  mutate(record_id = row_number(),
         assay = "Histology") %>%
  group_by(record_id) %>%
  mutate(tested = case_when(!is.na(histo_path_1_positive) ~ path_1_tested,
                            !is.na(histo_path_2_positive) ~ path_2_tested,
                            !is.na(histo_path_3_positive) ~ path_3_tested,
                            !is.na(histo_path_4_positive) ~ path_4_tested,
                            !is.na(histo_path_5_positive) ~ path_5_tested,
                            !is.na(histo_path_6_positive) ~ path_6_tested),
         positive = case_when(!is.na(path_1_tested) ~ histo_path_1_positive,
                              !is.na(path_2_tested) ~ histo_path_2_positive,
                              !is.na(path_3_tested) ~ histo_path_3_positive,
                              !is.na(path_4_tested) ~ histo_path_4_positive,
                              !is.na(path_5_tested) ~ histo_path_5_positive,
                              !is.na(path_6_tested) ~ histo_path_6_positive)) %>%
  select(-any_of(contains("path_"))) %>%
  filter(tested > 0) %>%
  st_as_sf(crs = crs(rodent_spatial))

pathogen <- bind_rows(tested_pcr, tested_serology, tested_culture, tested_histo) %>%
  mutate(class = case_when(assay != "Serology" ~ "Acute infection",
                           TRUE ~ "Serology"),
         record_ID = row_number()) %>%
  distinct(unique_id, year_trapping, month, country, iso3c, region, town_village, classification, geometry, pathogen_tested, tested, positive, class) %>%
  st_join(., contiguous_boundary, join = st_within, left = FALSE)

# We harmonise pathogen family names and define harmonised names for pathogens tested
pathogen_family <- c("Arenaviridae", "Borreliaceae", "Flaviviridae", "Hantaviridae",
                     "Phenuiviridae", "Phenuiviridae", "Trypanosomatidae", "Arenaviridae",
                     "Trypanosomatidae", "Anaplasmataceae", "Bartonellaceae", "Taeniidae",
                     "Leptospiraceae", "Plagiorchiidae", "Schistosomatidae", "Sarcocystidae",
                     "Trichuridae", "Flaviviridae", "Poxviridae", "Enterobacteriaceae",
                     "Enterobacteriaceae", "Mycobacteriaceae", "Coxiellaceae", "Ehrlichiaceae",
                     "Arenaviridae", "Mycoplasmataceae", "Rickettsiaceae", "Rickettsiaceae",
                     "Babesiidae", "Eimeriidae", "Plasmodiidae", "Strongyloididae")
names(pathogen_family) <- unique(pathogen$pathogen_tested)
pathogen_clean <- c("Arenaviridae sp.", "Borrelia sp.", "Flavivirus sp.", "Hantavirus sp.",
                    "Phlebovirus sp.", "Rift valley fever phlebovirus", "Trypansoma sp.", "Lassa mammarenavirus",
                    "Leishmania sp.", "Anaplasma sp.", "Bartonella sp.", "Hydatigera sp.", "Leptospira sp.",
                    "Plagiorchis sp.", "Schistosoma sp.", "Toxoplasma gondii", "Trichuria sp.", "Usutu virus",
                    "Orthopoxvirus sp.", "Escherichia coli", "Klebsiella pneumoniae", "Mycobacteria sp.", "Coxiella burnetii",
                    "Erhlichia sp.", "Mammarenavirus sp.", "Mycoplasma sp.", "Orentia sp.", "Ricketsia sp.", "Babesia sp.",
                    "Eimeria sp.", "Plasmodium sp.", "Strongyloides sp.")
names(pathogen_clean) <- unique(pathogen$pathogen_tested)

cleaned_pathogen <- pathogen %>%
  mutate(pathogen_family = recode_factor(pathogen_tested, !!!pathogen_family),
         pathogen_tested = recode_factor(pathogen_tested, !!!pathogen_clean)) %>%
  arrange(pathogen_family, pathogen_tested)

# Load in the raster and aggregate to ~20km cell sizes
ref_rast <- rast(here("data_download", "habitat_2005", "wa_hab_2005.tif")) %>%
  mask(., contiguous_boundary_v) %>%
  aggregate(fact = 15) %>%
  cellSize(unit = "km")

analysis_proportion_pixel <- function(species_name, trap_data = rodent_spatial, iucn_data = rodent_iucn, gbif_data = rodent_gbif) {

  # Produce a vector of detections and non-detections for each species
  trap_v <- rodent_spatial %>%
    filter(classification == species_name) %>%
    select(number, geometry) %>%
    mutate(pres_abs = case_when(number > 0 ~ 1,
                                TRUE ~ 0)) %>%
    select(pres_abs, geometry) %>%
    vect()

  # Convert this into a raster using the ref_rast with cell sizes of ~ 20km
  trap_r_detection <- rasterize(subset(trap_v, trap_v$pres_abs == 1), ref_rast, fun = "max", field = "pres_abs")

  trap_r_non_detection <- rasterize(subset(trap_v, trap_v$pres_abs == 0), ref_rast, fun = "max", field = "pres_abs")

  # Convert the IUCN data to a vector
  iucn_v <- iucn_data %>%
    filter(classification == species_name) %>%
    mutate(pres_abs = 1) %>%
    st_intersection(contiguous_boundary) %>%
    select(pres_abs, geometry)

  # Convert the GBIF data to a vector
  gbif_v <- rodent_gbif[[species_name]] %>%
    mutate(pres_abs = 1) %>%
    select(pres_abs, geometry) %>%
    vect()

  gbif_r <- rasterize(gbif_v, ref_rast, fun = "max", field = "pres_abs")

  # Combine both sources of presence data as vector and raster
  combined_v <- rbind(trap_v, gbif_v)

  combined_r <- rasterize(combined_v, ref_rast, fun = "max", field = "pres_abs")

  combined_det_r <- rbind(subset(trap_v , trap_v$pres_abs == 1), gbif_v) %>%
    rasterize(., ref_rast, fun = "max", field = "pres_abs")

  # As Mus has no range we use this to separate out their analysis
  if(nrow(iucn_v) > 0) {

    iucn_v <- iucn_v %>%
      vect()

    # Turn IUCN polygon into raster
    iucn_r <- rasterize(iucn_v, ref_rast, fun = "max", field = "pres_abs")

    # Retain the GBIF presence within the IUCN range and calculate the area of the cells
    area_gbif_in_iucn <- mask(gbif_r, iucn_v) %>%
      expanse(unit = "km")

    # Retain the GBIF presence outside the IUCN range and calculate the area of the cells
    area_gbif_outside_iucn <- mask(gbif_r, iucn_v, inverse = TRUE) %>%
      expanse(unit = "km")

    # Calculate the proportion of GBIF coverage within the entire IUCN range
    prop_gbif_coverage <- round((mask(gbif_r, iucn_v) %>%
                                   expanse(unit = "km")/expanse(iucn_r, unit = "km")) * 100, 2)

    # Repeat the same analysis steps for the trapping data
    area_trapping_detection_in_iucn <- mask(trap_r_detection, iucn_v) %>%
      expanse(unit = "km")

    prop_detection_coverage <- round((mask(trap_r_detection, iucn_v) %>%
                                        expanse(unit = "km")/expanse(iucn_r, unit = "km")) * 100, 2)

    area_detection_outside_iucn <- mask(trap_r_detection, iucn_v, inverse = TRUE) %>%
      expanse(unit = "km")

    # Additionally for trapping data we can calculate non-detection
    area_trapping_non_detection_in_iucn <- mask(trap_r_non_detection, iucn_v) %>%
      expanse(unit = "km")

    prop_non_detection_coverage <- round((mask(trap_r_non_detection, iucn_v) %>%
                                            expanse(unit = "km")/expanse(iucn_r, unit = "km")) * 100, 2)

    # Now repeat for a combined raster of both GBIF and trapping
    area_combined_in_iucn <- mask(combined_r, iucn_v) %>%
      expanse(unit = "km")

    prop_combined_coverage <- round((mask(combined_det_r, iucn_v) %>%
                                       expanse(unit = "km")/expanse(iucn_r, unit = "km")) * 100, 2)

    # These values can then be used to populate a table for each species
    # Values are divided by 1,000
    results = tibble(species = str_to_sentence(species_name),
                     range_area = round(expanse(iucn_r, unit = "km")/1000, 2),
                     gbif_detection_range = paste(round(area_gbif_in_iucn/1000, 2), paste0("(", prop_gbif_coverage, "%)")),
                     gbif_outside_range = round(area_gbif_outside_iucn/1000, 2),
                     detection_range = paste(round(area_trapping_detection_in_iucn/1000, 2), paste0("(", prop_detection_coverage, "%)")),
                     trapping_outside_range = round(area_detection_outside_iucn/1000, 2),
                     non_detection_range = paste(round(area_trapping_non_detection_in_iucn/1000, 2), paste0("(", prop_non_detection_coverage, "%)")),
                     combined_range = paste(round(area_combined_in_iucn/1000, 2), paste0("(", prop_combined_coverage, "%)")))

  } else {

    area_gbif_outside_iucn <- expanse(gbif_r, unit = "km")

    area_detection_outside_iucn <- expanse(trap_r_detection, unit = "km")

    area_combined <- expanse(combined_det_r, unit = "km")

    results = tibble(species = str_to_sentence(species_name),
                     range_area = as.numeric(NA),
                     gbif_detection_range = as.character(NA),
                     gbif_outside_range = round(area_gbif_outside_iucn/1000, 2),
                     detection_range = as.character(NA),
                     trapping_outside_range = round(area_detection_outside_iucn/1000, 2),
                     non_detection_range = as.character(NA),
                     combined_range = as.character(round(area_combined/1000, 2)))

  }

  return(results)
}

testing_coverage_pixel <- lapply(species_names, analysis_proportion_pixel)

table_1_pixel <- bind_rows(testing_coverage_pixel)

Table_1_pixel <- flextable(table_1_pixel) %>%
  bg(j = 2, bg = "grey", part = "all") %>%
  bg(j = 5:7, bg = "grey", part = "all") %>%
  set_header_labels(values = list(species = "Species",
                                  range_area = "Range \n(1,000 km_2_)",
                                  gbif_detection_range = "Area inside range \n(1,000 km_2_) \n(% of IUCN)",
                                  gbif_outside_range = "Area outside range \n(1,000 km_2_)",
                                  detection_range = "Detection area \ninside range (1,000 km_2_) \n(% of IUCN)",
                                  trapping_outside_range = "Area outside range \n(1,000 km_2_)",
                                  non_detection_range = "Non-detection area \ninside range \n(1,000 km_2_) \n(% of IUCN)",
                                  combined_range = "Detection area \ninside range \n(1,000 km_2_) \n(% of IUCN)")) %>%
  italic(j = "species", italic = TRUE, part = "body") %>%
  compose(part = "header", j = 2, value = as_paragraph("Range \n (1,000 km", as_sup("2"), ")")) %>%
  compose(part = "header", j = 3, value = as_paragraph("Area inside range (1,000 km", as_sup("2"), ") (% of IUCN)")) %>%
  compose(part = "header", j = 4, value = as_paragraph("Area outside range \n (1,000 km", as_sup("2"), ")")) %>%
  compose(part = "header", j = 5, value = as_paragraph("Detection area \ninside range (1,000 km", as_sup("2"), ") (% of IUCN)")) %>%
  compose(part = "header", j = 6, value = as_paragraph("Area outside range \n (1,000 km", as_sup("2"), ") (% of IUCN)")) %>%
  compose(part = "header", j = 7, value = as_paragraph("Non-detection area \ninside range (1,000 km", as_sup("2"), ") (% of IUCN)")) %>%
  compose(part = "header", j = 8, value = as_paragraph("Detection area \ninside range (1,000 km", as_sup("2"), ") (% of IUCN)")) %>%
  add_header_row(top = TRUE, values = c("", "IUCN", "GBIF", "Trapping studies", "Combined"), colwidths = c(1, 1, 2, 3, 1)) %>%
  align(part = "all", align = "center")

# Effect of combining curated and rodent trapping
combined <- tibble(species = species_names,
                   gbif = c(0.21, 0.26, 0.12, NA, 0.09, 0.15, 0.2),
                   combined = c(0.32, 0.48, 0.2, NA, 0.2, 0.22, 0.23),
                   diff_per = combined/gbif)

mean(combined$diff_per, na.rm = TRUE)
sd(combined$diff_per, na.rm = TRUE)

# Proportion of range non-detection occurred in
non_detection <- tibble(species = species_names,
                        non_det_per = c(0.09, 0.13, 0.11, NA, 0.17, 0.1, 0.11))

mean(non_detection$non_det_per, na.rm = TRUE)
sd(non_detection$non_det_per, na.rm = TRUE)

write_rds(Table_1_pixel, here("tables", "Table_1_updated_pixel.rds"))
