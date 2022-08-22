# Data --------------------------------------------------------------------

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

included_countries <- level_0 %>%
  filter(GID_0 %in% continental_countries)

contiguous_boundary <- included_countries %>%
  filter(!GID_0 == "CPV") %>%
  summarise()

write_rds(contiguous_boundary, here("data_clean", "WA_continental_boundary.rds"))

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

write_rds(rodent_iucn, here("data_clean", "rodent_iucn.rds"))

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


# Figure 3 ----------------------------------------------------------------

plot_fig_3 <- function(species_name, iucn = rodent_iucn, gbif = rodent_gbif, rodents = rodent_spatial) {

  gbif <- gbif[[species_name]] %>%
    select(classification, geometry)

  iucn <- iucn %>%
    filter(classification == species_name) %>%
    st_intersection(contiguous_boundary) %>%
    st_union()

  rodents <- rodents %>%
    filter(classification == species_name) %>%
    select(classification, number, geometry) %>%
    mutate(pres_abs = case_when(number == 0 ~ factor("Non-detection", levels = c("Detection", "Non-detection")),
                                TRUE ~ factor("Detection", levels = c("Detection", "Non-detection")))) %>%
    st_intersection(., contiguous_boundary) %>%
    distinct(classification, pres_abs, geometry)

  rodent_presence <- rodents %>%
    filter(pres_abs == "Detection")

  rodent_absence <- rodents %>%
    filter(pres_abs == "Non-detection") %>%
    filter(!geometry %in% rodent_presence$geometry)

  rodent_combined <- bind_rows(rodent_presence, rodent_absence)

  iucn_gbif_plot <- ggplot() +
    geom_sf(data = included_countries, fill = NA) +
    geom_sf(data = iucn, fill = "#C12D20", alpha = 0.1) +
    geom_sf(data = gbif, size = 1) +
    theme_minimal() +
    labs(title = element_blank(),
         x = element_blank(),
         y = element_blank()) +
    annotation_north_arrow(height = unit(1, "cm"),
                           style = north_arrow_minimal(text_size = 8),
                           pad_x = unit(0.1, "cm"),
                           pad_y = unit(0.1, "cm")) +
    annotation_scale(height = unit(0.1, "cm"),
                     location = "tr")

  studies_plot <- ggplot() +
    geom_sf(data = included_countries, fill = NA) +
    geom_sf(data = iucn, fill = "#C12D20", alpha = 0.1) +
    geom_sf(data = rodent_combined %>%
              filter(pres_abs != "Detection"), aes(colour = pres_abs), size = 1) +
    geom_sf(data = rodent_combined %>%
              filter(pres_abs == "Detection"), aes(colour = pres_abs), size = 1) +
    scale_colour_manual(values = c("#440154", "#ff8c00")) +
    theme_minimal() +
    labs(title = element_blank(),
         x = element_blank(),
         y = element_blank(),
         colour = "Detection/Non-detection") +
    annotation_north_arrow(height = unit(1, "cm"),
                           style = north_arrow_minimal(text_size = 8),
                           pad_x = unit(0.1, "cm"),
                           pad_y = unit(0.1, "cm")) +
    annotation_scale(height = unit(0.1, "cm"),
                     location = "tr")

  combined_plot <- plot_grid(plotlist = list(iucn_gbif_plot, studies_plot + theme(legend.position = "none")), align = "h", nrow = 1)

  legend <- get_legend(studies_plot +
                         guides(colour = guide_legend(override.aes = list(nrow = 1, size = 3))
                                ) +
                         theme(legend.position = "bottom",
                               legend.text = element_text(size = 12),
                               legend.title = element_text(size = 14)))

  return(list(combined_plot = combined_plot,
              legend = legend))
}

produced_plots <- list()
legend <- list()

for(i in 1:length(species_names)) {

  output <- plot_fig_3(species_names[[i]])

  produced_plots[[i]] <- output[["combined_plot"]]

  legend <- output[["legend"]]

}

names(produced_plots) <- species_names

test_plots_a <- plot_grid(produced_plots[[1]], produced_plots[[2]], produced_plots[[3]], produced_plots[[4]],
                          legend, ncol = 1, labels = c("Mastomys natalensis", "Rattus rattus", "Mastomys erythroleucus", "Mus musculus", ""),
                          hjust = c(-0.2, -0.32, -0.17, -0.29), label_size = 12, rel_heights = c(1, 1, 1, 1, 0.6))
test_plots_b <- plot_grid(produced_plots[[5]], produced_plots[[6]], produced_plots[[7]],
                          legend, ncol = 1, labels = c("Arvicanthis niloticus", "Praomys daltoni", "Cricetomys gambianus", ""),
                          hjust = c(-0.2, -0.25, -0.165), label_size = 12, rel_heights = c(1, 1, 1, 0.6))

save_plot(plot = test_plots_a, filename = here("figures", "Figure_3_a_updated.png"), base_height = 12, base_width = 8)
save_plot(plot = test_plots_b, filename = here("figures", "Figure_3_b_updated.png"), base_height = 12, base_width = 8)


# Proportion of range trapped ---------------------------------------------
# Reference raster from pop_density

contiguous_boundary_v <- vect(contiguous_boundary)

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
                     range_area = round(expanse(iucn_r, unit = "km")/1000, 0),
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

write_rds(table_1_pixel, here("data_clean", "table_1_pixel.rds"))

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
