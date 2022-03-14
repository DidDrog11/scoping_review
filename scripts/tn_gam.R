source(here::here("scripts", "libraries.R"))

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

  studies <- read_rds(here("data_clean", "studies.rds"))

  zoonotic_studies <- studies %>%
    filter(str_detect(aim, "Zoonoses")) %>%
    pull(unique_id)

  trapping_effort <- rodent_spatial %>%
    filter(unique_id %in% zoonotic_studies) %>%
    distinct(unique_id, year_trapping, month_trapping, country, region, town_village, habitat, trap_nights, trap_night_data, geometry)

  trapping_effort_sensitivity <- rodent_spatial %>%
    filter(unique_id %in% zoonotic_studies) %>%
    filter(trap_night_data != "Imputed") %>%
    distinct(unique_id, year_trapping, month_trapping, country, region, town_village, habitat, trap_nights, trap_night_data, geometry)

  sites_2 <- st_intersection(x = level_2, y = trapping_effort)

  sites_2_sensitivity <- st_intersection(x = level_2, y = trapping_effort_sensitivity)

  trap_nights_region <- tibble(sites_2) %>%
    group_by(NAME_2, GID_2, unique_id, year_trapping, month_trapping, region, town_village, habitat) %>%
    summarise(total_trap_nights = sum(trap_nights)) %>%
    group_by(GID_2, NAME_2) %>%
    summarise(region_trap_nights = sum(total_trap_nights))

  trap_nights_region_sensitivity <- tibble(sites_2_sensitivity) %>%
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

  sites_2_sensitivity <- level_2 %>%
    left_join(., trap_nights_region_sensitivity, by = c("GID_2", "NAME_2")) %>%
    mutate(region_trap_nights = case_when(is.na(region_trap_nights) ~ 0,
                                          TRUE ~ region_trap_nights))  %>%
    mutate(area_m2 = st_area(.),
           tn_density = region_trap_nights/(as.numeric(area_m2)/1000000),
           tn_density = ifelse(is.na(tn_density), NA, tn_density))

  write_rds(sites_2, here("data_clean", "traps_level_2_zoonoses.rds"))
  write_rds(sites_2_sensitivity, here("data_clean", "traps_level_2_zoonoses_sensitivity.rds"))

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

  habitat_2005 <- rast(here("data_download", "habitat_2005", "habitat_2005.nc"))
  crop_habitat <- crop(habitat_2005[[1]], wa_pop)

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

  wa_perc_habitat <- habitats %>%
    mutate(habitat = recode(value, !!!land_type_classification)) %>%
    group_by(GID_2, habitat) %>%
    summarise(count = sum(count)) %>%
    mutate(percentage = count/sum(count) * 100) %>%
    filter(habitat %in% c("cropland", "tree_cover", "shrubland", "urban")) %>%
    select(GID_2, habitat, percentage) %>%
    pivot_wider(names_from = habitat, values_from = percentage) %>%
    replace_na(replace = list(cropland = 0, shrubland = 0, tree_cover = 0, urban = 0))

  region_pop_hab_sf <- left_join(region_pop_sf, wa_perc_habitat)

  region_pop_hab_sf_sens <- region_pop_hab_sf %>%
    select(-tn_density) %>%
    add_column(tn_density = sites_2_sensitivity$tn_density)

  write_rds(region_pop_hab_sf, here("data_clean", "pop_habitat_tn_analysis.rds"))
  write_rds(region_pop_hab_sf_sens, here("data_clean", "pop_habitat_tn_analysis_sensitivity.rds"))

} else {
  region_pop_hab_sf <- read_rds(here("data_clean", "pop_habitat_tn_analysis.rds"))
  region_pop_hab_sf_sens <- read_rds(here("data_clean", "pop_habitat_tn_analysis_sensitivity.rds"))
}

tn_simple_model <- gam(tn_density ~ s(x, y, k = 540),
                       family = "tw",
                       data = region_pop_hab_sf %>% filter(GID_0 != "CPV"))

tn_pop_model <- gam(tn_density ~ s(log(pop_2005), k = 9) + s(x, y, k = 540),
                    family = "tw",
                    data = region_pop_hab_sf %>% filter(GID_0 != "CPV"))

tn_pop_habitat_model <- gam(tn_density ~ s(log(pop_2005), k = 9) + s(cropland) + s(x, y, k = 540),
                            family = "tw",
                            data = region_pop_hab_sf %>% filter(GID_0 != "CPV"))

tn_habitat_model <- gam(tn_density ~ s(cropland) + s(shrubland) + s(tree_cover) +s(urban) + s(x, y, k = 540),
                            family = "tw",
                            data = region_pop_hab_sf %>% filter(GID_0 != "CPV"))

tn_habitat_model_2 <- gam(tn_density ~ s(cropland) + s(tree_cover) +s(urban) + s(x, y, k = 540),
                        family = "tw",
                        data = region_pop_hab_sf %>% filter(GID_0 != "CPV"))

tn_habitat_model_3 <- gam(tn_density ~ s(tree_cover, k = 15) + s(urban) + s(x, y, k = 540),
                          family = "tw",
                          data = region_pop_hab_sf %>% filter(GID_0 != "CPV"))

summary(tn_simple_model)
gam.check(tn_simple_model)
plot(tn_simple_model, all.terms = TRUE)

summary(tn_pop_model)
gam.check(tn_pop_model)
plot(tn_pop_model, all.terms = TRUE)

summary(tn_pop_habitat_model)
gam.check(tn_pop_habitat_model)
plot(tn_pop_habitat_model, all.terms = TRUE)

summary(tn_habitat_model)
gam.check(tn_habitat_model)
plot(tn_habitat_model, all.terms = TRUE)

summary(tn_habitat_model_2)
gam.check(tn_habitat_model_2)
plot(tn_habitat_model_2, all.terms = TRUE)

summary(tn_habitat_model_3)
gam.check(tn_habitat_model_3)
plot(tn_habitat_model_3, all.terms = TRUE)

as_flextable(tn_simple_model) %>%
  set_caption(caption = "Supplementary Table 3.1: GAM model Trap night density ~ Tweedie(coordinates)") %>%
  write_rds(here("tables", "supplementary_table_3_1.rds"))

as_flextable(tn_pop_model) %>%
  set_caption(caption = "Supplementary Table 3.2: GAM model Trap night density ~ Tweedie(log(population density) + coordinates)") %>%
  write_rds(here("tables", "supplementary_table_3_2.rds"))

as_flextable(tn_habitat_model) %>%
  set_caption(caption = "Supplementary Table 3.3: GAM model Trap night density ~ Tweedie(proportion cropland + proportion shrubland + proportion tree cover + proportion urban + coordinates)") %>%
  write_rds(here("tables", "supplementary_table_3_3.rds"))

as_flextable(tn_habitat_model_3) %>%
  set_caption(caption = "Supplementary Table 3.4: Final GAM model Trap night density ~ Tweedie(proportion tree cover + proportion urban + coordinates)") %>%
  write_rds(here("tables", "supplementary_table_3_4.rds"))

write_rds(tn_habitat_model_3, here("data_clean", "tn_final_model.rds"))

# Sensitivity analysis
tn_pop_model_sens <- gam(tn_density ~ s(x, y, k = 540) + s(log(pop_2005), k = 9),
                         family = "tw",
                         data = region_pop_hab_sf_sens %>% filter(GID_0 != "CPV"))

tn_pop_habitat_model_sens <- gam(tn_density ~ s(log(pop_2005), k = 9) + s(cropland) + s(tree_cover, k = 15) +s(urban) + s(x, y, k = 540),
                            family = "tw",
                            data = region_pop_hab_sf_sens %>% filter(GID_0 != "CPV"))

tn_habitat_model_3_sens <- gam(tn_density ~  s(tree_cover, k = 15) + s(urban) + s(x, y, k = 540),
                                 family = "tw",
                                 data = region_pop_hab_sf_sens %>% filter(GID_0 != "CPV"))

summary(tn_habitat_model_3_sens)
gam.check(tn_habitat_model_3_sens)
plot(tn_habitat_model_3_sens, all.terms = TRUE)

write_rds(tn_habitat_model_3_sens, here("data_clean", "tn_pop_habitat_model_sens.rds"))
