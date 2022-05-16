source(here::here("scripts", "libraries.R"))

if(!file.exists(here("data_clean", "pop_habitat_tn_analysis.rds"))) {

  level_1 <- read_rds(here("data_download", "admin_spatial", "level_1_admin.rds"))
  level_2 <- read_rds(here("data_download", "admin_spatial", "level_2_admin.rds"))
  non_trapped <- read_rds(here("data_download", "admin_spatial", "level_2_TGOGMB.rds"))

  level_2 <- bind_rows(level_2, non_trapped) %>%
    filter(!str_detect(GID_0, "CPV"))

  t <- level_2 %>%
    slice_head(n = 10)
  border <- t %>%
    summarise(border = st_union(geometry)) %>%
    st_buffer(dist = 10000)

  # Trap night and population -----------------------------------------------

  # Load the cleaned rodent data, each line represents a species within a trap site within a study
  # Join this data to the cleaned habitats and imputed trap night data
  rodent_data <- read_rds(here("data_clean", "rodent_df.rds")) %>%
    tibble()  %>%
    dplyr::select(-all_of(c("intensity_use", "geometry", "trap_nights"))) %>%
    left_join(.,
              habitat_data ,
              by = "record_id") %>%
    left_join(., imputed_tn,
              by = c("unique_id", "year_trapping", "month_trapping",
                     "region", "town_village", "habitat")) %>%
    mutate(unique_id = as_factor(unique_id)) %>%
    drop_na(longitude, latitude) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(level_2))

  trapping_effort <- rodent_data %>%
    distinct(unique_id, year_trapping, month_trapping, country, region, town_village, habitat, trap_nights, trap_night_data, geometry)

  trapping_effort_sensitivity <- rodent_data %>%
    filter(trap_night_data != "Imputed") %>%
    distinct(unique_id, year_trapping, month_trapping, country, region, town_village, habitat, trap_nights, trap_night_data, geometry)

  sites_in_2 <-  st_join(x = trapping_effort, y = level_2, join = st_within, left = FALSE)

  sites_in_2_sensitivity <- st_join(x = trapping_effort_sensitivity, y = level_2, join = st_within, left = FALSE)

  trap_nights_region <- tibble(sites_in_2) %>%
    group_by(NAME_2, GID_2, unique_id, year_trapping, month_trapping, region, town_village, habitat) %>%
    distinct(.keep_all = TRUE) %>%
    summarise(total_trap_nights = sum(trap_nights, na.rm = TRUE)) %>%
    group_by(GID_2, NAME_2) %>%
    summarise(region_trap_nights = sum(total_trap_nights))

  trap_nights_region_sensitivity <- tibble(sites_in_2_sensitivity) %>%
    group_by(NAME_2, GID_2, unique_id, year_trapping, month_trapping, region, town_village, habitat) %>%
    distinct(.keep_all = TRUE) %>%
    summarise(total_trap_nights = sum(trap_nights)) %>%
    group_by(GID_2, NAME_2) %>%
    summarise(region_trap_nights = sum(total_trap_nights))

  sites_2 <- level_2 %>%
    left_join(., trap_nights_region, by = c("GID_2", "NAME_2")) %>%
    mutate(region_trap_nights = case_when(is.na(region_trap_nights) ~ 0,
                                          TRUE ~ region_trap_nights))  %>%
    mutate(area_km2 = set_units(st_area(.), "km^2"),
           tn_density = region_trap_nights/as.numeric(area_km2),
           tn_density = ifelse(is.na(tn_density), NA, tn_density))

  sites_2_sensitivity <- level_2 %>%
    left_join(., trap_nights_region_sensitivity, by = c("GID_2", "NAME_2")) %>%
    mutate(region_trap_nights = case_when(is.na(region_trap_nights) ~ 0,
                                          TRUE ~ region_trap_nights))  %>%
    mutate(area_km2 = set_units(st_area(.), "km^2"),
           tn_density = region_trap_nights/as.numeric(area_km2),
           tn_density = ifelse(is.na(tn_density), NA, tn_density))

  write_rds(sites_2, here("data_clean", "traps_level_2.rds"))
  write_rds(sites_2_sensitivity, here("data_clean", "traps_level_2_sensitivity.rds"))

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

  region_pop_hab_sf <- left_join(region_pop_sf, wa_perc_habitat) %>%
    mutate(area_km2 = as.numeric(area_km2))

  region_pop_hab_sf_sens <- region_pop_hab_sf %>%
    select(-tn_density) %>%
    add_column(tn_density = sites_2_sensitivity$tn_density)

  write_rds(region_pop_hab_sf, here("data_clean", "pop_habitat_tn_analysis.rds"))
  write_rds(region_pop_hab_sf_sens, here("data_clean", "pop_habitat_tn_analysis_sensitivity.rds"))

} else {
  region_pop_hab_sf <- read_rds(here("data_clean", "pop_habitat_tn_analysis.rds"))
  region_pop_hab_sf_sens <- read_rds(here("data_clean", "pop_habitat_tn_analysis_sensitivity.rds"))
}

if(!file.exists(here("models", "tn_simple_model.rds"))) {

  # Check the distribution of the trap night response variable to aid model family choice

  dir.create(here("models"))

  tn_simple_model <- bam(tn_density ~ s(x, y, bs = "ts"),
                         family = "tw",
                         data = region_pop_hab_sf,
                         select = TRUE)

  tn_log_model <- bam(log_tn ~ s(x, y, bs = "ts"),
                      family = nb(link = "log"),
                      data = b,
                      select = TRUE)

  tn_simple_model_k <- bam(tn_density ~ s(x, y, bs = "ts", k = 60),
                           family = "tw",
                           data = region_pop_hab_sf %>% filter(GID_0 != "CPV"),
                           select = TRUE)

  tn_pop_model <- bam(tn_density ~ s(pop_2005, bs = "cs") + s(x, y, bs = "ts", k = 40),
                      family = "tw",
                      select = TRUE,
                      data = region_pop_hab_sf %>% filter(GID_0 != "CPV"))

  tn_pop_model_2 <- bam(tn_density ~ s(pop_2005, k = 12, bs = "cs") + s(x, y, k = 40, bs = "ts"),
                        select = TRUE,
                        family = "tw",
                        data = region_pop_hab_sf %>% filter(GID_0 != "CPV"))

  tn_pop_model_3 <- bam(tn_density ~ s(pop_2005, k = 12, bs = "cs") + s(area_km2, bs = "cs") + s(x, y, k = 40, bs = "ts"),
                        select = TRUE,
                        family = "tw",
                        data = region_pop_hab_sf %>% filter(GID_0 != "CPV"))

  tn_habitat_model <- bam(tn_density ~ s(cropland) + s(shrubland) + s(tree_cover) +s(urban) + s(x, y, k = 40),
                          family = "tw",
                          data = region_pop_hab_sf %>% filter(GID_0 != "CPV"))


  tn_pop_habitat_model <- bam(tn_density ~ s(pop_2005, k = 12) + s(cropland) + s(x, y, k = 40),
                              family = "tw",
                              data = region_pop_hab_sf %>% filter(GID_0 != "CPV"))


  tn_habitat_model_2 <- bam(tn_density ~ s(cropland) + s(tree_cover) +s(urban) + s(x, y, k = 540),
                            family = "tw",
                            data = region_pop_hab_sf %>% filter(GID_0 != "CPV"))

  tn_habitat_model_3 <- bam(tn_density ~ s(tree_cover, bs = "cs") + s(urban, bs = "cs") + s(x, y, bs = "ts"),
                            family = "tw",
                            data = region_pop_hab_sf %>% filter(GID_0 != "CPV"),
                            select = TRUE)

  write_rds(tn_simple_model, here("models", "tn_simple_model.rds"))
  write_rds(tn_pop_model, here("models", "tn_pop_model.rds"))
  write_rds(tn_pop_habitat_model, here("models", "tn_pop_habitat_model.rds"))
  write_rds(tn_habitat_model, here("models", "tn_habitat_model.rds"))
  write_rds(tn_habitat_model_2, here("models", "tn_habitat_model_2.rds"))
  write_rds(tn_habitat_model_3, here("models", "tn_habitat_model_3.rds"))

} else {

  tn_simple_model <- read_rds(here("models", "tn_simple_model.rds"))
  tn_pop_model <- read_rds(here("models", "tn_pop_model.rds"))
  tn_pop_habitat_model <- read_rds(here("models", "tn_pop_habitat_model.rds"))
  tn_habitat_model <- read_rds(here("models", "tn_habitat_model.rds"))
  tn_habitat_model_2 <- read_rds(here("models", "tn_habitat_model_2.rds"))
  tn_habitat_model_3 <- read_rds(here("models", "tn_habitat_model_3.rds"))

}

all_models <- list(null_model = tn_simple_model,
                   null_increased_k = tn_simple_model_k,
                   pop_model = tn_pop_model,
                   pop_model_increased_k = tn_pop_model_2,
                   pop_area_model = tn_pop_model_3)
summary(tn_simple_model)
AIC(tn_simple_model)
model_1_areal <- getViz(tn_simple_model)
check(model_1_areal)
plot(model_1_areal, all.terms = TRUE)

summary(tn_pop_model)
AIC(tn_pop_model)
model_2_areal <- getViz(tn_pop_model)
check(model_2_areal)
plot(model_2_areal, all.terms = TRUE)

summary(tn_pop_model_3)
AIC(tn_pop_model_3)
model_3_areal <- getViz(tn_pop_model_3)
check(model_3_areal)
plot(model_3_areal, all.terms = TRUE)

summary(tn_pop_habitat_model)
AIC(tn_pop_habitat_model)
model_3_areal <- getViz(tn_pop_habitat_model)
check(model_3_areal)
plot(model_3_areal, all.terms = TRUE)

summary(tn_habitat_model)
AIC(tn_habitat_model)
model_4_areal <- getViz(tn_habitat_model)
check(model_4_areal)
plot(model_4_areal, all.terms = TRUE)

summary(tn_habitat_model_2)
AIC(tn_habitat_model_2)
model_5_areal <- getViz(tn_habitat_model_2)
check(model_5_areal)
plot(model_5_areal, all.terms = TRUE)

summary(tn_habitat_model_3)
AIC(tn_habitat_model_3)
model_6_areal <- getViz(tn_habitat_model_3)
check(model_6_areal)
plot(model_6_areal, all.terms = TRUE)

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
