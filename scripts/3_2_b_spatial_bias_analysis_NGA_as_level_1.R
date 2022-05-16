# Exploring effect of using Level 1 for NGA and GHA -----------------------
# Trial with NGA and GHA as level 1
trial_levels <- level_2 %>%
  filter(!str_detect(GID_0, "NGA|GHA")) %>%
  bind_rows(level_1 %>%
              filter(str_detect(GID_0, "NGA|GHA")))

sites_in_mixed <-  st_join(x = trapping_effort, y = trial_levels, join = st_within, left = FALSE)

trap_nights_region <- tibble(sites_in_mixed) %>%
  group_by(NAME_1, GID_1, NAME_2, GID_2, unique_id, year_trapping, month_trapping, region, town_village, habitat) %>%
  distinct(.keep_all = TRUE) %>%
  summarise(total_trap_nights = sum(trap_nights, na.rm = TRUE)) %>%
  group_by(GID_1, GID_2, NAME_2) %>%
  summarise(region_trap_nights = sum(total_trap_nights))

sites_in_mixed <- trial_levels %>%
  left_join(., trap_nights_region, by = c("GID_1", "GID_2", "NAME_2")) %>%
  mutate(region_trap_nights = case_when(is.na(region_trap_nights) ~ 0,
                                        TRUE ~ region_trap_nights))  %>%
  mutate(area_km2 = set_units(st_area(.), "km^2"),
         tn_density = region_trap_nights/as.numeric(area_km2),
         tn_density = ifelse(is.na(tn_density), NA, tn_density),
         ID = row_number())

human_pop <- rast(here("data_download", "pop_2005","pop_2005.tif"))

vect_sites <- vect(sites_in_mixed)

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

all_regions <- lapply(1:nrow(sites_in_mixed), function(x) crop(crop_habitat, sites_in_mixed[x,]))
all_regions_hab <- lapply(1:length(all_regions), function(x) as.data.frame(freq(all_regions[[x]])) %>%
                            select(-layer))
names(all_regions_hab) <- sites_in_mixed$ID

habitats <- as.data.frame(data.table::rbindlist(all_regions_hab, idcol = TRUE)) %>%
  rename("ID" = ".id") %>%
  mutate(ID = as.integer(ID))

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
  group_by(ID, habitat) %>%
  summarise(count = sum(count)) %>%
  mutate(percentage = count/sum(count) * 100) %>%
  filter(habitat %in% c("cropland", "tree_cover", "shrubland", "urban")) %>%
  select(ID, habitat, percentage) %>%
  pivot_wider(names_from = habitat, values_from = percentage) %>%
  replace_na(replace = list(cropland = 0, shrubland = 0, tree_cover = 0, urban = 0))

region_pop_hab_sf <- left_join(region_pop_sf, wa_perc_habitat) %>%
  mutate(area_km2 = as.numeric(area_km2))

tn_simple_model_trial <- bam(tn_density ~ s(x, y, bs = "ts"),
                             family = "tw",
                             data = region_pop_hab_sf,
                             select = TRUE)

tn_simple_model_k_trial <- bam(tn_density ~ s(x, y, bs = "ts", k = 40),
                               family = "tw",
                               data = region_pop_hab_sf,
                               select = TRUE)

tn_pop_habitat_model_1_trial <- bam(tn_density ~ s(pop_2005, k = 12) + s(area_km2, bs = "cs") + s(urban, bs = "cs") + s(x, y, k = 40),
                                    family = "tw",
                                    data = region_pop_hab_sf,
                                    select = TRUE)

test <- region_pop_hab_sf %>%
  select(ID, GID_0, GID_2, NAME_2, x, y, region_trap_nights, tn_density, area_km2, pop_2005, cropland, shrubland, tree_cover, urban) %>%
  mutate(null_model_trial = exp(as.numeric(predict(all_models$tn_simple_model_trial, newdata = region_pop_hab_sf))),
         null_increased_k_model_trial = exp(as.numeric(predict(all_models$tn_simple_model_k_trial, newdata = region_pop_hab_sf))),
         final_model = exp(as.numeric(predict(all_models$tn_pop_habitat_model_1_trial, newdata = region_pop_hab_sf)))) %>%
  pivot_longer(cols = contains("model"),
               values_to = "prediction",
               names_to = "model") %>%
  mutate(model = fct_inorder(model))

ggplot(test, aes(x = prediction, y = tn_density, group = model)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ model)
