source(here::here("scripts", "libraries.R"))

if(!file.exists(here("models", "all_models.rds"))) {

  habitat_split <- c("habitat_1", "habitat_2", "habitat_3", "habitat_4", "habitat_5", "habitat_6", "habitat_7")
  habitat_data <- read_rds(here("data_clean", "habitat_types.rds")) %>%
    tibble() %>%
    dplyr::select(record_id, all_of(habitat_split), intensity_use)

  imputed_tn <- read_rds(here("data_clean", "imputed_trap_nights.rds"))

  level_1 <- read_rds(here("data_download", "admin_spatial", "level_1_admin.rds"))
  level_2 <- read_rds(here("data_download", "admin_spatial", "level_2_admin.rds"))
  non_trapped <- read_rds(here("data_download", "admin_spatial", "level_2_TGOGMB.rds"))

  level_2 <- bind_rows(level_2, non_trapped) %>%
    filter(!str_detect(GID_0, "CPV"))

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

  # Associated trap night density with administrative regions
  sites_in_2 <-  st_join(x = trapping_effort, y = level_2, join = st_within, left = FALSE)

  sites_in_2_sensitivity <- st_join(x = trapping_effort_sensitivity, y = level_2, join = st_within, left = FALSE)

  # Calculate the total number of traps within a region
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

  # Calculate the trap night density within a region
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

  # Save these
  write_rds(sites_2, here("data_clean", "traps_level_2.rds"))
  write_rds(sites_2_sensitivity, here("data_clean", "traps_level_2_sensitivity.rds"))

  # Load in the human population raster
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

  if(!file.exists(here("data_download", "habitat_2005", "wa_hab_2005.tif"))) {
  # Load in the habitat raster
  habitat_2005 <- rast(here("data_download", "habitat_2005", "habitat_2005.nc"))
  crop_habitat <- crop(habitat_2005[[1]], wa_pop)
  wa_hab <- writeRaster(crop_habitat, here("data_download", "habitat_2005", "wa_hab_2005.tif"))

  } else {

    wa_hab <- rast(here("data_download", "habitat_2005", "wa_hab_2005.tif"))

  }

  all_regions <- lapply(1:nrow(sites_2), function(x) crop(wa_hab, sites_2[x,]))
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

  # Calculate the percentage cover of each type of land use within a region
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

  # Run the models on the data with changes to number of knots of variables included
  dir.create(here("models"))

  tn_simple_model <- bam(tn_density ~ s(x, y, bs = "ts"),
                         family = "tw",
                         data = region_pop_hab_sf,
                         select = TRUE)

  tn_simple_model_k <- bam(tn_density ~ s(x, y, bs = "ts", k = 40),
                           family = "tw",
                           data = region_pop_hab_sf,
                           select = TRUE)

  tn_pop_model_1 <- bam(tn_density ~ s(pop_2005, bs = "cs") + s(x, y, bs = "ts", k = 40),
                        family = "tw",
                        select = TRUE,
                        data = region_pop_hab_sf)

  tn_pop_model_2 <- bam(tn_density ~ s(pop_2005, k = 12, bs = "cs") + s(x, y, k = 40, bs = "ts"),
                        select = TRUE,
                        family = "tw",
                        data = region_pop_hab_sf)

  tn_pop_model_3 <- bam(tn_density ~ s(pop_2005, k = 12, bs = "cs") + s(area_km2, bs = "cs") + s(x, y, k = 40, bs = "ts"),
                        select = TRUE,
                        family = "tw",
                        data = region_pop_hab_sf)

  tn_habitat_model_1 <- bam(tn_density ~ s(cropland, bs = "cs") + s(shrubland, bs = "cs") + s(tree_cover, bs = "cs") +s(urban, bs = "cs") + s(x, y, k = 20, bs = "ts"),
                            family = "tw",
                            data = region_pop_hab_sf,
                            select = TRUE)

  tn_habitat_model_2 <- bam(tn_density ~ s(shrubland, bs = "cs") + s(tree_cover, bs = "cs") +s(urban, bs = "cs") + s(x, y, k = 40),
                            family = "tw",
                            data = region_pop_hab_sf,
                            select = TRUE)

  tn_habitat_model_3 <- bam(tn_density ~ s(tree_cover, bs = "cs") + s(urban, bs = "cs") + s(x, y, bs = "ts", k = 40),
                            family = "tw",
                            data = region_pop_hab_sf,
                            select = TRUE)

  tn_pop_habitat_model_1 <- bam(tn_density ~ s(pop_2005, k = 12) + s(area_km2, bs = "cs") + s(urban, bs = "cs") + s(x, y, k = 40),
                                family = "tw",
                                data = region_pop_hab_sf,
                                select = TRUE)

  all_models <- list(null_model = tn_simple_model,
                     null_increased_k = tn_simple_model_k,
                     pop_model = tn_pop_model_1,
                     pop_model_increased_k = tn_pop_model_2,
                     pop_area_model = tn_pop_model_3,
                     all_hab_model = tn_habitat_model_1,
                     reduced_hab_model_1 = tn_habitat_model_2,
                     reduced_hab_model_2 = tn_habitat_model_3,
                     combined_model_1 = tn_pop_habitat_model_1)

  write_rds(all_models, here("models", "all_models.rds"))


} else {

  all_models <- read_rds(here("models", "all_models.rds"))

}

# Calculate model metrics for each model run
all_models_metrics <- list()
all_models_metrics <- lapply(all_models, function(x) {

  name = names(x)
  summary = summary(x)
  AIC = AIC(x)
  viz = getViz(x)
  check_x = check(viz)
  plot_x = plot(viz, all_terms = TRUE)

  return(name = list(summary = summary,
                     AIC = AIC,
                     check = check_x,
                     plot = plot_x))

})

model_table <- tibble()

for(i in 1:length(all_models_metrics)) {

  model <- names(all_models_metrics)[i]
  AIC <- all_models_metrics[[i]][["AIC"]]
  dev_exp <- round(all_models_metrics[[i]][["summary"]][["dev.expl"]], 2) * 100

  model_table <- bind_rows(model_table,
                           tibble("model" = model,
                                  "AIC" = AIC,
                                  "deviance" = dev_exp))

}

# Produce a table to compare model metrics
model_table

test <- region_pop_hab_sf %>%
  select(ID, GID_0, GID_2, NAME_2, x, y, region_trap_nights, tn_density, area_km2, pop_2005, cropland, shrubland, tree_cover, urban) %>%
  mutate(null_model = exp(as.numeric(predict(all_models$null_model, newdata = region_pop_hab_sf))),
         null_increased_k_model = exp(as.numeric(predict(all_models$null_increased_k, newdata = region_pop_hab_sf))),
         pop_model = exp(as.numeric(predict(all_models$pop_model, newdata = region_pop_hab_sf))),
         pop_model_increased_k = exp(as.numeric(predict(all_models$pop_model_increased_k, newdata = region_pop_hab_sf))),
         pop_area_model = exp(as.numeric(predict(all_models$pop_area_model, newdata = region_pop_hab_sf))),
         all_hab_model = exp(as.numeric(predict(all_models$all_hab_model, newdata = region_pop_hab_sf))),
         reduced_hab_model_1 = exp(as.numeric(predict(all_models$reduced_hab_model_1, newdata = region_pop_hab_sf))),
         reduced_hab_model_2 = exp(as.numeric(predict(all_models$reduced_hab_model_2, newdata = region_pop_hab_sf))),
         final_model = exp(as.numeric(predict(all_models$combined_model_1, newdata = region_pop_hab_sf)))) %>%
  pivot_longer(cols = contains("model"),
               values_to = "prediction",
               names_to = "model") %>%
  mutate(model = fct_inorder(model))

ggplot(test, aes(x = prediction, y = tn_density, group = model)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ model)

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

# Figure 2 ----------------------------------------------------------------

model_1 <- getViz(all_models$combined_model_1)

model_1_raster <- plot(sm(model_1, 4), n = 150, too.far = 0.08) +
  l_fitRaster(pTrans = zto1(0.05, 2, 0.1)) +
  l_fitContour() +
  coord_cartesian(expand = FALSE)

rect_border <- tibble(x = c(-17.64, -17.64, 15.995642, 15.995642),
                      y = c(4.2, 27.683098, 4.2, 27.683098)) %>%
  st_as_sf(coords = c("x", "y"), crs = crs(included_countries)) %>%
  summarise() %>%
  st_cast("POLYGON")

country_border <- included_countries %>%
  summarise()

inverse_country <- st_difference(rect_border, country_border)

# Reduce the opacity of the grid lines: Default is 255
col_grid <- rgb(235, 235, 235, 100, maxColorValue = 255)

fig_2_updated <- model_1_raster +
  geom_sf(data = inverse_country, fill = "white", colour = "white", inherit.aes = FALSE) +
  geom_sf(data = included_countries, fill = NA, alpha = 1, lwd = 0.5, inherit.aes = FALSE) +
  scale_fill_viridis_c(option = "inferno", na.value = "#ffffff00", direction = 1) +
  theme_minimal() +
  labs(title = element_blank(),
       x = element_blank(),
       y = element_blank(),
       fill = "Relative \ntrapping effort \nbias") +
  annotation_north_arrow(height = unit(1, "cm"),
                         style = north_arrow_minimal(text_size = 8)) +
  annotation_scale(height = unit(0.1, "cm"),
                   location = "tr") +
  theme(panel.ontop = TRUE,
        panel.grid = element_line(color = col_grid))

save_plot(plot = as.grob(fig_2_updated$ggObj),
          filename = here("figures", "Figure_2_updated.png"), dpi = 320, base_height = 10, base_width = 12)


# Supplementary figure model sensitivity ----------------------------------

tn_pop_habitat_model_sens <- read_rds(here("data_clean", "tn_pop_habitat_model_sens.rds"))

model_1_s <- getViz(tn_pop_habitat_model_sens)

supplementary_fig_2_updated <- plot(sm(model_1_s, 5), n = 150, too.far = 0.02) +
  l_fitRaster(pTrans = zto1(0.05, 2, 0.1)) +
  geom_sf(data = included_countries %>% filter(GID_0 != "CPV"), fill = NA, alpha = 0.4, lwd = 0.1, inherit.aes = FALSE) +
  scale_fill_viridis_c(option = "inferno", na.value = "#ffffff00", direction = -1) +
  theme_minimal() +
  labs(title = element_blank(),
       x = element_blank(),
       y = element_blank(),
       fill = "Relative \ntrapping effort \nbias") +
  annotation_north_arrow(height = unit(1, "cm"),
                         style = north_arrow_minimal(text_size = 8)) +
  annotation_scale(height = unit(0.1, "cm"),
                   location = "tr")

save_plot(plot = as.grob(supplementary_fig_2_updated$ggObj),
          filename = here("figures", "Figure_3_updated_sensitivity.png"), dpi = 320, base_height = 10, base_width = 12)
