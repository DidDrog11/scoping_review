source(here::here("scripts", "libraries.R"))

# Trap night and population -----------------------------------------------
if(!file.exists(here("data_clean", "pixel_pop_hab_sf.rds"))) {
  rodent_spatial <- read_rds(here("data_clean", "rodent_spatial.rds")) %>%
    dplyr::select(-trap_nights)

  imputed_tn <- read_rds(here("data_clean", "imputed_trap_nights.rds"))

  rodent_spatial <- rodent_spatial %>%
    left_join(., imputed_tn,
              by = c("unique_id", "year_trapping", "month_trapping",
                     "region", "town_village", "habitat"))

  continental_countries <- c("BEN", "BFA", "CIV", "ESH", "GHA",
                             "GIN", "GMB", "GNB", "LBR", "MLI", "MRT",
                             "NER", "NGA", "SEN", "SLE", "TGO")

  level_0 <- read_rds(here("data_download", "admin_spatial", "level_0_admin.rds"))

  level_1 <- read_rds(here("data_download", "admin_spatial", "level_1_admin.rds"))

  level_2 <- read_rds(here("data_download", "admin_spatial", "level_2_admin.rds"))

  non_trapped <- read_rds(here("data_download", "admin_spatial", "level_2_TGOGMB.rds"))

  level_2_all <- bind_rows(level_2, non_trapped)

  included_countries <- level_0 %>%
    filter(GID_0 %in% continental_countries)

  contiguous_boundary <- included_countries %>%
    filter(!GID_0 == "CPV") %>%
    st_union() %>%
    vect()

  studies <- read_rds(here("data_clean", "studies.rds"))

  zoonotic_studies <- studies %>%
    filter(str_detect(aim, "Zoonoses")) %>%
    pull(unique_id)

  trapping_effort <- rodent_spatial %>%
    filter(unique_id %in% zoonotic_studies) %>%
    distinct(unique_id, year_trapping, month_trapping, trap_nights, geometry) %>%
    drop_na(geometry, trap_nights) %>%
    mutate(x = st_coordinates(geometry)[,1],
           y = st_coordinates(geometry)[,2]) %>%
    group_by(x, y) %>%
    summarise(trap_nights = sum(trap_nights)) %>%
    st_as_sf(coords = c("x", "y")) %>%
    vect() %>%
    mask(contiguous_boundary)

  wa_pop <- rast(here("data_download", "pop_2005","pop_2005.tif")) %>%
    crop(contiguous_boundary) %>%
    mask(contiguous_boundary)

  wa_pop <- wa_pop$pop_2005

  source(here("scripts", "land_use_classification.R"))

  habitat_2005 <- rast(here("data_download", "habitat_2005", "habitat_2005.nc"))[[1]] %>%
    crop(contiguous_boundary) %>%
    mask(contiguous_boundary) %>%
    classify(classification_raster, include.lowest = TRUE)

  resample_hab <- resample(habitat_2005, wa_pop, method = "near")

  trapping_effort$pop <- extract(wa_pop, trapping_effort)$pop_2005
  trapping_effort$hab <- extract(resample_hab, trapping_effort)$lccs_class
  trapping_effort$area <- extract(cellSize(mask(resample_hab, trapping_effort), unit = "km"), trapping_effort)$area

  pixel_trapping <- st_as_sf(trapping_effort) %>%
    drop_na(hab) %>%
    tibble() %>%
    mutate(hab = factor(case_when(hab == 1 ~ "agriculture",
                                  hab == 2 ~ "forest",
                                  hab == 3 ~ "grassland",
                                  hab == 4 ~ "shrubland",
                                  hab == 5 ~ "sparse_vegetation",
                                  hab == 6 ~ "wetland",
                                  hab == 7 ~ "urban",
                                  hab == 8 ~ "bare",
                                  hab == 9 ~ "water")),
           tn_density = trap_nights/area,
           tn = trap_nights,
           x = st_coordinates(geometry)[,1],
           y = st_coordinates(geometry)[,2]) %>%
    select(tn, tn_density, pop, hab, x, y)

  pixel_non_trapped <- tibble(spatSample(resample_hab, size = nrow(pixel_trapping)*5, method = "stratified", na.rm = TRUE, cells = TRUE, xy = TRUE)) %>%
    mutate(hab = factor(case_when(lccs_class == 1 ~ "agriculture",
                                  lccs_class == 2 ~ "forest",
                                  lccs_class == 3 ~ "grassland",
                                  lccs_class == 4 ~ "shrubland",
                                  lccs_class == 5 ~ "sparse_vegetation",
                                  lccs_class == 6 ~ "wetland",
                                  lccs_class == 7 ~ "urban",
                                  lccs_class == 8 ~ "bare",
                                  lccs_class == 9 ~ "water")),
           tn_density = 0,
           tn = 0) %>%
    select(tn, tn_density, hab, x, y)

  pixel_non_trapped$pop <- as.numeric(extract(wa_pop, vect(pixel_non_trapped, geom = c("x", "y")))$pop_2005)
  pixel_non_trapped <- pixel_non_trapped %>%
    mutate(pop = case_when(is.na(pop) ~ 0,
                           pop < -3 ~ 0,
                           TRUE ~ pop))

  pixel_trapping_combined <- bind_rows(pixel_trapping, pixel_non_trapped)

  write_rds(pixel_trapping_combined, here("data_clean", "pixel_pop_hab_sf.rds"))

} else {

  pixel_trapping_combined <- read_rds(here("data_clean", "pixel_pop_hab_sf.rds"))

}

if(!file.exists(here("models", "pixel_models.rds"))) {

  tn_pixel_simple <- gam(tn_density ~ s(x, y, k = 540, bs = "ts"),
                         family = "tw",
                         data = pixel_trapping_combined,
                         select = TRUE)

  tn_pixel_pop_hab_model <- gam(tn ~ s(pop, k = 81, bs = "cs") + hab + s(x, y, k = 540, bs = "ts"),
                                family = "tw",
                                select = TRUE,
                                data = pixel_trapping_combined)

  pixel_models <- list(simple_pixel = tn_pixel_simple,
                       full_pixel = tn_pixel_pop_hab_model)

  write_rds(pixel_models, here("models", "pixel_models.rds"))

} else {

  pixel_models <- read_rds(here("models", "pixel_models.rds"))

}

pixel_models_metrics <- list()
pixel_models_metrics <- lapply(pixel_models, function(x) {

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

rect_border <- tibble(x = c(-17.64, -17.64, 15.995642, 15.995642),
                      y = c(4.2, 27.683098, 4.2, 27.683098)) %>%
  st_as_sf(coords = c("x", "y"), crs = crs(included_countries)) %>%
  summarise() %>%
  st_cast("POLYGON") %>%
  st_buffer(dist = 10)

country_border <- included_countries %>%
  summarise()

inverse_country <- st_difference(rect_border, country_border)

# Reduce the opacity of the grid lines: Default is 255
col_grid <- rgb(235, 235, 235, 100, maxColorValue = 255)

# Diverging palette
brbg_hcl <- colorspace::diverging_hcl(11,
                                      h = c(180, 50), c = 80, l = c(20, 95), power = c(0.7, 1.3),
                                      register = "diverging_map")

model_1_pixel <- getViz(pixel_models$full_pixel)

model_1_p_raster <- plot(sm(model_1_pixel, 2), n = 150, too.far = 0.02) +
  l_fitRaster(pTrans = zto1(0.05, 2, 0.1)) +
  l_fitContour() +
  coord_cartesian(expand = FALSE)

supplementary_fig_3_updated <- model_1_p_raster +
  geom_sf(data = inverse_country, fill = "white", colour = "white", inherit.aes = FALSE) +
  geom_sf(data = included_countries, fill = NA, alpha = 1, lwd = 0.5, inherit.aes = FALSE) +
  scale_fill_continuous_diverging(palette = "diverging_map", na.value = "#ffffff00", limits = c(-15, 15)) +
  theme_minimal() +
  labs(title = element_blank(),
       x = element_blank(),
       y = element_blank(),
       fill = "Relative \ntrapping effort \nbias") +
  annotation_north_arrow(height = unit(2, "cm"),
                         style = north_arrow_minimal(text_size = 10)) +
  annotation_scale(height = unit(0.1, "cm"),
                   location = "tr") +
  theme(panel.ontop = TRUE,
        panel.grid = element_line(color = col_grid))

save_plot(plot = as.grob(supplementary_fig_3_updated$ggObj),
          filename = here("figures", "Supplementary_Figure_3.pdf"), dpi = 320, base_height = 10, base_width = 12)
save_plot(plot = as.grob(supplementary_fig_3_updated$ggObj),
          filename = here("figures", "Supplementary_Figure_3.png"), dpi = 320, base_height = 10, base_width = 12)
