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
                           pop < -3 ~ 0
                           TRUE ~ pop))

  pixel_trapping_combined <- bind_rows(pixel_trapping, pixel_non_trapped)

  tn_pixel_simple <- bam(tn_density ~ s(x, y, k = 540, bs = "ts"),
                         family = "tw",
                         data = pixel_trapping_combined,
                         select = TRUE)

  tn_pixel_pop_model <- bam(tn ~ s(pop, k = 81, bs = "cs") + s(x, y, k = 540, bs = "ts"),
                            family = "tw",
                            select = TRUE,
                            data = pixel_trapping_combined)

  tn_pixel_habitat_model <- bam(tn ~ hab + s(x, y, k = 540, bs = "ts"),
                                family = "tw",
                                select = TRUE,
                                data = pixel_trapping_combined)

  tn_pixel_pop_hab_model <- bam(tn ~ s(pop, k = 81, bs = "cs", by = hab) + s(x, y, k = 540, bs = "ts"),
                                family = "tw",
                                select = TRUE,
                                data = pixel_trapping_combined)

  summary(tn_pixel_simple)
  gam.check(tn_pixel_simple)
  plot(tn_pixel_simple, all.terms = TRUE)

  summary(tn_pixel_pop_model)
  gam.check(tn_pixel_pop_model)
  plot(tn_pixel_pop_model, all.terms = TRUE)

  summary(tn_pixel_habitat_model)
  gam.check(tn_pixel_habitat_model)
  plot(tn_pixel_habitat_model, all.terms = TRUE)

  summary(tn_pixel_pop_hab_model)
  gam.check(tn_pixel_pop_hab_model)
  plot(tn_pixel_pop_hab_model, all.terms = TRUE)

  model_1_pixel <- getViz(tn_pixel_simple)
  model_4_pixel <- getViz(tn_pixel_pop_hab_model)

  fig_2__simple_pixel <- plot(sm(model_1_pixel, 1), n = 150, too.far = 0.02) +
    l_fitRaster(pTrans = zto1(0.05, 2, 0.1)) +
    geom_sf(data = included_countries %>% filter(GID_0 != "CPV"), fill = NA, alpha = 1, lwd = 0.5, inherit.aes = FALSE) +
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

  fig_2__complex_pixel <- plot(sm(model_4_pixel, 10), n = 150, too.far = 0.02) +
    l_fitRaster(pTrans = zto1(0.05, 2, 0.1)) +
    geom_sf(data = included_countries %>% filter(GID_0 != "CPV"), fill = NA, alpha = 1, lwd = 0.5, inherit.aes = FALSE) +
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

  plot_grid(plotlist = list(fig_2__simple_pixel$ggObj, fig_2__complex_pixel$ggObj))

  save_plot(plot = as.grob(fig_2_pixel$ggObj),
            filename = here("figures", "Figure_2_pixel.png"), dpi = 320, base_height = 10, base_width = 12)

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
