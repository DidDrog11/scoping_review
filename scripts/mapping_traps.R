source(here::here("scripts", "libraries.R"))

all_countries <- c("BEN", "BFA", "CIV", "CMR", "CPV", "DZA", "ESH", "GHA",
                   "GIN", "GMB", "GNB", "LBR", "MAR", "MLI", "MRT", "NER",
                   "NGA", "SEN", "SLE", "TCD", "TGO")
wa_countries <- c("BEN", "BFA", "CIV", "CPV", "ESH", "GHA",
                  "GIN", "GMB", "GNB", "LBR", "MLI", "MRT",
                  "NER", "NGA", "SEN", "SLE", "TGO")
no_data_countries <- c("GMB", "TGO")

level_0 <- read_rds(here("data_download", "admin_spatial", "level_0_admin.rds"))

level_1 <- read_rds(here("data_download", "admin_spatial", "level_1_admin.rds"))

level_2 <- read_rds(here("data_download", "admin_spatial", "level_2_admin.rds")) %>%
  bind_rows(level_1 %>%
              filter(GID_0 == "CPV")) #Cape Verde doesn't have level_2 administrative regions

non_trapped <- read_rds(here("data_download", "admin_spatial", "level_2_TGOGMB.rds"))

included_countries <- level_0 %>%
  filter(GID_0 %in% wa_countries)

studies <- read_rds(here("data_clean", "studies.rds"))

rodent_spatial <- read_rds(here("data_clean", "rodent_spatial.rds")) %>%
  dplyr::select(-trap_nights)

imputed_tn <- read_rds(here("data_clean", "imputed_trap_nights.rds"))

rodent_spatial <- rodent_spatial %>%
  left_join(., imputed_tn,
            by = c("unique_id", "year_trapping", "month_trapping",
                   "region", "town_village", "habitat"))

bbox_rodent <- st_bbox(rodent_spatial)

tmap_options(check.and.fix = TRUE)

#expand the bounding box
xcrange <- bbox_rodent$xmax - bbox_rodent$xmin # range of x values
ycrange <- bbox_rodent$ymax - bbox_rodent$ymin # range of y values
bbox_rodent[1] <- bbox_rodent[1] - (0.1 * xcrange) # xmin - left
bbox_rodent[3] <- bbox_rodent[3] + (0.2 * xcrange) # xmax - right
bbox_rodent[2] <- bbox_rodent[2] - (0.1 * ycrange) # ymin - bottom
bbox_rodent[4] <- bbox_rodent[4] + (0.1 * ycrange) # ymax - top
bbox_rodent <- bbox_rodent %>%
  st_as_sfc()

if(!file.exists(here("figures", "static_site_map.png"))){
  trapping_map <-
    tm_shape(level_0 %>%
               filter(GID_0 %in% c(wa_countries) & !GID_0 %in% c(no_data_countries)),
             bbox = st_bbox(level_0)) +
    tm_polygons(col = "#f2f0f0") +
    tm_shape(level_0 %>%
               filter(GID_0 %in% c(no_data_countries))) +
    tm_polygons(col = "white") +
    tm_shape(level_0 %>%
               filter(GID_0 %in% c(all_countries) & !GID_0 %in% c(wa_countries)) %>%
               st_cast(to = "MULTIPOLYGON")) +
    tm_polygons(col = "white") + tm_text("NAME_0") +
    tm_layout(frame = F) +
    tm_shape(rodent_spatial %>%
               distinct(geometry)) + tm_dots(col = "black", size = 0.05, shape = 19) +
    tm_scale_bar(position = c("left", "bottom")) +
    tm_compass(position = c("right", "top"))

  data("World")
  afr <- st_as_sf(World) %>%
    filter(continent == "Africa") %>%
    st_make_valid()

  # extracting bounding box Africa
  region <- st_as_sfc(st_bbox(afr))

  afrmap <- tm_shape(afr) + tm_polygons() +
    tm_shape(st_as_sfc(st_bbox(level_0))) + tm_polygons(col = "orange", alpha = 0.5) +
    tm_shape(region) + tm_borders(lwd = .2)

  vp <- grid::viewport(0.13, 0.88, width = 0.23, height = 0.23)

  tmap_save(trapping_map, filename = here("figures", "static_site_map.png"),
            dpi = 320, insets_tm = afrmap, insets_vp = vp)
}

# Plot as leaflet ---------------------------------------------------------
tmap_mode("plot")

tm_shape(rodent_spatial) +
  tm_dots(col = "iso3c")

mapview(rodent_spatial,
        zcol = "iso3c")

trap_map <- rodent_spatial %>%
  left_join(., studies %>%
              dplyr::select(unique_id, first_author, year_publication, link),
            by = "unique_id") %>%
  distinct(unique_id, geometry, .keep_all = T)

factcont <- colorNumeric("viridis", trap_map$year_publication, alpha = T, reverse = T)

leaflet(trap_map) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(fillColor = ~factcont(year_publication),
                   stroke = F,
                   fillOpacity = 0.5,
                   radius = 4,
                   label = paste(trap_map$first_author, trap_map$year_publication, sep = ", "),
                   popup = paste0("<a href='",
                                  trap_map$link,
                                  "', target ='_blank'>",
                                  "Article link")) %>%
  addLegend("topright",
            title = "Year published",
            pal = factcont,
            opacity = 0.9,
            values = ~year_publication,
            labFormat = labelFormat(big.mark = ""))

# Map as discrete ---------------------------------------------------------
trapping_effort <- rodent_spatial %>%
  distinct(unique_id, year_trapping, month_trapping, country, region, town_village, habitat, trap_nights, geometry)

if(!file.exists(here("data_clean", "traps_level_2.rds"))) {
  sites_2 <- st_intersection(x = level_2, y = trapping_effort)
  write_rds(sites_2, here("data_clean", "traps_level_2.rds"))
} else {
  sites_2 <- read_rds(here("data_clean", "traps_level_2.rds"))
}

trap_nights_region <- tibble(sites_2) %>%
  group_by(GID_1, NAME_2, GID_2, unique_id, year_trapping, month_trapping, region, town_village, habitat) %>%
  summarise(total_trap_nights = sum(trap_nights)) %>%
  group_by(GID_1, GID_2, NAME_2) %>%
  summarise(region_trap_nights = sum(total_trap_nights))

level_2_sites <- level_2 %>%
  left_join(., trap_nights_region,
            by = c("GID_1", "GID_2", "NAME_2")) %>%
  mutate(area_m2 = st_area(.),
         tn_density = region_trap_nights/(as.numeric(area_m2)/1000000),
         tn_density = ifelse(is.na(tn_density), NA, tn_density))

level_2_sites$breaks <- cut(level_2_sites$tn_density,
                            breaks = c(0, 0.001, 0.01, 0.1, 1, 10, 100, 200),
                            labels = c("0 - 0.001", "0.001 - 0.01", "0.01 - 0.1",
                                       "0.1 - 1", "1 - 10", "10 - 100", "100 - 200"))

trap_night_density_level2 <- ggplot() +
  geom_sf(data = level_2_sites,
          mapping = aes(fill = breaks),
          lwd = 0.05,
          colour = "gray") +
  scale_fill_viridis_d(aesthetics = "fill",
                       direction = 1,
                       labels = c("0 - 0.001", "0.001 - 0.01", "0.01 - 0.1",
                                  "0.1 - 1", "1 - 10", "10 - 100", "100 - 200",
                                  "No trapping")) +
  geom_sf(data = level_0 %>%
            filter(GID_0 %in% wa_countries),
          alpha = 0, lwd = 0.1, colour = "black") +
  theme_minimal() +
  labs(fill = parse(text = paste("Density~of~trap~nights~per~1000~km^2"))) +
  annotation_north_arrow(height = unit(1, "cm"),
                         style = north_arrow_minimal(text_size = 8)) +
  annotation_scale(height = unit(0.1, "cm"),
                   location = "tr")

write_rds(trap_night_density_level2, here("plots", "tn_density.rds"))

# Human population --------------------------------------------------------

human_pop <- rast(here("data_download", "pop_2005","pop_2005.tif"))
vect_sites <- vect(bind_rows(level_2_sites,
                             non_trapped))

crop_pop <- crop(human_pop, vect_sites)
region_pop <- terra::extract(crop_pop, vect_sites, fun = "median", method = "simple", na.rm = TRUE, touches = TRUE)
region_pop_sf <- cbind(vect_sites, region_pop) %>%
  st_as_sf() %>%
  mutate(pop_2005 = case_when(GID_2 == "CIV.14.1_1" ~ 12,
                              GID_2 == "NGA.1.1_1" ~ 6,
                              GID_2 == "NGA.4.18_1" ~ 8,
                              GID_2 == "NGA.20.31_1" ~ 1.6,
                              GID_2 == "NGA.31.9_1" ~ 11.3,
                              GID_2 == "GMB.1.1_1" ~ 3800,
                              GID_2 == "GMD.3.2_1" ~ 72,
                              TRUE ~ pop_2005))

deciles <- quantile(region_pop_sf$pop_2005, na.rm = T, probs = seq(0.1, 0.9, by = 0.1)) %>%
  round(., -1)

population_map <- tm_shape(region_pop_sf) +
  tm_fill("pop_2005",
          style = "fixed",
          breaks = c(0, deciles, round(max(region_pop_sf$pop_2005), -4)),
          title = parse(text = paste("Population~density~per~1~km^2"))) +
  tm_shape(level_0 %>%
             filter(GID_0 %in% wa_countries)) +
  tm_polygons(alpha = 0, lwd = 1) +
  tm_compass(type = "arrow", size = 1, position = c("left", "bottom")) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_graticules(labels.show = TRUE,
                lwd = 0.5,
                alpha = 0.5)

write_rds(population_map, here("plots", "pop_map_2005.rds"))
tmap_save(population_map, filename = here("figures", "pop_density_2.png"))

non_trapped_regions <- tibble(region_pop_sf) %>%
  select(-geometry) %>%
  mutate(tn_density = case_when(is.na(tn_density) ~ 0,
                                TRUE ~ tn_density)) %>%
  filter(tn_density == 0)

log_pop_tn <- ggplot(tibble(region_pop_sf) %>%
                       select(-geometry) %>%
                       mutate(tn_density = case_when(is.na(tn_density) ~ 0,
                                                     TRUE ~ tn_density))) +
  geom_smooth(aes(x = tn_density, y = log(pop_2005)),
              formula = y ~ s(x, bs = "cs"),
              method = "gam") +
  geom_point(data = . %>%
               filter(tn_density > 0),
             aes(x = tn_density, y = log(pop_2005)),
             colour = "#440154") +
  geom_boxplot(non_trapped_regions,
               mapping = aes(x = 0.01, y = log(pop_2005)),
               colour = "#fde725",
               fill = "#fde725",
               alpha = 0.6,
               inherit.aes = FALSE,
               position = position_nudge(x = - 2)) +
  theme_minimal() +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = "b") +
  labs(x = parse(text = paste("log~Trap~night~density~per~1000~km^2")),
       y = parse(text = paste("log~Population~density~per~1~km^2~(2005)")))

pop_tn <- ggplot(tibble(region_pop_sf) %>%
         select(-geometry) %>%
         mutate(tn_density = case_when(is.na(tn_density) ~ 0,
                                       TRUE ~ tn_density))) +
  geom_smooth(aes(x = tn_density, y = pop_2005),
              formula = y ~ s(x, bs = "cs")) +
  geom_point(data = . %>%
               filter(tn_density > 0),
             aes(x = tn_density, y = pop_2005),
             colour = "#440154") +
  geom_point(data = . %>%
               filter(tn_density == 0),
             aes(x = tn_density, y = pop_2005),
             colour = "#fde725",
             alpha = 0.2) +
  theme_minimal() +
  labs(x = parse(text = paste("Trap~night~density~per~1000~km^2")),
       y = parse(text = paste("Population~density~per~1~km^2~(2005~level)")))

write_rds(log_pop_tn, here("plots", "log_pop_tn.rds"))
write_rds(region_pop_sf, here("data_clean", "pop_tn_analysis.rds"))

ggsave(plot = log_pop_tn, here("figures", "log_pop_tn.png"), dpi = 300)
ggsave(plot = pop_tn, here("figures", "pop_tn.png"), dpi = 300)


# Plotting trapping effort model ------------------------------------------

pop_predict <- rast(here("data_clean", "prediction_space.tif"))

tn_to_pop <- tm_shape(pop_predict) +
  tm_raster(title = "Trap nights per log(Population density)",
            palette = "-viridis",
            n = 14,
            midpoint = 0,
            showNA = TRUE,
            legend.is.portrait = FALSE) +
  tm_shape(st_cast(level_0, to = "MULTIPOLYGON")) +
  tm_borders() +
  tm_text("NAME_0") +
  tm_layout(legend.outside = TRUE,
            legend.outside.position = "bottom") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("left", "bottom")) +
  tm_graticules(labels.show = TRUE,
                lwd = 0.5,
                alpha = 0.5)

write_rds(tn_to_pop, here("plots", "tn_to_pop.rds"))

tn_pop_map <- read_rds(here("data_clean", "tn_pop_model.rds"))

map_1 <- getViz(tn_pop_map)

m1 <- plot(sm(map_1, 1), n = 150, too.far = 0.02) +
  l_fitRaster(pTrans = zto1(0.05, 2, 0.1)) +
  geom_sf(data = included_countries, alpha = 0.1, lwd = 0.1, inherit.aes = FALSE) +
  scale_fill_viridis_c(na.value = "#ffffff00") +
  theme_minimal() +
  labs(title = element_blank(),
       x = element_blank(),
       y = element_blank(),
       fill = "Relative trapping density") +
  annotation_north_arrow(height = unit(1, "cm"),
                         style = north_arrow_minimal(text_size = 8)) +
  annotation_scale(height = unit(0.1, "cm"),
                   location = "tr")

tn_pop_map_sens <- read_rds(here("data_clean", "tn_pop_model_sens.rds"))

m1_ss <- plot(sm(map_1, 1), n = 150, too.far = 0.02) +
  l_fitRaster(pTrans = function(.p) .p<0.05) +
  geom_sf(data = included_countries, alpha = 0.1, lwd = 0.1, inherit.aes = FALSE) +
  scale_fill_viridis_c(na.value = "#ffffff00") +
  theme_minimal() +
  labs(title = element_blank(),
       x = element_blank(),
       y = element_blank(),
       fill = "Relative trapping density") +
  annotation_north_arrow(height = unit(1, "cm"),
                         style = north_arrow_minimal(text_size = 8)) +
  annotation_scale(height = unit(0.1, "cm"),
                   location = "tr")

map_1_s <- getViz(tn_pop_map_sens)

m1_s <- plot(sm(map_1_s, 1), n = 150, too.far = 0.02) +
  l_fitRaster(pTrans = zto1(0.05, 2, 0.1)) +
  geom_sf(data = included_countries, alpha = 0.1, lwd = 0.1, inherit.aes = FALSE) +
  scale_fill_viridis_c(na.value = "#ffffff00") +
  theme_minimal() +
  labs(title = element_blank(),
       x = element_blank(),
       y = element_blank(),
       fill = "Relative trapping density") +
  annotation_north_arrow(height = unit(1, "cm"),
                         style = north_arrow_minimal(text_size = 8)) +
  annotation_scale(height = unit(0.1, "cm"),
                   location = "tr")

m1_s_ss <- plot(sm(map_1_s, 1), n = 150, too.far = 0.02) +
  l_fitRaster(pTrans = function(.p) .p<0.05) +
  geom_sf(data = included_countries, alpha = 0.1, lwd = 0.1, inherit.aes = FALSE) +
  scale_fill_viridis_c(na.value = "#ffffff00") +
  theme_minimal() +
  labs(title = element_blank(),
       x = element_blank(),
       y = element_blank(),
       fill = "Relative trapping density") +
  annotation_north_arrow(height = unit(1, "cm"),
                         style = north_arrow_minimal(text_size = 8)) +
  annotation_scale(height = unit(0.1, "cm"),
                   location = "tr")

B <- as.grob(m1$ggObj)

A <- trap_night_density_level2

fig_2 <- plot_grid(A,
                   B,
                   nrow = 2,
                   align = "v",
                   axis = "l",
                   labels = "AUTO")

fig_2_sens <- plot_grid(m1_s$ggObj)

fig_2_ss <- plot_grid(as.grob(m1_ss$ggObj),
                      as.grob(m1_s_ss$ggObj),
                      nrow = 2,
                      align = "v",
                      axis = "l",
                      labels = "AUTO")


save_plot(here("figures", "Figure_2.png"), fig_2, base_height = 10, base_width = 12)
save_plot(here("figures", "Figure_2_sensitivity.png"), fig_2_sens, base_height = 10, base_width = 12)
save_plot(here("figures", "Figure_2_ss.png"), fig_2_ss, base_height = 10, base_width = 12)
