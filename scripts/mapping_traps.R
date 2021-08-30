source(here::here("scripts", "libraries.r"))

all_countries <- c("BEN", "BFA", "CIV", "CMR", "CPV", "DZA", "ESH", "GHA",
                   "GIN", "GMB", "GNB", "LBR", "MAR", "MLI", "MRT", "NER",
                   "NGA", "SEN", "SLE", "TCD", "TGO")
wa_countries <- c("BEN", "BFA", "CIV", "CPV", "ESH", "GHA",
                  "GIN", "GMB", "GNB", "LBR", "MLI", "MRT",
                  "NER", "NGA", "SEN", "SLE", "TGO")
no_data_countries <- c("GMB", "TGO")

level_0 <- read_rds(here("data_download", "admin_spatial", "level_0_admin.rds"))

level_1 <- read_rds(here("data_download", "admin_spatial", "level_1_admin.rds"))

level_2 <- read_rds(here("data_download", "admin_spatial", "level_2_admin.rds"))

studies <- read_rds(here("data_clean", "studies.rds"))

rodent_spatial <- read_rds(here("data_clean", "rodent_spatial.rds")) %>%
  select(-trap_nights)

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
  group_by(NAME_2, GID_2, unique_id, year_trapping, month_trapping, region, town_village, habitat) %>%
  summarise(total_trap_nights = sum(trap_nights)) %>%
  group_by(GID_2, NAME_2) %>%
  summarise(region_trap_nights = sum(total_trap_nights))

level_2_sites <- level_2 %>%
  left_join(., trap_nights_region,
            by = c("GID_2", "NAME_2")) %>%
  mutate(area_m2 = st_area(.),
         tn_density = region_trap_nights/(as.numeric(area_m2)/1000000),
         tn_density = ifelse(is.na(tn_density), NA, tn_density))

trap_night_density_level2 <- tm_shape(level_2_sites) +
  tm_polygons(col = "tn_density", style = "fixed",
              breaks = c(0, 0.001, 0.01, 0.1, 1, 10, 100, 200),
              legend.format = list(fun = function(x) paste0(formatC(x, drop0trailing = TRUE))),
              palette = "-viridis", colorNA = NULL, border.alpha = 1, border.col = "grey", lwd = 0.1,
              title = parse(text = paste("Density~of~trap~nights~per~1000~km^2"))) +
  tm_shape(level_0 %>%
             filter(GID_0 %in% wa_countries)) +  tm_polygons(alpha = 0, lwd = 1) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_graticules(labels.show = TRUE,
                lwd = 0.5,
                alpha = 0.5)

tmap_save(trap_night_density_level2, filename = here("figures", "static_tn_density_2.png"))

# Human population --------------------------------------------------------

human_pop <- rast(here("data_download", "pop_2005","pop_2005.tif"))
vect_sites <- vect(level_2_sites)

crop_pop <- crop(human_pop, vect_sites)
region_pop <- terra::extract(crop_pop, vect_sites, fun = "median", method = "simple")
region_pop_sf <- cbind(level_2_sites, region_pop) %>%
  st_as_sf()

deciles <- quantile(region_pop_sf$pop_2005, na.rm = T, probs = seq(0.1, 0.9, by = 0.1)) %>%
  round(., -1)

population_map <- tm_shape(region_pop_sf) +
  tm_fill("pop_2005",
          style = "fixed",
          breaks = c(0, deciles, max(region_pop_sf$pop_2005))) +
  tm_shape(level_0 %>%
             filter(GID_0 %in% wa_countries)) +  tm_polygons(alpha = 0, lwd = 1) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_graticules(labels.show = TRUE,
                lwd = 0.5,
                alpha = 0.5)



