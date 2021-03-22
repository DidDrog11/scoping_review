source(here::here("scripts", "libraries.r"))

all_countries <- c("BEN", "BFA", "CIV", "CMR", "CPV", "DZA", "ESH", "GHA",
                   "GIN", "GMB", "GNB", "LBR", "MAR", "MLI", "MRT", "NER",
                   "NGA", "SEN", "SLE", "TCD", "TGO")
wa_countries <- c("BEN", "BFA", "CIV", "CPV", "ESH", "GHA",
                  "GIN", "GMB", "GNB", "LBR", "MLI", "MRT",
                  "NER", "NGA", "SEN", "SLE", "TGO")
no_data_countries <- c("GMB", "TGO", "CPV")

level_0 <- read_rds(here("data_download", "admin_spatial", "level_0_admin.rds"))
list2env(level_0, envir = .GlobalEnv)
level_0 <- do.call(rbind.SpatialPolygonsDataFrame, level_0)  %>%
  st_as_sf()

level_1 <- read_rds(here("data_download", "admin_spatial", "level_1_admin.rds"))
list2env(level_1, envir = .GlobalEnv)
level_1_all <- do.call(rbind.SpatialPolygonsDataFrame, level_1) %>%
  st_as_sf()

rodent_spatial <- read_rds(here("data_clean", "rodent_spatial.rds"))
bbox_rodent <- st_bbox(rodent_spatial)

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
trapping_map <- tm_shape(BEN_0, bbox = bbox_rodent) + tm_polygons(col = "#f2f0f0") + tm_layout(frame = F) +
  tm_shape(BFA_0) + tm_polygons(col = "#f2f0f0") +
  tm_shape(GMB_0) + tm_polygons(col = "white") +
  tm_shape(GHA_0) + tm_polygons(col = "#f2f0f0") +
  tm_shape(GIN_0) + tm_polygons(col = "#f2f0f0") +
  tm_shape(GNB_0) + tm_polygons(col = "#f2f0f0") +
  tm_shape(CIV_0) + tm_polygons(col = "#f2f0f0") +
  tm_shape(LBR_0) + tm_polygons(col = "#f2f0f0") +
  tm_shape(MLI_0) + tm_polygons(col = "#f2f0f0") +
  tm_shape(MRT_0) + tm_polygons(col = "#f2f0f0") +
  tm_shape(NER_0) + tm_polygons(col = "#f2f0f0") +
  tm_shape(NGA_0) + tm_polygons(col = "#f2f0f0") +
  tm_shape(SEN_0) + tm_polygons(col = "#f2f0f0") +
  tm_shape(SLE_0) + tm_polygons(col = "#f2f0f0") +
  tm_shape(TGO_0) + tm_polygons(col = "white") +
  tm_shape(TCD_0) + tm_polygons(col = "white") + tm_text("NAME_0") +
  tm_shape(MAR_0) + tm_polygons(col = "white") + tm_text("NAME_0") +
  tm_shape(CMR_0) + tm_polygons(col = "white") + tm_text("NAME_0") +
  tm_shape(ESH_0) + tm_polygons(col = "white") +
  tm_shape(DZA_0) + tm_polygons(col = "white") + tm_text("NAME_0") +
  tm_shape(rodent_spatial %>%
             distinct(geometry)) + tm_dots(col = "black", size = 0.05, shape = 19) +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_compass(position = c("right", "top"))

data("wrld_simpl")
afr <- wrld_simpl[wrld_simpl$REGION==2,]

# extracting bounding box Abidjan
region <- st_as_sfc(st_bbox(afr))

afrmap <- tm_shape(afr) + tm_polygons() +
  tm_shape(bbox_rodent) + tm_polygons(col = "orange", alpha = 0.5) +
  tm_shape(region) + tm_borders(lwd = .2)

print(afrmap, vp = grid::viewport(0.13, 0.88, width = 0.23, height = 0.23))
}

# Plot as mapview ---------------------------------------------------------
tmap_mode("view")

tm_shape(rodent_spatial) +
  tm_dots(col = "iso3c")

mapview(rodent_spatial,
        zcol = "iso3c")

tmap_mode("plot")
# Map as discrete ---------------------------------------------------------
sites <- st_intersection(x = level_1_all, y = rodent_spatial)
n_sites_region <- sites %>%
  group_by(NAME_1) %>%
  count() %>%
  tibble()

level_1_sites <- level_1_all %>%
  left_join(., n_sites_region %>%
              dplyr::select(-geometry),
            by = "NAME_1") %>%
  mutate(area_m2 = st_area(.),
         site_density = n/(as.numeric(area_m2)/1000000),
         site_density = ifelse(is.na(site_density), NA, site_density))

site_density <- tm_shape(level_1_sites) +  tm_polygons(col = "site_density", style = "fixed", breaks = c(0, 0.001, 0.005, 0.01, 0.05, 1, 6),
                                                       palette = "-viridis", colorNA = NULL, border.alpha = 0.4, border.col = "grey", lwd = 1,
                                                       title = parse(text = paste("Density~of~trap~sites~per~1000~km^2"))) +
  tm_shape(level_0 %>%
             filter(GID_0 %in% wa_countries)) +  tm_polygons(alpha = 0, lwd = 1)

tmap_save(site_density, filename = here("figures", "static_site_density.png"))

high_density <- level_1_sites %>%
  arrange(-site_density) %>%
  as_tibble() %>%
  dplyr::select(NAME_0, NAME_1, TYPE_1, site_density) %>%
  filter(site_density > 0.01)
