source(here::here("scripts", "libraries.r"))

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

countries <- rodent_spatial %>%
  drop_na(country)
countries <- as.character(unique(countries$country))

#WA
benin <- getData("GADM", country = "Benin", level = 0, path = here("data_download", "admin_spatial"))
burkina_faso <- getData("GADM", country = "BFA", level = 0, path = here("data_download", "admin_spatial"))
cape_verde <- getData("GADM", country = "CPV", level = 0, path = here("data_download", "admin_spatial"))
gambia <- getData("GADM", country = "GMB", level = 0, path = here("data_download", "admin_spatial"))
ghana <- getData("GADM", country = "Ghana", level = 0, path = here("data_download", "admin_spatial"))
guinea <- getData("GADM", country = "Guinea", level = 0, path = here("data_download", "admin_spatial"))
guinea_bissau <- getData("GADM", country = "GNB", level = 0, path = here("data_download", "admin_spatial"))
ivory_coast <- getData("GADM", country = "CIV", level = 0, path = here("data_download", "admin_spatial"))
liberia <- getData("GADM", country = "LBR", level = 0, path = here("data_download", "admin_spatial"))
mali <- getData("GADM", country = "Mali", level = 0, path = here("data_download", "admin_spatial"))
mauritania <- getData("GADM", country = "Mauritania", level = 0, path = here("data_download", "admin_spatial"))
niger <- getData("GADM", country = "Niger", level = 0, path = here("data_download", "admin_spatial"))
nigeria <- getData("GADM", country = "Nigeria", level = 0, path = here("data_download", "admin_spatial"))
senegal <- getData("GADM", country = "Senegal", level = 0, path = here("data_download", "admin_spatial"))
sierra_leone <- getData("GADM", country = "SL", level = 0, path = here("data_download", "admin_spatial"))
togo <- getData("GADM", country = "Togo", level = 0, path = here("data_download", "admin_spatial"))

#non-WA
chad <- getData("GADM", country = "Chad", level = 0, path = here("data_download", "admin_spatial"))
morocco <- getData("GADM", country = "Morocco", level = 0, path = here("data_download", "admin_spatial"))
cameroon <- getData("GADM", country = "Cameroon", level = 0, path = here("data_download", "admin_spatial"))
western_sahara <- getData("GADM", country = "ESH", level = 0, path = here("data_download", "admin_spatial"))
algeria <- getData("GADM", country = "Algeria", level = 0, path = here("data_download", "admin_spatial"))

trapping_map <- tm_shape(benin, bbox = bbox_rodent) + tm_polygons(col = "#f2f0f0") + tm_layout(frame = F) +
  tm_shape(burkina_faso) + tm_polygons(col = "#f2f0f0") +
  tm_shape(gambia) + tm_polygons(col = "white") +
  tm_shape(ghana) + tm_polygons(col = "#f2f0f0") +
  tm_shape(guinea) + tm_polygons(col = "#f2f0f0") +
  tm_shape(guinea_bissau) + tm_polygons(col = "#f2f0f0") +
  tm_shape(ivory_coast) + tm_polygons(col = "#f2f0f0") +
  tm_shape(liberia) + tm_polygons(col = "#f2f0f0") +
  tm_shape(mali) + tm_polygons(col = "#f2f0f0") +
  tm_shape(mauritania) + tm_polygons(col = "#f2f0f0") +
  tm_shape(niger) + tm_polygons(col = "#f2f0f0") +
  tm_shape(nigeria) + tm_polygons(col = "#f2f0f0") +
  tm_shape(senegal) + tm_polygons(col = "#f2f0f0") +
  tm_shape(sierra_leone) + tm_polygons(col = "#f2f0f0") +
  tm_shape(togo) + tm_polygons(col = "white") +
  tm_shape(chad) + tm_polygons(col = "white") + tm_text("NAME_0") +
  tm_shape(morocco) + tm_polygons(col = "white") + tm_text("NAME_0") +
  tm_shape(cameroon) + tm_polygons(col = "white") + tm_text("NAME_0") +
  tm_shape(western_sahara) + tm_polygons(col = "white") +
  tm_shape(algeria) + tm_polygons(col = "white") + tm_text("NAME_0") +
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

mapview(rodent_spatial,
        zcol = "country")
