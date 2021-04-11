source(here::here("scripts", "libraries.r"))

wa_mainland <- c("BEN", "BFA", "CIV", "ESH", "GHA",
                  "GIN", "GMB", "GNB", "LBR", "MLI", "MRT",
                  "NER", "NGA", "SEN", "SLE", "TGO")
wa_countries <- c("BEN", "BFA", "CIV", "CPV", "ESH", "GHA",
                  "GIN", "GMB", "GNB", "LBR", "MLI", "MRT",
                  "NER", "NGA", "SEN", "SLE", "TGO")

level_0 <- read_rds(here("data_download", "admin_spatial", "level_0_admin.rds"))
list2env(level_0, envir = .GlobalEnv)
level_0 <- do.call(rbind.SpatialPolygonsDataFrame, level_0)  %>%
  st_as_sf()

level_1 <- read_rds(here("data_download", "admin_spatial", "level_1_admin.rds"))
list2env(level_1, envir = .GlobalEnv)
level_1_all <- do.call(rbind.SpatialPolygonsDataFrame, level_1) %>%
  st_as_sf()

level_2 <- read_rds(here("data_download", "admin_spatial", "level_2_admin.rds"))
list2env(level_2, envir = .GlobalEnv)
level_2_all <- do.call(rbind.SpatialPolygonsDataFrame, level_2) %>%
  st_as_sf()

species_data <- read_rds(here("data_clean", "species_data.rds"))
rodent_spatial <- read_rds(here("data_clean", "rodent_spatial.rds")) %>%
  filter(iso3c %in% wa_countries)
studies <- read_rds(here("data_clean", "studies.rds"))

count_species <- species_data %>%
  filter(iso3c %in% wa_countries) %>%
  group_by(species_gbif, classification) %>%
  drop_na(species_gbif) %>%
  summarise(number = sum(number)) %>%
  mutate(percent = round(number/sum(.$number)*100, 2)) %>%
  arrange(-percent) %>%
  rename(`GBIF ID` = "species_gbif",
         "Classification" = "classification",
         "Number of individuals" = "number",
         "Percent (%)" = "percent") %>%
  mutate(Classification = snakecase::to_sentence_case(Classification)) # the number of individuals trapped identified to species level

top_6_species <- head(count_species$`GBIF ID`, 6)

top_6_spatial <- rodent_spatial %>%
  filter(species_gbif %in% top_6_species) %>%
  mutate(pres_abs = ifelse(number == 0, "Absent", "Present")) %>%
  left_join(., studies %>%
              dplyr::select(metric, unique_id, aim),
            by = "unique_id") %>%
  mutate(abundance = ifelse(metric == "presence", NA, number),
         classification = snakecase::to_sentence_case(classification))

afr_bbox <- st_bbox(level_0 %>%
          filter(GID_0 %in% wa_mainland))

top_6_plot <- tm_shape(level_0 %>%
                         filter(GID_0 %in% wa_mainland), bbox = afr_bbox) +  tm_polygons(alpha = 0, lwd = 1) +
  tm_shape(top_6_spatial) + tm_dots(col = "pres_abs", palette = "Dark2", title = "", size = .1, shape = 20) +
  tm_facets(by = "classification")

tmap_save(top_6_plot, here("figures", "top_6.png"))

# library("maptools")
# library("spatstat")
# t6_sf_utm <- st_transform(top_6_spatial %>%
#                             filter(classification == "Mastomys natalensis" & pres_abs == "Present"),
#                           32619) # project from geographic to UTM
# t6_sp  <- as(t6_sf_utm, "Spatial")  # Create Spatial* object
# t6_ppp <- as(t6_sp, "ppp")
# t6_ppp$marks <- NULL
#
# wa_sp <- level_0 %>%
#   filter(GID_0 %in% wa_mainland) %>%
#   st_union() %>%
#   st_cast("POLYGON") %>%
#   st_transform(32619)
#
# wa_ppp <- as(as_Spatial(wa_sp), "owin")
#
# Window(t6_ppp) <- wa_ppp
#
# K1 <- density(t6_ppp) # Using the default bandwidth
# plot(K1, main=NULL, las=1)
# contour(K1, add=TRUE)
