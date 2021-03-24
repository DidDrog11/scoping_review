source(here::here("scripts", "libraries.r"))

wa_mainland <- c("BEN", "BFA", "CIV", "ESH", "GHA",
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

rodent_spatial <- read_rds(here("data_clean", "rodent_spatial.rds")) %>%
  filter(iso3c %in% wa_countries)
studies <- read_rds(here("data_clean", "studies.rds"))

top_6_species <- c(2438911, 2438912, 2439270, 7429082, 9733997, 4264823)

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
