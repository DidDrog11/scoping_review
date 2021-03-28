source(here::here("scripts", "libraries.r"))

studies <- read_rds(here("data_clean", "studies.rds"))

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

pathogen <- read_rds(here("data_clean", "pathogen.rds")) %>%
  filter(iso3c %in% wa_mainland)

pathogen_tested <- c("path_1", "path_2", "path_3", "path_4", "path_5", "path_6")
pcr_test <- c("pcr_path_1_positive", "pcr_path_2_positive", "pcr_path_3_positive", "pcr_path_4_positive", "pcr_path_5_positive", "pcr_path_6_positive")
ab_ag_test <- c("ab_ag_path_1_positive", "ab_ag_path_2_positive", "ab_ag_path_3_positive", "ab_ag_path_4_positive", "ab_ag_path_5_positive")
culture_test <- c("culture_path_1_positive", "culture_path_1_positive", "culture_path_1_positive")
direct_visualisation <- c("histo_path_1_positive", "histo_path_2_positive", "histo_path_3_positive", "histo_path_4_positive", "histo_path_5_positive", "histo_path_6_positive")

arenaviridae <- c("arenaviridae_species", "lassa_mammarenavirus", "mammarenavirus_species")

arenavirus_map <- pathogen %>%
  filter(path_1_tested != 0) %>%
  filter(path_1 %in% arenaviridae | path_2 %in% arenaviridae | path_3 %in% arenaviridae | path_5 %in% arenaviridae) %>%
  janitor::remove_empty("cols")  %>%
  pivot_longer(cols = c("path_1", "path_2", "path_3"), values_to = "pathogen") %>%
  drop_na(pathogen) %>%
  filter(pathogen %in% arenaviridae) %>%
  mutate(p_pos = if_else(name == "path_1",
                         if_else(!is.na(pcr_path_1_positive), pcr_path_1_positive/path_1_tested*100,
                                 ifelse(!is.na(ab_ag_path_1_positive), ab_ag_path_1_positive/path_1_tested*100,
                                        ifelse(!is.na(culture_path_1_positive), culture_path_1_positive/path_1_tested*100,
                                               ifelse(!is.na(pcr_path_2_positive), pcr_path_2_positive/path_2_tested*100,
                                                      ifelse(!is.na(ab_ag_path_2_positive), ab_ag_path_2_positive/path_2_tested*100, 0))))), 0),
         pos_neg = if_else(p_pos > 0, "positive", "negative"),
         sample = path_1_tested + path_2_tested) %>%
  st_as_sf()

tm_shape(level_0 %>%
           filter(GID_0 %in% wa_mainland)) +
  tm_polygons() +
  tm_shape(arenavirus_map) +
  tm_dots(col = "pos_neg", palette = "Dark2", size = 0.5, alpha = 0.4, title = "Arenaviridae")

borrelia <- c("borrelia_species")

borrelia_map <- pathogen %>%
  filter(path_1_tested != 0) %>%
  filter(path_1 %in% borrelia | path_2 %in% borrelia) %>%
  janitor::remove_empty("cols")  %>%
  pivot_longer(cols = c("path_1", "path_2"), values_to = "pathogen") %>%
  dplyr::select(-c("path_3", "path_4", "path_5", "path_6")) %>%
  drop_na(pathogen) %>%
  filter(pathogen %in% borrelia) %>%
  mutate(p_pos = if_else(name == "path_1",
                         if_else(!is.na(pcr_path_1_positive), pcr_path_1_positive/path_1_tested*100,
                                 ifelse(!is.na(ab_ag_path_1_positive), ab_ag_path_1_positive/path_1_tested*100,
                                        ifelse(!is.na(histo_path_1_positive), histo_path_1_positive/path_1_tested*100,
                                               ifelse(!is.na(pcr_path_2_positive), pcr_path_2_positive/path_2_tested*100, 0)))), 0),
         pos_neg = if_else(p_pos > 0, "positive", "negative"),
         sample = ifelse(is.na(path_2_tested), path_1_tested, path_1_tested + path_2_tested)) %>%
  st_as_sf()

tm_shape(level_0 %>%
           filter(GID_0 %in% wa_mainland)) +
  tm_polygons() +
  tm_shape(borrelia_map) +
  tm_dots(col = "pos_neg", palette = "Dark2", size = 0.5, alpha = 0.4, title = "Borrelia")

bartonella <- c("bartonella_species")

bartonella_map <- pathogen %>%
  filter(path_1_tested != 0) %>%
  filter(path_1 %in% bartonella) %>%
  janitor::remove_empty("cols")  %>%
  pivot_longer(cols = c("path_1"), values_to = "pathogen") %>%
  dplyr::select(-c("path_2", "path_3", "path_4", "path_5", "path_6")) %>%
  drop_na(pathogen) %>%
  filter(pathogen %in% bartonella) %>%
  mutate(p_pos = if_else(!is.na(pcr_path_1_positive), pcr_path_1_positive/path_1_tested*100, 0),
         pos_neg = if_else(p_pos > 0, "positive", "negative"),
         sample = path_1_tested) %>%
  st_as_sf()

tm_shape(level_0 %>%
           filter(GID_0 %in% wa_mainland)) +
  tm_polygons() +
  tm_shape(bartonella_map) +
  tm_dots(col = "pos_neg", palette = "Dark2", size = 0.5, alpha = 0.4, title = "Bartonella")

toxo <- c("toxoplasma_gondii")

toxo_map <- pathogen %>%
  filter(path_1_tested != 0) %>%
  filter(path_1 %in% toxo | path_6 %in% toxo) %>%
  janitor::remove_empty("cols")  %>%
  pivot_longer(cols = c("path_1", "path_6"), values_to = "pathogen") %>%
  dplyr::select(-c("path_3", "path_4", "path_5", "path_2")) %>%
  drop_na(pathogen) %>%
  filter(pathogen %in% toxo) %>%
  mutate(p_pos = if_else(name == "path_1",
                         ifelse(!is.na(pcr_path_1_positive), pcr_path_1_positive/path_1_tested*100,
                                ifelse(!is.na(ab_ag_path_1_positive), ab_ag_path_1_positive/path_1_tested*100, 0)),
                         histo_path_6_positive/path_6_tested*100),
         pos_neg = if_else(p_pos > 0, "positive", "negative"),
         sample = path_1_tested) %>%
  st_as_sf()

tm_shape(level_0 %>%
           filter(GID_0 %in% wa_mainland)) +
  tm_polygons() +
  tm_shape(toxo_map) +
  tm_dots(col = "pos_neg", palette = "Dark2", size = 0.5, alpha = 0.4, title = "Toxoplasmosis")

all_path_map <- bind_rows(arenavirus_map, borrelia_map, bartonella_map, toxo_map) %>%
  mutate(pathogen = recode(pathogen, "arenaviridae_species" = "arenaviridae",
                           "lassa_mammarenavirus" = "arenaviridae",
                           "mammarenavirus_species" = "arenaviridae"))

afr_bbox <- st_bbox(level_0 %>%
                      filter(GID_0 %in% wa_mainland))

all_path_maps <- tm_shape(level_0 %>%
             filter(GID_0 %in% wa_mainland),  bbox = afr_bbox) +
  tm_polygons() +
  tm_shape(all_path_map) +
  tm_dots(col = "pos_neg", palette = "Dark2", size = 0.1, alpha = 0.4, title = "") +
  tm_facets(by = "pathogen")

tmap_save(all_path_maps, here("figures", "top_4.png"))
