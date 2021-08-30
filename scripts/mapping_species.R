source(here::here("scripts", "libraries.r"))

level_2 <- read_rds(here("data_download", "admin_spatial", "level_2_admin.rds"))

studies <- read_rds(here("data_clean", "studies.rds"))

rodent_spatial <- read_rds(here("data_clean", "rodent_spatial.rds")) %>%
  select(-trap_nights)

rodent_iucn <- st_read(here("data_download", "iucn_data", "data_0.shp"))

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
