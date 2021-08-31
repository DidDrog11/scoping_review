source(here::here("scripts", "libraries.R"))

all_countries <- c("BEN", "BFA", "CIV", "CMR", "CPV", "DZA", "ESH", "GHA",
                   "GIN", "GMB", "GNB", "LBR", "MAR", "MLI", "MRT", "NER",
                   "NGA", "SEN", "SLE", "TCD", "TGO")
wa_countries <- c("BEN", "BFA", "CIV", "CPV", "ESH", "GHA",
                  "GIN", "GMB", "GNB", "LBR", "MLI", "MRT",
                  "NER", "NGA", "SEN", "SLE", "TGO")
no_data_countries <- c("GMB", "TGO")

level_0 <- read_rds(here("data_download", "admin_spatial", "level_0_admin.rds"))

level_2 <- read_rds(here("data_download", "admin_spatial", "level_2_admin.rds"))

studies <- read_rds(here("data_clean", "studies.rds"))

rodent_spatial <- read_rds(here("data_clean", "rodent_spatial.rds")) %>%
  select(-trap_nights)
species_data <- read_rds(here("data_clean", "species_data.rds"))
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

top_7_species <- head(count_species$`GBIF ID`, 7)

top_7_spatial <- rodent_spatial %>%
  filter(species_gbif %in% top_7_species) %>%
  mutate(pres_abs = ifelse(number == 0, "Absent", "Present")) %>%
  left_join(., studies %>%
              dplyr::select(metric, unique_id, aim),
            by = "unique_id") %>%
  mutate(abundance = ifelse(metric == "presence", NA, number),
         classification = snakecase::to_sentence_case(classification))

afr_bbox <- st_bbox(level_0 %>%
                      filter(GID_0 %in% wa_countries))

top_7_plot <- tm_shape(level_0 %>%
                         filter(GID_0 %in% wa_countries), bbox = afr_bbox) +  tm_polygons(alpha = 0, lwd = 1) +
  tm_shape(top_7_spatial) + tm_dots(col = "pres_abs", palette = "Dark2", title = "", size = .1, shape = 20) +
  tm_facets(by = "classification")

# Figure 3 ----------------------------------------------------------------

m_nat_trap <- rodent_spatial %>%
  filter(classification == "mastomys natalensis" &
           iso3c %in% wa_countries) %>%
  mutate(pres_abs = ifelse(number == 0, "Absent", "Present"),
         source = "This review")

tm_shape(level_0 %>%
           filter(GID_0 %in% wa_countries), bbox = afr_bbox) +
  tm_polygons(alpha = 0, lwd = 1) +
  tm_shape(m_nat_trap) +
  tm_dots(col = "pres_abs", palette = "Dark2", title = "", size = .1, shape = 20)
