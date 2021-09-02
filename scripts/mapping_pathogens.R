source(here::here("scripts", "libraries.R"))

studies <- read_rds(here("data_clean", "studies.rds"))

wa_mainland <- c("BEN", "BFA", "CIV", "ESH", "GHA",
                 "GIN", "GMB", "GNB", "LBR", "MLI", "MRT",
                 "NER", "NGA", "SEN", "SLE", "TGO")

level_0 <- read_rds(here("data_download", "admin_spatial", "level_0_admin.rds"))

level_1 <- read_rds(here("data_download", "admin_spatial", "level_1_admin.rds"))

level_2 <- read_rds(here("data_download", "admin_spatial", "level_2_admin.rds"))

pathogen <- read_rds(here("data_clean", "pathogen.rds")) %>%
  filter(iso3c %in% wa_mainland)
wide_pathogen <- read_rds(here("data_clean", "wide_pathogen.rds")) %>%
  filter(iso3c %in% wa_mainland)

species_data <- read_rds(here("data_clean", "species_data.rds")) %>%
  distinct(genus, species, classification, gbif_id, genus_gbif, species_gbif)

pathogen <- pathogen %>%
  left_join(., species_data,
            by = "gbif_id")

pathogen_tested <- c("path_1", "path_2", "path_3", "path_4", "path_5", "path_6")
pcr_test <- c("pcr_path_1_positive", "pcr_path_2_positive", "pcr_path_3_positive", "pcr_path_4_positive", "pcr_path_5_positive", "pcr_path_6_positive")
ab_ag_test <- c("ab_ag_path_1_positive", "ab_ag_path_2_positive", "ab_ag_path_3_positive", "ab_ag_path_4_positive", "ab_ag_path_5_positive")
culture_test <- c("culture_path_1_positive", "culture_path_1_positive", "culture_path_1_positive")
direct_visualisation <- c("histo_path_1_positive", "histo_path_2_positive", "histo_path_3_positive", "histo_path_4_positive", "histo_path_5_positive", "histo_path_6_positive")

arenaviridae <- c("arenaviridae_species", "lassa_mammarenavirus", "mammarenavirus_species")
borrelia <- c("borrelia_species", "borrelia")
bartonella <- c("bartonella_species")
toxo <- c("toxoplasma_gondii")

four_paths <- vctrs::vec_c(arenaviridae, borrelia, bartonella, toxo)

four_paths_wide <- wide_pathogen %>%
  tibble() %>%
  dplyr::select(1:16, matches(four_paths)) %>%
  left_join(., species_data,
            by = c("gbif_id", "classification")) %>%
  distinct(record_id, .keep_all = T)

pathogen_map <- function(pathogen_genus) {

  included_countries = level_0 %>%
    filter(GID_0 %in% wa_mainland)

  pathogen_groups = list(arenaviridae = c("arenaviridae_species", "lassa_mammarenavirus", "mammarenavirus_species"),
                         borrelia = c("borrelia_species", "borrelia"),
                         bartonella = c("bartonella_species"),
                         toxo = c("toxoplasma_gondii"))

  pathogen_data = list(pathogen = four_paths_wide %>%
                         dplyr::select(1:16, matches(c(pathogen_groups[[pathogen_genus]])),
                                       genus, species, genus_gbif, species_gbif) %>%
                         janitor::remove_empty("cols")  %>%
                         mutate(number_tested = rowSums(.[grep("tested", names(.))], na.rm = T)) %>%
                         filter(number_tested != 0) %>%
                         mutate(pcr_positive = rowSums(.[grep("pcr", names(.))], na.rm = T),
                                ab_ag_positive = rowSums(.[grep("ab_ag", names(.))], na.rm = T),
                                culture_positive = rowSums(.[grep("culture", names(.))], na.rm = T),
                                pos_neg = case_when(pcr_positive + ab_ag_positive + culture_positive > 0 ~ "Positive",
                                                    TRUE ~ "Negative")) %>%
                         dplyr::select(1:15, all_of(c("number_tested", "pcr_positive", "ab_ag_positive", "culture_positive", "pos_neg")),
                                       genus, species, genus_gbif, species_gbif) %>%
                         st_as_sf())

  species = list(rodents = pathogen_data$pathogen %>%
                   tibble() %>%
                   mutate(genus = snakecase::to_sentence_case(genus),
                          species = snakecase::to_sentence_case(classification),
                          pos_neg = case_when(pos_neg == "Positive" ~ 1,
                                              TRUE ~ 0)) %>%
                   group_by(genus, species) %>%
                   summarise(number_tested = sum(number_tested),
                             number_positive = sum(pos_neg),
                             number_negative = number_tested-number_positive))


  plots = list(pos_neg_plot =
                 ggplot() +
                 geom_sf(data = included_countries, fill = "#808080", alpha = 0.1, lwd = 0.1) +
                 geom_point(data = pathogen_data$pathogen %>%
                                      distinct(geometry, pos_neg) %>%
                                      mutate(x = st_coordinates(.)[,1],
                                             y = st_coordinates(.)[,2]),
                                    mapping = aes(x = x, y = y, colour = pos_neg))


               geom_sf(data = included_countries, fill = "#808080", alpha = 0.1, lwd = 0.1) +
                 geom_point(data = data$gbif_review,
                            mapping = aes(x = x, y = y, colour = source), size = .5,
                            crs = 4326) +
                 geom_sf(data = data$iucn,
                         fill = "#C12D20", alpha = 0.4) +
                 coord_sf(xlim = c(afr_bbox[1], afr_bbox[3]), ylim = c(afr_bbox[2], afr_bbox[4])) +
                 labs(colour = "",
                      title = paste0(snakecase::to_sentence_case(species_name), "- IUCN range"),
                      x = element_blank(),
                      y = element_blank()) +
                 scale_colour_manual(values = c("#006400", "#440154")) +
                 annotation_north_arrow(height = unit(1, "cm"),
                                        style = north_arrow_minimal(text_size = 8)) +
                 annotation_scale(height = unit(0.1, "cm"),
                                  location = "tr") +
                 theme_minimal())
}

arenavirus_map <- four_paths_wide %>%
  dplyr::select(1:16, matches(arenaviridae), any_of(names(species_data))) %>%
  janitor::remove_empty("cols")  %>%
  mutate(number_tested = rowSums(.[grep("tested", names(.))], na.rm = T)) %>%
  filter(number_tested != 0) %>%
  mutate(pcr_positive = rowSums(.[grep("pcr", names(.))], na.rm = T),
         ab_ag_positive = rowSums(.[grep("ab_ag", names(.))], na.rm = T),
         culture_positive = rowSums(.[grep("culture", names(.))], na.rm = T),
         pos_neg = case_when(pcr_positive + ab_ag_positive + culture_positive > 0 ~ "Positive",
                             TRUE ~ "Negative")) %>%
  dplyr::select(1:15, any_of(names(species_data)), all_of(c("number_tested", "pcr_positive", "ab_ag_positive", "culture_positive", "pos_neg"))) %>%
  st_as_sf()

arenavirus_species <- arenavirus_map %>%
  tibble() %>%
  mutate(genus = snakecase::to_sentence_case(genus)) %>%
  count(genus, sort = T)

arenavirus_species_palette <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
                                "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a")
names(arenavirus_species_palette) <- c(head(arenavirus_species$genus, 9), "Other")

arenavirus_map <- arenavirus_map %>%
  mutate(genus = snakecase::to_sentence_case(genus),
         genus = case_when(!genus %in% head(arenavirus_species$genus, 9) ~ "Other",
                           TRUE ~ genus))

a_map <- tm_shape(level_0 %>%
           filter(GID_0 %in% wa_mainland)) +
  tm_polygons(alpha = 0.5) +
  tm_shape(arenavirus_map) +
  tm_dots(col = "genus", palette = arenavirus_species_palette, size = 0.2,
          jitter = 0.1, alpha = 1, legend.show = F) +
  tm_facets(by = "pos_neg", free.coords = F) +
  tm_layout(panel.labels = c("Arenavirus negative", "Arenavirus positive")) +
  tm_add_legend(type = "fill", col = arenavirus_species_palette, labels = names(arenavirus_species_palette),
                title = "Rodent genus")

tmap_save(a_map, here("figures", "arenavirus_map.png"))

borrelia_map <- four_paths_wide %>%
  dplyr::select(1:16, matches(borrelia), any_of(names(species_data))) %>%
  janitor::remove_empty("cols")  %>%
  mutate(number_tested = rowSums(.[grep("tested", names(.))], na.rm = T)) %>%
  filter(number_tested != 0) %>%
  mutate(pcr_positive = rowSums(.[grep("pcr", names(.))], na.rm = T),
         ab_ag_positive = rowSums(.[grep("ab_ag", names(.))], na.rm = T),
         culture_positive = rowSums(.[grep("culture", names(.))], na.rm = T),
         pos_neg = case_when(pcr_positive + ab_ag_positive + culture_positive > 0 ~ "Positive",
                             TRUE ~ "Negative")) %>%
  dplyr::select(1:15, any_of(names(species_data)), all_of(c("number_tested", "pcr_positive", "ab_ag_positive", "culture_positive", "pos_neg"))) %>%
  st_as_sf()

borrelia_species <- borrelia_map %>%
  tibble() %>%
  mutate(genus = snakecase::to_sentence_case(genus)) %>%
  count(genus, sort = T)

borrelia_species_palette <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
                                "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a")
names(borrelia_species_palette) <- c(head(borrelia_species$genus, 9), "Other")

borrelia_map <- borrelia_map %>%
  mutate(genus = snakecase::to_sentence_case(genus),
         genus = case_when(!genus %in% head(borrelia_species$genus, 9) ~ "Other",
                           TRUE ~ genus))

bo_map <- tm_shape(level_0 %>%
           filter(GID_0 %in% wa_mainland)) +
  tm_polygons(alpha = 0.5) +
  tm_shape(borrelia_map) +
  tm_dots(col = "genus", palette = borrelia_species_palette, size = 0.2,
          jitter = 0.1, alpha = 1, legend.show = F) +
  tm_facets(by = "pos_neg", free.coords = F) +
  tm_layout(panel.labels = c("Borrelia negative", "Borrelia positive")) +
  tm_add_legend(type = "fill", col = borrelia_species_palette, labels = names(borrelia_species_palette),
                title = "Rodent genus")

tmap_save(bo_map, here("figures", "borrelia_map.png"))

bartonella_map <- four_paths_wide %>%
  dplyr::select(1:16, matches(bartonella), any_of(names(species_data))) %>%
  janitor::remove_empty("cols")  %>%
  mutate(number_tested = rowSums(.[grep("tested", names(.))], na.rm = T)) %>%
  filter(number_tested != 0) %>%
  mutate(pcr_positive = rowSums(.[grep("pcr", names(.))], na.rm = T),
         ab_ag_positive = rowSums(.[grep("ab_ag", names(.))], na.rm = T),
         culture_positive = rowSums(.[grep("culture", names(.))], na.rm = T),
         pos_neg = case_when(pcr_positive + ab_ag_positive + culture_positive > 0 ~ "Positive",
                             TRUE ~ "Negative")) %>%
  dplyr::select(1:15, any_of(names(species_data)), all_of(c("number_tested", "pcr_positive", "ab_ag_positive", "culture_positive", "pos_neg"))) %>%
  st_as_sf()

bartonella_species <- bartonella_map %>%
  tibble() %>%
  mutate(genus = snakecase::to_sentence_case(genus)) %>%
  count(genus, sort = T)

bartonella_species_palette <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
                              "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a")
names(bartonella_species_palette) <- c(head(bartonella_species$genus, 9), "Other")

bartonella_map <- bartonella_map %>%
  mutate(genus = snakecase::to_sentence_case(genus),
         genus = case_when(!genus %in% head(bartonella_species$genus, 9) ~ "Other",
                           TRUE ~ genus))

ba_map <- tm_shape(level_0 %>%
                    filter(GID_0 %in% wa_mainland)) +
  tm_polygons(alpha = 0.5) +
  tm_shape(bartonella_map) +
  tm_dots(col = "genus", palette = bartonella_species_palette, size = 0.2,
          jitter = 0.1, alpha = 1, legend.show = F) +
  tm_facets(by = "pos_neg", free.coords = F) +
  tm_layout(panel.labels = c("Bartonella negative", "Bartonella positive")) +
  tm_add_legend(type = "fill", col = bartonella_species_palette, labels = names(bartonella_species_palette),
                title = "Rodent genus")

toxoplasma_map <- four_paths_wide %>%
  dplyr::select(1:16, matches(toxo), any_of(names(species_data))) %>%
  janitor::remove_empty("cols")  %>%
  mutate(number_tested = rowSums(.[grep("tested", names(.))], na.rm = T)) %>%
  filter(number_tested != 0) %>%
  mutate(pcr_positive = rowSums(.[grep("pcr", names(.))], na.rm = T),
         ab_ag_positive = rowSums(.[grep("ab_ag", names(.))], na.rm = T),
         culture_positive = rowSums(.[grep("culture", names(.))], na.rm = T),
         pos_neg = case_when(pcr_positive + ab_ag_positive + culture_positive > 0 ~ "Positive",
                             TRUE ~ "Negative")) %>%
  dplyr::select(1:15, any_of(names(species_data)), all_of(c("number_tested", "pcr_positive", "ab_ag_positive", "culture_positive", "pos_neg"))) %>%
  st_as_sf()

toxoplasma_species <- toxoplasma_map %>%
  tibble() %>%
  mutate(genus = snakecase::to_sentence_case(genus)) %>%
  count(genus, sort = T)

toxoplasma_species_palette <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
                                "#e31a1c", "#ff7f00", "#6a3d9a")
names(toxoplasma_species_palette) <- c(head(toxoplasma_species$genus, 9), "Other")

toxoplasma_map <- toxoplasma_map %>%
  mutate(genus = snakecase::to_sentence_case(genus),
         genus = case_when(!genus %in% head(toxoplasma_species$genus, 9) ~ "Other",
                           TRUE ~ genus))

t_map <- tm_shape(level_0 %>%
                     filter(GID_0 %in% wa_mainland)) +
  tm_polygons(alpha = 0.5) +
  tm_shape(toxoplasma_map) +
  tm_dots(col = "genus", palette = toxoplasma_species_palette, size = 0.2,
          jitter = 0.1, alpha = 1, legend.show = F) +
  tm_facets(by = "pos_neg", free.coords = F) +
  tm_layout(panel.labels = c("Toxoplasma negative", "Toxoplasma positive")) +
  tm_add_legend(type = "fill", col = toxoplasma_species_palette, labels = names(toxoplasma_species_palette),
                title = "Rodent genus")

all_path_map <- four_paths_wide %>%
  dplyr::select(1:16, name, matches(four_paths), any_of(names(species_data))) %>%
  janitor::remove_empty("cols")  %>%
  mutate(number_tested = rowSums(.[grep("tested", names(.))], na.rm = T)) %>%
  filter(number_tested != 0) %>%
  mutate(pcr_positive = rowSums(.[grep("pcr", names(.))], na.rm = T),
         ab_ag_positive = rowSums(.[grep("ab_ag", names(.))], na.rm = T),
         culture_positive = rowSums(.[grep("culture", names(.))], na.rm = T),
         pos_neg = case_when(pcr_positive + ab_ag_positive + culture_positive > 0 ~ "Positive",
                             TRUE ~ "Negative")) %>%
  dplyr::select(1:15, any_of(names(species_data)), name, all_of(c("number_tested", "pcr_positive", "ab_ag_positive", "culture_positive", "pos_neg"))) %>%
  mutate(pathogen = case_when(name %in% arenaviridae ~ "Arenavirus",
                              name %in% borrelia ~ "Borrelia",
                              name %in% bartonella ~ "Bartonella",
                              name %in% toxo ~ "Toxoplasma",
                              TRUE ~ "Other")) %>%
  st_as_sf()

all_species <- all_path_map %>%
  tibble() %>%
  mutate(genus = snakecase::to_sentence_case(genus)) %>%
  count(genus, sort = T)

all_species_palette <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
                              "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a")
names(all_species_palette) <- c(head(all_species$genus, 9), "Other")

all_path_map <- all_path_map %>%
  mutate(genus = snakecase::to_sentence_case(genus),
         genus = case_when(!genus %in% head(all_species$genus, 9) ~ "Other",
                           TRUE ~ genus)) %>%
  st_as_sf()

all_path_fig <- tm_shape(level_0 %>%
           filter(GID_0 %in% wa_mainland)) +
  tm_polygons(alpha = 0.5) +
  tm_shape(all_path_map) +
  tm_dots(col = "genus", palette = all_species_palette, size = 0.05,
          jitter = 0.08, alpha = 1, legend.show = F) +
  tm_facets(by = c("pos_neg", "pathogen"), free.coords = F) +
  tm_layout() +
  tm_add_legend(type = "fill", col = all_species_palette, labels = names(all_species_palette),
                title = "Rodent genus")

tmap_save(all_path_fig, here("figures", "top_4.png"))

species <- all_path_map %>%
  tibble() %>%
  group_by(classification.x) %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  head(n = 10)

species_path <- all_path_map %>%
  filter(classification.x %in% c("mastomys natalensis", "mastomys erythroleucus", "crocidura sp.",
                                 "rattus rattus", "mus musculus", "praomys daltoni",
                                 "arvicanthis niloticus", "mus minutoides", "taterillus sp.",
                                 "mastomys huberti")) %>%
  filter(pos_neg == "Positive")


species_path_fig <- tm_shape(level_0 %>%
           filter(GID_0 %in% wa_mainland)) +
  tm_polygons(alpha = 0.5) +
  tm_shape(species_path) +
  tm_dots(col = "pos_neg", size = 0.05, palette = "black",
          jitter = 0.08, alpha = 1, legend.show = F) +
  tm_facets(by = c("pathogen", "classification.x"), free.coords = F) +
  tm_layout() +
  tm_add_legend(title = "Positive")

tmap_save(species_path_fig, here("figures", "top_10_species.png"))
