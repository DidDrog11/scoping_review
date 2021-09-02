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
rodent_iucn <- st_read(here("data_download", "iucn_data", "data_0.shp")) %>%
  bind_rows(st_read(here("data_download", "iucn_data", "data_1.shp")))

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


# M natalensis ------------------------------------------------------------

included_countries <- level_0 %>%
  filter(GID_0 %in% wa_countries)

plot_species <- function(species_name) {

  species_name = tolower(species_name)

  data <- list(
    review = rodent_spatial %>%
      filter(classification == species_name &
               iso3c %in% wa_countries) %>%
      mutate(pres_abs = ifelse(number == 0, "Absent", "Present"),
             source = "This review",
             x = st_coordinates(.)[,1],
             y = st_coordinates(.)[,2]),
    iucn = rodent_iucn %>%
      filter(BINOMIAL == snakecase::to_sentence_case(species_name)),
    gbif = read_tsv(here("data_download", "gbif_species", paste0(gsub(" ", "_", species_name), "_gbif.csv"))) %>%
      select(gbifID, species, countryCode, decimalLatitude, decimalLongitude))

  data$gbif <- data$gbif  %>%
    distinct(decimalLatitude, decimalLongitude) %>%
    drop_na(decimalLatitude, decimalLongitude) %>%
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude")) %>%
    st_set_crs(., value = st_crs(included_countries)) %>%
    st_filter(., included_countries) %>%
    mutate(x = st_coordinates(.)[,1],
           y = st_coordinates(.)[,2],
           presence = "Present")

  data$gbif_review <- data$gbif %>%
    mutate(source = "GBIF") %>%
    bind_rows(., data$review %>%
                filter(pres_abs == "Present") %>%
                distinct(geometry) %>%
                mutate(x = st_coordinates(.)[,1],
                       y = st_coordinates(.)[,2],
                       source = "Review"))

  plots <- list(
    review = ggplot() +
      geom_sf(data = included_countries, fill = "#808080", alpha = 0.1, lwd = 0.1) +
      geom_point(data = data$review %>%
                   filter(pres_abs == "Present"), mapping = aes(colour = pres_abs, x = x, y = y), size = .5,
                 crs = 4326) +
      geom_spatial_point(data = anti_join(tibble(data$review), tibble(data$review) %>%
                                            filter(pres_abs == "Present"),
                                          by = c("x", "y")),
                         mapping = aes(colour = pres_abs, x = x, y = y), size = .5,
                         crs = 4326) +
      scale_colour_manual(values = c("#ff8c00", "#440154")) +
      labs(colour = "",
           title = paste0(snakecase::to_sentence_case(species_name), " - this review"),
           x = element_blank(),
           y = element_blank()) +
      annotation_north_arrow(height = unit(1, "cm"),
                             style = north_arrow_minimal(text_size = 8)) +
      annotation_scale(height = unit(0.1, "cm"),
                       location = "tr") +
      theme_minimal(),

    gbif_occurrence = ggplot() +
      geom_sf(data = included_countries, fill = "#808080", alpha = 0.1, lwd = 0.1) +
      geom_point(data = data$gbif,
                 mapping = aes(x = x, y = y, colour = presence), size = .5,
                 crs = 4326) +
      scale_colour_manual(values = c("#006400")) +
      labs(colour = "",
           title = paste0(snakecase::to_sentence_case(species_name), "- GBIF (2021)"),
           x = element_blank(),
           y = element_blank()) +
      annotation_north_arrow(height = unit(1, "cm"),
                             style = north_arrow_minimal(text_size = 8)) +
      annotation_scale(height = unit(0.1, "cm"),
                       location = "tr") +
      theme_minimal(),

    iucn_plot = ggplot() +
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
      theme_minimal()
    )

  legends = list(
    list(review = get_legend(plots$review),
         gbif = get_legend(plots$gbif_occurrence),
         iucn = get_legend(plots$iucn_plot)))

  species_plots <- list(plots$review +
                          theme(legend.position = "none"),
                        plots$gbif_occurrence +
                          theme(legend.position = "none"),
                        plots$iucn_plot +
                          theme(legend.position = "none"),
                        legends)

  return(species_plots)
}

mastomys_natalensis_plots <- plot_species("Mastomys natalensis")
mnat_row <- plot_grid(plotlist = mastomys_natalensis_plots[c(1:3)], nrow = 1)
rattus_rattus_plots <- plot_species("Rattus rattus")
rrat_row <- plot_grid(plotlist = rattus_rattus_plots[c(1:3)], nrow = 1)
mastomys_erythroleucus_plots <- plot_species("Mastomys erythroleucus")
mery_row <- plot_grid(plotlist = mastomys_erythroleucus_plots[c(1:3)], nrow = 1)
mus_musculus_plots <- plot_species("Mus musculus")
mmus_row <- plot_grid(plotlist = mus_musculus_plots[c(1:3)], nrow = 1)
arvicanthis_niloticus_plots <- plot_species("Arvicanthis niloticus")
anil_row <- plot_grid(plotlist = arvicanthis_niloticus_plots[c(1:3)], nrow = 1)
mastomys_huberti_plots <- plot_species("Mastomys huberti")
mhub_row <- plot_grid(plotlist = mastomys_huberti_plots[c(1:3)], nrow = 1)
praomys_daltoni_plots <- plot_species("Praomys daltoni")
pdal_row <- plot_grid(plotlist = praomys_daltoni_plots[c(1:3)], nrow = 1)
legend_row <- plot_grid(plotlist = praomys_daltoni_plots[[4]][[1]][c(1:3)], nrow = 1)

fig_3 <- list(mnat_row,
              rrat_row,
              mery_row,
              mmus_row,
              anil_row,
              mhub_row,
              pdal_row,
              legend_row)

save_plot(plot_grid(plotlist = fig_3,
                    ncol = 1,
                    greedy = TRUE,
                    rel_heights = c(1, 1, 1, 1, 1, 1, 1, 0.5)),
          filename = here("figures", "Figure_3.png"), base_height = 18, base_width = 14)
