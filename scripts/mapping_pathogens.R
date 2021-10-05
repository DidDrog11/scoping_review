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
toxoplasma <- c("toxoplasma_gondii")

four_paths <- vctrs::vec_c(arenaviridae, borrelia, bartonella, toxoplasma)

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
                         toxoplasma = c("toxoplasma_gondii"))

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
                         st_as_sf() %>%
                         mutate(x = round(st_coordinates(.)[,1], 3),
                                y = round(st_coordinates(.)[,2], 3),
                                genus = snakecase::to_sentence_case(genus),
                                `Species` = snakecase::to_sentence_case(classification),
                                pos_neg = case_when(pos_neg == "Positive" ~ 1,
                                                    TRUE ~ 0),
                                unique_positive = case_when(pcr_positive == 0 ~ ab_ag_positive + culture_positive,
                                                            pcr_positive != 0 & ab_ag_positive != 0 ~ ab_ag_positive + culture_positive,
                                                            pcr_positive != 0 & ab_ag_positive == 0 ~ pcr_positive + culture_positive,
                                                            TRUE ~ ab_ag_positive + culture_positive)) %>%
                         group_by(x, y) %>%
                         summarise(number_tested = sum(number_tested),
                                   number_positive = sum(unique_positive),
                                   number_negative = number_tested-number_positive,
                                   prop_positive = number_positive/number_tested) %>%
                         mutate(pos_neg = case_when(number_positive > 0 ~ "Positive",
                                                    TRUE ~ "Negative"))
  )

  species = list(rodents = four_paths_wide %>%
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
                   tibble() %>%
                   mutate(genus = snakecase::to_sentence_case(genus),
                          `Species` = snakecase::to_sentence_case(classification),
                          pos_neg = case_when(pos_neg == "Positive" ~ 1,
                                              TRUE ~ 0)) %>%
                   mutate(unique_positive = case_when(pcr_positive == 0 ~ ab_ag_positive + culture_positive,
                                                      pcr_positive != 0 & ab_ag_positive != 0 ~ ab_ag_positive + culture_positive,
                                                      pcr_positive != 0 & ab_ag_positive == 0 ~ pcr_positive + culture_positive,
                                                      TRUE ~ ab_ag_positive + culture_positive)) %>%
                   group_by(genus, `Species`) %>%
                   summarise(`Tested` = sum(number_tested),
                             `Positive` = sum(unique_positive),
                             `Negative` = `Tested`-`Positive`) %>%
                   mutate(`Prop. positive` = round(`Positive`/`Tested`, 3)) %>%
                   ungroup() %>%
                   arrange(-`Positive`, -`Tested`) %>%
                   head(10) %>%
                   group_by(genus) %>%
                   mutate(genus_test = sum(`Tested`)) %>%
                   arrange(-genus_test) %>%
                   ungroup() %>%
                   select(-genus_test, -genus))


  plots = list(pos_neg_plot =
                 ggplot() +
                 geom_sf(data = included_countries, fill = "#808080", alpha = 0.1, lwd = 0.1) +
                 geom_point(data = pathogen_data$pathogen,
                            mapping = aes(x = x, y = y, colour = pos_neg, size = prop_positive), alpha = 0.5) +
                 labs(colour = "",
                      size = "Proportion positive",
                      title = paste0(snakecase::to_sentence_case(pathogen_genus)),
                      x = element_blank(),
                      y = element_blank()) +
                 scale_colour_manual(values = c("#ff8c00", "#440154")) +
                 annotation_north_arrow(height = unit(1, "cm"),
                                        style = north_arrow_minimal(text_size = 8)) +
                 annotation_scale(height = unit(0.1, "cm"),
                                  location = "tr") +
                 theme_minimal(),

               species_table = tableGrob(species$rodents, theme = ttheme_minimal(), rows = NULL)
               )

  legend = list(
    get_legend(plots$pos_neg_plot  +
                 theme(legend.position = "bottom"))
  )

  output = list(plots$pos_neg_plot +
                  theme(legend.position = "none"),
                plots$species_table,
                legend)

  return(output)
}

arenaviridae_plots <- pathogen_map("arenaviridae")
av_row <- plot_grid(plotlist = arenaviridae_plots[c(1:2)], nrow = 1)
borrelia_plots <- pathogen_map("borrelia")
bo_row <- plot_grid(plotlist = borrelia_plots[c(1:2)], nrow = 1)
bartonella_plots <- pathogen_map("bartonella")
ba_row <- plot_grid(plotlist = bartonella_plots[c(1:2)], nrow = 1)
toxplasma_plots <- pathogen_map("toxoplasma")
to_row <- plot_grid(plotlist = toxplasma_plots[c(1:2)], nrow = 1)
legend <- plot_grid(as_grob(arenaviridae_plots[[3]][[1]]), rel_heights = 0.5)

save_plot(plot_grid(plotlist = list(av_row,
                                    ba_row),
                    ncol = 1,
                    greedy = FALSE,
                    rel_heights = c(1, 1)),
          filename = here("figures", "Figure_5a.png"), base_height = 10, base_width = 16)

save_plot(plot_grid(plotlist = list(bo_row,
                                    to_row),
                    ncol = 1,
                    greedy = FALSE,
                    rel_heights = c(1, 1)),
          filename = here("figures", "Figure_5b.png"), base_height = 10, base_width = 16)

save_plot(plot_grid(plot = legend,
                    ncol = 1,
                    greedy = FALSE,
                    rel_heights = c(1)),
          filename = here("figures", "Figure_5_legend.png"), base_height = 2, base_width = 10)

save_plot(plot_grid(plot_grid(plotlist = list(av_row,
                                    ba_row),
                    ncol = 1,
                    greedy = FALSE,
                    rel_heights = c(1, 1)),
          plot_grid(plotlist = list(bo_row,
                                    to_row),
                    ncol = 1,
                    greedy = FALSE,
                    rel_heights = c(1, 1)),
          plot_grid(plot = legend,
                    ncol = 1,
                    greedy = FALSE,
                    rel_heights = c(1)),
          rel_heights = c(1, 1, 0.2),
          rel_widths = c(1, 1, 0.6)),
          filename = here("figures", "Figure_5_combined.png"),
          base_height = 20,
          base_width = 32)
