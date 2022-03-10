source(here::here("scripts", "libraries.R"))


# Data --------------------------------------------------------------------


all_countries <- c("BEN", "BFA", "CIV", "CMR", "CPV", "DZA", "ESH", "GHA",
                   "GIN", "GMB", "GNB", "LBR", "MAR", "MLI", "MRT", "NER",
                   "NGA", "SEN", "SLE", "TCD", "TGO")
wa_countries <- c("BEN", "BFA", "CIV", "CPV", "ESH", "GHA",
                  "GIN", "GMB", "GNB", "LBR", "MLI", "MRT",
                  "NER", "NGA", "SEN", "SLE", "TGO")
continental_countries <- c("BEN", "BFA", "CIV", "ESH", "GHA",
                  "GIN", "GMB", "GNB", "LBR", "MLI", "MRT",
                  "NER", "NGA", "SEN", "SLE", "TGO")
no_data_countries <- c("GMB", "TGO")

level_0 <- read_rds(here("data_download", "admin_spatial", "level_0_admin.rds"))

level_1 <- read_rds(here("data_download", "admin_spatial", "level_1_admin.rds"))

level_2 <- read_rds(here("data_download", "admin_spatial", "level_2_admin.rds"))

non_trapped <- read_rds(here("data_download", "admin_spatial", "level_2_TGOGMB.rds"))

included_countries <- level_0 %>%
  filter(GID_0 %in% continental_countries)

contiguous_boundary <- included_countries %>%
  filter(!GID_0 == "CPV") %>%
  st_union()

studies <- read_rds(here("data_clean", "studies.rds"))

rodent_spatial <- read_rds(here("data_clean", "rodent_spatial.rds")) %>%
  dplyr::select(-trap_nights)

imputed_tn <- read_rds(here("data_clean", "imputed_trap_nights.rds"))

rodent_spatial <- rodent_spatial %>%
  left_join(., imputed_tn,
            by = c("unique_id", "year_trapping", "month_trapping",
                   "region", "town_village", "habitat"))

bbox_rodent <- st_bbox(rodent_spatial)

# IUCN data
# As some of these ranges cross the dateline we use S1 methods
sf::sf_use_s2(FALSE)


rodent_iucn <- st_read(here("data_download", "iucn_data", "data_0.shp")) %>%
  bind_rows(st_read(here("data_download", "iucn_data", "data_1.shp"))) %>%
  mutate(classification = str_to_lower(BINOMIAL)) %>%
  select(classification, geometry) %>%
  filter(classification %in% rodent_spatial$classification) %>%
  group_by(classification) %>%
  summarise(geometry = st_union(geometry))

# GBIF data
# We limit the figures to the top 7 most commonly trapped species

species_names <- rodent_spatial %>%
  filter(!str_detect(classification, ".sp|.spp")) %>%
  distinct(classification, geometry) %>%
  tibble() %>%
  group_by(classification) %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  head(7) %>%
  pull(classification)

if(!file.exists(here("data_clean", "rodent_gbif_spatial.rds"))) {
  rodent_gbif <- list()

  for(i in 1:length(species_names)) {

    rodent_gbif[[i]] <- read_tsv(here("data_download", "gbif_species", paste0(gsub(" ", "_", species_names[[i]]), "_gbif.csv"))) %>%
      select(gbifID, species, countryCode, decimalLatitude, decimalLongitude)

  }

  names(rodent_gbif) <- c(species_names)

  rodent_gbif <- lapply(rodent_gbif, function(x) {

    x %>%
      drop_na(decimalLongitude, decimalLatitude) %>%
      st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = crs(included_countries)) %>%
      mutate(classification = str_to_lower(species)) %>%
      distinct(classification, geometry) %>%
      st_join(included_countries, st_within) %>%
      filter(!is.na(GID_0))

  })

  write_rds(rodent_gbif, here("data_clean", "rodent_gbif_spatial.rds"))

} else {

  rodent_gbif <- read_rds(here("data_clean", "rodent_gbif_spatial.rds"))

}

# Figure 1 ----------------------------------------------------------------

trap_site_mapping <- rodent_spatial[st_within(rodent_spatial, included_countries) %>% lengths > 0,]  %>%
  select(unique_id, year_trapping, month_trapping, region, town_village, habitat, geometry) %>%
  distinct() %>%
  left_join(., imputed_tn) %>%
  mutate(trap_nights_cat = cut(trap_nights, c(0, 100, 300, 500, 1000, 2000, 5000, 60000)))

fig_1a_updated <- trap_site_mapping %>%
  ggplot() +
  geom_sf(aes(colour = trap_nights_cat)) +
  geom_sf(data = level_0 %>%
            filter(GID_0 %in% wa_countries), alpha = 0) +
  scale_colour_viridis_d(direction = -1) +
  labs(colour = "Trap nights") +
  theme_minimal() +
  annotation_north_arrow(height = unit(1, "cm"),
                         style = north_arrow_minimal(text_size = 8)) +
  annotation_scale(height = unit(0.1, "cm"),
                   location = "tr") +
  guides(colour = guide_coloursteps(show.limits = TRUE, ticks = TRUE))

fig_1b_updated <- trap_site_mapping %>%
  drop_na(trap_nights_cat) %>%
  ggplot() +
  geom_bar(aes(x = trap_nights_cat, fill = trap_nights_cat)) +
  scale_fill_viridis_d(direction = -1) +
  scale_x_discrete(labels = c("0-100", "101-300", "301-500", "501-1,000", "1,000-2,000", "2,001-5000", "5,001-50,320")) +
  theme_minimal() +
  labs(x = "Trap nights",
       y = "Sites (n)") +
  guides(fill = "none")


save_plot(plot_grid(plotlist = list(fig_1a_updated, fig_1b_updated),
                    ncol = 1, rel_heights = c(1, 0.2), labels = c("A", "B")),
          filename = here("figures", "Figure_1_updated.png"), dpi = 320, base_height = 9, base_width = 10)


# Figure 2 ----------------------------------------------------------------

tn_final_model <- read_rds(here("data_clean", "tn_final_model.rds"))

model_1 <- getViz(tn_final_model)

fig_2_updated <- plot(sm(model_1, 3), n = 150, too.far = 0.02) +
  l_fitRaster(pTrans = zto1(0.05, 2, 0.1)) +
  geom_sf(data = included_countries %>% filter(GID_0 != "CPV"), fill = NA, alpha = 1, lwd = 0.5, inherit.aes = FALSE) +
  scale_fill_viridis_c(option = "inferno", na.value = "#ffffff00", direction = -1) +
  theme_minimal() +
  labs(title = element_blank(),
       x = element_blank(),
       y = element_blank(),
       fill = "Relative \ntrapping effort \nbias") +
  annotation_north_arrow(height = unit(1, "cm"),
                         style = north_arrow_minimal(text_size = 8)) +
  annotation_scale(height = unit(0.1, "cm"),
                   location = "tr")

save_plot(plot = as.grob(fig_2_updated$ggObj),
          filename = here("figures", "Figure_2_updated.png"), dpi = 320, base_height = 10, base_width = 12)

tn_pop_habitat_model_sens <- read_rds(here("data_clean", "tn_pop_habitat_model_sens.rds"))

model_1_s <- getViz(tn_pop_habitat_model_sens)

supplementary_fig_2_updated <- plot(sm(model_1_s, 5), n = 150, too.far = 0.02) +
  l_fitRaster(pTrans = zto1(0.05, 2, 0.1)) +
  geom_sf(data = included_countries %>% filter(GID_0 != "CPV"), fill = NA, alpha = 0.4, lwd = 0.1, inherit.aes = FALSE) +
  scale_fill_viridis_c(option = "inferno", na.value = "#ffffff00", direction = -1) +
  theme_minimal() +
  labs(title = element_blank(),
       x = element_blank(),
       y = element_blank(),
       fill = "Relative \ntrapping effort \nbias") +
  annotation_north_arrow(height = unit(1, "cm"),
                         style = north_arrow_minimal(text_size = 8)) +
  annotation_scale(height = unit(0.1, "cm"),
                   location = "tr")

save_plot(plot = as.grob(supplementary_fig_2_updated$ggObj),
          filename = here("figures", "Figure_3_updated_sensitivity.png"), dpi = 320, base_height = 10, base_width = 12)

# Figure 3 ----------------------------------------------------------------

plot_fig_3 <- function(species_name, iucn = rodent_iucn, gbif = rodent_gbif, rodents = rodent_spatial) {

  gbif <- gbif[[species_name]] %>%
    select(classification, geometry)

  iucn <- iucn %>%
    filter(classification == species_name) %>%
    st_intersection(contiguous_boundary) %>%
    st_union()

  rodents <- rodents %>%
    filter(classification == species_name) %>%
    select(classification, number, geometry) %>%
    mutate(pres_abs = case_when(number == 0 ~ factor("Non-detection", levels = c("Detection", "Non-detection")),
                                TRUE ~ factor("Detection", levels = c("Detection", "Non-detection")))) %>%
    st_intersection(contiguous_boundary) %>%
    distinct(classification, pres_abs, geometry)

  rodent_presence <- rodents %>%
    filter(pres_abs == "Detection")

  rodent_absence <- rodents %>%
    filter(pres_abs == "Non-detection") %>%
    filter(!geometry %in% rodent_presence$geometry)

  rodent_combined <- bind_rows(rodent_presence, rodent_absence)

  iucn_gbif_plot <- ggplot() +
    geom_sf(data = included_countries, fill = NA) +
    geom_sf(data = iucn, fill = "#C12D20", alpha = 0.1) +
    geom_sf(data = gbif, size = 1) +
    theme_minimal() +
    labs(title = element_blank(),
         x = element_blank(),
         y = element_blank()) +
    annotation_north_arrow(height = unit(1, "cm"),
                           style = north_arrow_minimal(text_size = 8)) +
    annotation_scale(height = unit(0.1, "cm"),
                     location = "tr")

  studies_plot <- ggplot() +
    geom_sf(data = included_countries, fill = NA) +
    geom_sf(data = iucn, fill = "#C12D20", alpha = 0.1) +
    geom_sf(data = rodent_combined, aes(colour = pres_abs), size = 1) +
    scale_colour_manual(values = c("#440154", "#ff8c00")) +
    theme_minimal() +
    labs(title = element_blank(),
         x = element_blank(),
         y = element_blank(),
         colour = "Detection/Non-detection") +
    annotation_north_arrow(height = unit(1, "cm"),
                           style = north_arrow_minimal(text_size = 8)) +
    annotation_scale(height = unit(0.1, "cm"),
                     location = "tr")

  combined_plot <- plot_grid(plotlist = list(iucn_gbif_plot, studies_plot + theme(legend.position = "none")), align = "hv", nrow = 1)

  legend <- get_legend(studies_plot +
                         guides(colour = guide_legend(nrow = 1)) +
                         theme(legend.position = "bottom"))

  return(list(combined_plot = combined_plot,
              legend = legend))
}

produced_plots <- list()
legend <- list()

for(i in 1:length(species_names)) {

  output <- plot_fig_3(species_names[[i]])

  produced_plots[[i]] <- output[["combined_plot"]]

  legend <- output[["legend"]]

}

names(produced_plots) <- species_names

test_plots_a <- plot_grid(produced_plots[[1]], produced_plots[[2]], produced_plots[[3]], produced_plots[[4]],
                        legend, ncol = 1, rel_heights = c(1, 1, 1, 1, 0.1), labels = str_to_sentence(species_names)[1:4])
test_plots_b <- plot_grid(produced_plots[[5]], produced_plots[[6]], produced_plots[[7]],
                          legend, ncol = 1, rel_heights = c(1, 1, 1, 0.1), labels = str_to_sentence(species_names)[5:7])

save_plot(plot = test_plots_a, filename = here("figures", "Figure_3_test_a.png"), base_height = 12, base_width = 8)
save_plot(plot = test_plots_b, filename = here("figures", "Figure_3_test_b.png"), base_height = 12, base_width = 8)


# Figure 4 ----------------------------------------------------------------

confirmed_pathogen_host <- read_rds(here("data_clean", "host_pathogen_positive_plot_data.rds"))
confirmed_pathogen_family_host <- read_rds(here("data_clean", "host_pathogen_family_positive_plot_data.rds"))
negative_pathogen_host <- read_rds(here("data_clean", "host_pathogen_negative_plot_data.rds"))

plot_4_df <- confirmed_pathogen_host %>%
  rowwise() %>%
  mutate(prop_acute = acute_infection/n_tested * 100,
         prop_prior = prior_infection/n_tested * 100) %>%
  select(classification, n_tested, prop_acute, prop_prior, pathogen_name, pathogen_family, source) %>%
  pivot_longer(cols = c("prop_acute", "prop_prior")) %>%
  rowwise() %>%
  mutate(species = paste0(str_to_upper(str_sub(unlist(str_split(classification, " "))[1], 1, 1)), ". ", unlist(str_split(classification, " "))[2]),
         pathogen_name = factor(pathogen_name, levels = c("lassa mammarenavirus", "usutu virus", "toxoplasma gondii", "coxiella burnetii",
                                                          "escherichia coli", "klebsiella pneumoniae")),
         pathogen_name = factor(str_wrap(str_to_sentence(pathogen_name), width = 10)),
         percent = paste(round(value, 1), "%"),
         name = case_when(name == "prop_acute" ~ "Acute infection",
                          name == "prop_prior" ~ "Serology"),
         source = factor(source, labels = c("CLOVER")))

plot_4 <- plot_4_df %>%
  ggplot() +
  geom_tile(aes(x = pathogen_name, y = species, fill = value, colour = source, width = 0.95, height = 0.95), lwd = 1) +
  geom_label(aes(x = pathogen_name, y = species, label = percent)) +
  facet_wrap(~ name) +
  scale_fill_viridis_c(option = "inferno", direction = -1) +
  scale_colour_manual(na.translate = FALSE, values = "black") +
  theme_minimal() +
  labs(fill = "Infection (%)",
       x = element_blank(),
       y = element_blank(),
       colour = element_blank()) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_text(size = 14))

save_plot(plot_4, filename = here("figures", "Figure_4_updated.png"), base_width = 12, base_height = 10)



# Supplementary figure 2 --------------------------------------------------

habitat_2005 <- rast(here("data_download", "habitat_2005", "habitat_2005.nc"))[[1]]
crop_habitat <- crop(habitat_2005, vect(contiguous_boundary))

sites_2 <- read_rds(here("data_clean", "traps_level_2_zoonoses.rds")) %>%
  vect()

all_regions <- lapply(1:nrow(sites_2), function(x) crop(crop_habitat, sites_2[x,]))
all_regions_hab <- lapply(1:length(all_regions), function(x) as.data.frame(freq(all_regions[[x]])) %>%
                            select(-layer))
names(all_regions_hab) <- sites_2$GID_2
habitats <- as.data.frame(data.table::rbindlist(all_regions_hab, idcol = TRUE)) %>%
  rename("GID_2" = ".id")

land_type_classification <- as.list(c("cropland",
                                      "cropland",
                                      "cropland",
                                      "cropland",
                                      "mosaic_cropland",
                                      "mosaic_cropland",
                                      "tree_cover",
                                      "tree_cover",
                                      "tree_cover",
                                      "tree_cover",
                                      "mosaic_vegetation",
                                      "mosaic_vegetation",
                                      "shrubland",
                                      "shrubland",
                                      "grassland",
                                      "sparse_vegetation",
                                      "sparse_vegetation",
                                      "sparse_vegetation",
                                      "flooded",
                                      "flooded",
                                      "flooded",
                                      "urban",
                                      "bare",
                                      "bare",
                                      "bare",
                                      "water"))
names(land_type_classification) <- as.list(c(10, 11, 12, 20, 30, 40, 50, 60, 61, 62, 100, 110, 120, 122, 130, 150, 152, 153, 160, 170, 180, 190, 200, 201, 202, 210))

wa_habitats <- habitats %>%
  mutate(habitat = recode(value, !!!land_type_classification)) %>%
  group_by(GID_2, habitat) %>%
  summarise(count = sum(count))

compare_wa_habitats <- wa_habitats %>%
  group_by(habitat) %>%
  filter(habitat != "water") %>%
  summarise(count = sum(count)) %>%
  mutate(data = "all",
         proportion = count/sum(count))

zoonotic_regions <- as.data.frame(sites_2) %>%
  filter(tn_density > 0) %>%
  distinct(GID_2)

compare_trap_habitat <- wa_habitats %>%
  filter(GID_2 %in% zoonotic_regions$GID_2) %>%
  group_by(habitat) %>%
  filter(habitat != "water") %>%
  summarise(count = sum(count)) %>%
  mutate(data = "Trapped regions",
         proportion = count/sum(count))

compare_trapping_habitats <- bind_rows(compare_wa_habitats, compare_trap_habitat) %>%
  select(data, habitat, count) %>%
  pivot_wider(names_from = habitat, values_from = count) %>%
  rowwise(data) %>%
  mutate(all = sum(c_across(where(is.numeric))))

prop_tests <- list()

for(i in 1:10) {

  prop_tests[[i]] <- prop.test(x = c(compare_trapping_habitats[[1,i+1]], compare_trapping_habitats[[2,i+1]]),
                               n = c(compare_trapping_habitats[[1,12]], compare_trapping_habitats[[2,12]]))
}

names(prop_tests) <- c(names(compare_trapping_habitats[2:11]))


plot_habitats <- bind_rows(compare_wa_habitats,
                           compare_trap_habitat) %>%
  arrange(-proportion) %>%
  mutate(habitat = fct_inorder(snakecase::to_sentence_case(habitat)),
         data = str_to_sentence(data)) %>%
  ggplot() +
  geom_col(aes(x = fct_rev(habitat), y = proportion, fill = data), position = position_dodge2()) +
  coord_flip() +
  labs(x = "Land cover classification",
       y = "Proportion of West African region",
       fill = "") +
  scale_fill_manual(values = c("#fde725", "#440154")) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_minimal() +
  theme(text = element_text(size = 16))

write_rds(plot_habitats, here("plots", "trap_habitats.rds"))
save_plot(here("figures", "Supplementary_figure_2.png"), plot_grid(plot_habitats), base_height = 8, base_width = 12)


# Supplementary figure 4 --------------------------------------------------


supp_4_df <- confirmed_pathogen_family_host %>%
  ungroup() %>%
  rowwise() %>%
  mutate(prop_acute = acute_infection/n_tested * 100,
         prop_prior = prior_infection/n_tested * 100) %>%
  select(classification, n_tested, prop_acute, prop_prior, pathogen_family, source) %>%
  pivot_longer(cols = c("prop_acute", "prop_prior")) %>%
  rowwise() %>%
  mutate(species = paste0(str_to_upper(str_sub(unlist(str_split(classification, " "))[1], 1, 1)), ". ", unlist(str_split(classification, " "))[2]),
         pathogen_family = str_to_sentence(pathogen_family),
         percent = paste(round(value, 1), "%"),
         name = case_when(name == "prop_acute" ~ "Acute infection",
                          name == "prop_prior" ~ "Serology"),
         source = factor(source, labels = c("CLOVER")))

plot_4_supp <- supp_4_df %>%
  ggplot() +
  geom_tile(aes(x = pathogen_family, y = species, fill = value, colour = source, width = 0.95, height = 0.95), lwd = 1) +
  facet_wrap(~ name) +
  scale_fill_viridis_c(option = "inferno", direction = -1) +
  scale_colour_manual(na.translate = FALSE, values = "black") +
  theme_minimal() +
  labs(fill = "Infection (%)",
       x = element_blank(),
       y = element_blank(),
       colour = element_blank()) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_text(size = 14)) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

save_plot(plot_3_supp, filename = here("figures", "Supplementary_figure_4_updated.png"), base_width = 12, base_height = 10)

