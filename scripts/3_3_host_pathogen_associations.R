# Load data ---------------------------------------------------------------

rodent_spatial <- read_rds(here("data_clean", "rodent_spatial.rds")) %>%
  dplyr::select(-trap_nights)

species_data <- read_rds(here("data_clean", "species_data.rds")) %>%
  filter(!is.na(species_gbif)) %>%
  distinct(classification, species_gbif)

tested_pcr <- tibble(read_rds(here("data_clean", "long_pathogen.rds"))) %>%
  select(-any_of(contains("habitat"))) %>%
  select(-geometry) %>%
  pivot_wider(names_from = assay, values_from = number) %>%
  filter(!is.na(pcr_path_1_positive) | !is.na(pcr_path_2_positive) | !is.na(pcr_path_3_positive) |
           !is.na(pcr_path_4_positive) | !is.na(pcr_path_5_positive) | !is.na(pcr_path_6_positive)) %>%
  select(-any_of(c(contains("ab_ag"), contains("culture"), contains("histo")))) %>%
  mutate(record_id = row_number(),
         assay = "PCR") %>%
  group_by(record_id) %>%
  mutate(tested = case_when(!is.na(pcr_path_1_positive) ~ path_1_tested,
                                !is.na(pcr_path_2_positive) ~ path_2_tested,
                                !is.na(pcr_path_3_positive) ~ path_3_tested,
                                !is.na(pcr_path_4_positive) ~ path_4_tested,
                                !is.na(pcr_path_5_positive) ~ path_5_tested,
                                !is.na(pcr_path_6_positive) ~ path_6_tested),
         positive = case_when(!is.na(path_1_tested) ~ pcr_path_1_positive,
                                  !is.na(path_2_tested) ~ pcr_path_2_positive,
                                  !is.na(path_3_tested) ~ pcr_path_3_positive,
                                  !is.na(path_4_tested) ~ pcr_path_4_positive,
                                  !is.na(path_5_tested) ~ pcr_path_5_positive,
                                  !is.na(path_6_tested) ~ pcr_path_6_positive)) %>%
  select(-any_of(contains("path_")))

tested_serology <- tibble(read_rds(here("data_clean", "long_pathogen.rds"))) %>%
  select(-any_of(contains("habitat"))) %>%
  select(-geometry) %>%
  pivot_wider(names_from = assay, values_from = number) %>%
  filter(!is.na(ab_ag_path_1_positive) | !is.na(ab_ag_path_2_positive) | !is.na(ab_ag_path_3_positive) |
           !is.na(ab_ag_path_4_positive) | !is.na(ab_ag_path_5_positive)) %>%
  select(-any_of(c(contains("pcr"), contains("culture"), contains("histo")))) %>%
  mutate(record_id = row_number(),
         assay = "Serology") %>%
  group_by(record_id) %>%
  mutate(tested = case_when(!is.na(ab_ag_path_1_positive) ~ path_1_tested,
                                     !is.na(ab_ag_path_2_positive) ~ path_2_tested,
                                     !is.na(ab_ag_path_3_positive) ~ path_3_tested,
                                     !is.na(ab_ag_path_4_positive) ~ path_4_tested,
                                     !is.na(ab_ag_path_5_positive) ~ path_5_tested),
         positive = case_when(!is.na(path_1_tested) ~ ab_ag_path_1_positive,
                                       !is.na(path_2_tested) ~ ab_ag_path_2_positive,
                                       !is.na(path_3_tested) ~ ab_ag_path_3_positive,
                                       !is.na(path_4_tested) ~ ab_ag_path_4_positive,
                                       !is.na(path_5_tested) ~ ab_ag_path_5_positive)) %>%
  select(-any_of(contains("path_")))

tested_culture <- tibble(read_rds(here("data_clean", "long_pathogen.rds"))) %>%
  select(-any_of(contains("habitat"))) %>%
  select(-geometry) %>%
  pivot_wider(names_from = assay, values_from = number) %>%
  filter(!is.na(culture_path_1_positive)) %>%
  select(-any_of(c(contains("pcr"), contains("ab_ag"), contains("histo")))) %>%
  mutate(record_id = row_number(),
         assay = "Culture") %>%
  group_by(record_id) %>%
  mutate(tested = case_when(!is.na(culture_path_1_positive) ~ path_1_tested),
         positive = case_when(!is.na(path_1_tested) ~ culture_path_1_positive)) %>%
  select(-any_of(contains("path_")))

tested_histo <- tibble(read_rds(here("data_clean", "long_pathogen.rds"))) %>%
  select(-any_of(contains("habitat"))) %>%
  select(-geometry) %>%
  pivot_wider(names_from = assay, values_from = number) %>%
  filter(!is.na(histo_path_1_positive) | !is.na(histo_path_2_positive) | !is.na(histo_path_3_positive) |
           !is.na(histo_path_4_positive) | !is.na(histo_path_5_positive) | !is.na(histo_path_6_positive)) %>%
  select(-any_of(c(contains("pcr"), contains("culture"), contains("ab_ag")))) %>%
  mutate(record_id = row_number(),
         assay = "Histology") %>%
  group_by(record_id) %>%
  mutate(tested = case_when(!is.na(histo_path_1_positive) ~ path_1_tested,
                            !is.na(histo_path_2_positive) ~ path_2_tested,
                            !is.na(histo_path_3_positive) ~ path_3_tested,
                            !is.na(histo_path_4_positive) ~ path_4_tested,
                            !is.na(histo_path_5_positive) ~ path_5_tested,
                            !is.na(histo_path_6_positive) ~ path_6_tested),
         positive = case_when(!is.na(path_1_tested) ~ histo_path_1_positive,
                              !is.na(path_2_tested) ~ histo_path_2_positive,
                              !is.na(path_3_tested) ~ histo_path_3_positive,
                              !is.na(path_4_tested) ~ histo_path_4_positive,
                              !is.na(path_5_tested) ~ histo_path_5_positive,
                              !is.na(path_6_tested) ~ histo_path_6_positive)) %>%
  select(-any_of(contains("path_")))

pathogen <- bind_rows(tested_pcr, tested_serology, tested_culture, tested_histo) %>%
  mutate(class = case_when(assay != "Serology" ~ "Acute infection",
                           TRUE ~ "Serology"),
         record_ID = row_number())

# We can now bring this data together and reshape so we have a single row for each host-pathogen pair

host_pathogen <- pathogen %>%
  filter(tested != 0) %>%
  group_by(classification, pathogen_tested, class) %>%
  summarise(tested = sum(tested, na.rm = TRUE),
            positive = sum(positive, na.rm = TRUE)) %>%
  group_by(classification) %>%
  mutate(n_pathogens_tested = n())

if(!file.exists(here("data_clean", "hp_associations_dictionary.rds"))) {
# We harmonise pathogen family names and define harmonised names for pathogens tested
pathogen_family <- c("Arenaviridae", "Borreliaceae", "Flaviviridae", "Hantaviridae",
                     "Phenuiviridae", "Phenuiviridae", "Trypanosomatidae", "Arenaviridae",
                     "Trypanosomatidae", "Anaplasmataceae", "Bartonellaceae", "Taeniidae",
                     "Leptospiraceae", "Plagiorchiidae", "Schistosomatidae", "Sarcocystidae",
                     "Trichuridae", "Flaviviridae", "Poxviridae", "Enterobacteriaceae",
                     "Enterobacteriaceae", "Mycobacteriaceae", "Coxiellaceae", "Ehrlichiaceae",
                     "Arenaviridae", "Mycoplasmataceae", "Rickettsiaceae", "Rickettsiaceae",
                     "Babesiidae", "Eimeriidae", "Plasmodiidae", "Strongyloididae")
names(pathogen_family) <- unique(host_pathogen$pathogen_tested)
pathogen_clean <- c("Arenaviridae sp.", "Borrelia sp.", "Flavivirus sp.", "Hantavirus sp.",
                    "Phlebovirus sp.", "Rift valley fever phlebovirus", "Trypansoma sp.", "Lassa mammarenavirus",
                    "Leishmania sp.", "Anaplasma sp.", "Bartonella sp.", "Hydatigera sp.", "Leptospira sp.",
                    "Plagiorchis sp.", "Schistosoma sp.", "Toxoplasma gondii", "Trichuria sp.", "Usutu virus",
                    "Orthopoxvirus sp.", "Escherichia coli", "Klebsiella pneumoniae", "Mycobacteria sp.", "Coxiella burnetii",
                    "Erhlichia sp.", "Mammarenavirus sp.", "Mycoplasma sp.", "Orentia sp.", "Ricketsia sp.", "Babesia sp.",
                    "Eimeria sp.", "Plasmodium sp.", "Strongyloides sp.")
names(pathogen_clean) <- unique(host_pathogen$pathogen_tested)

list(pathogen_family, pathogen_clean) %>%
  write_rds(here("data_clean", "hp_associations_dictionary.rds"))

} else {

  pathogen_family <- read_rds(here("data_clean", "hp_associations_dictionary.rds"))[[1]]
  pathogen_clean <- read_rds(here("data_clean", "hp_associations_dictionary.rds"))[[2]]

}

cleaned_pathogen <- host_pathogen %>%
  mutate(pathogen_family = recode_factor(pathogen_tested, !!!pathogen_family),
         pathogen_tested = recode_factor(pathogen_tested, !!!pathogen_clean)) %>%
  arrange(pathogen_family, pathogen_tested)

# Host-pathogen comparators ------------------------------------------
clover <- list()

# We import the data from the CLOVER 1.0 dataset previously downloaded
# First at species level
clover$pathogen_taxa_species <- bind_rows(read_csv(here("data_download", "host-pathogen", "clover_1.0", "CLOVER_1.0_Bacteria_AssociationsFlatFile.csv")),
                                          read_csv(here("data_download", "host-pathogen", "clover_1.0", "CLOVER_1.0_HelminthProtozoaFungi_AssociationsFlatFile.csv")),
                                          read_csv(here("data_download", "host-pathogen", "clover_1.0", "CLOVER_1.0_Viruses_AssociationsFlatFile.csv"))) %>%
  select(any_of(starts_with("Pathogen"))) %>%
  rename(type = PathogenType, pathogen_name = Pathogen, pathogen_genus = PathogenGenus, pathogen_family = PathogenFamily,
         pathogen_order = PathogenOrder, pathogen_class = PathogenClass, pathogen_taxID = PathogenTaxID, pathogen_NCBI = PathogenNCBIResolved) %>%
  select(-PathogenOriginal) %>%
  distinct()
# Then at family level
clover$pathogen_taxa_family <- bind_rows(read_csv(here("data_download", "host-pathogen", "clover_1.0", "CLOVER_1.0_Bacteria_AssociationsFlatFile.csv")),
                                         read_csv(here("data_download", "host-pathogen", "clover_1.0", "CLOVER_1.0_HelminthProtozoaFungi_AssociationsFlatFile.csv")),
                                         read_csv(here("data_download", "host-pathogen", "clover_1.0", "CLOVER_1.0_Viruses_AssociationsFlatFile.csv"))) %>%
  select(any_of(starts_with("Pathogen"))) %>%
  distinct() %>%
  select(type = PathogenType, pathogen_family = PathogenFamily,
         pathogen_order = PathogenOrder, pathogen_class = PathogenClass) %>%
  distinct()

# We also produce pathogen type specific datatables where the host matches our observed hosts
clover$bacteria <- read_csv(here("data_download", "host-pathogen", "clover_1.0", "CLOVER_1.0_Bacteria_AssociationsFlatFile.csv")) %>%
  filter(Host %in% unique(species_data$classification))
clover$other <- read_csv(here("data_download", "host-pathogen", "clover_1.0", "CLOVER_1.0_HelminthProtozoaFungi_AssociationsFlatFile.csv")) %>%
  filter(Host %in% unique(species_data$classification))
clover$virus <- read_csv(here("data_download", "host-pathogen", "clover_1.0", "CLOVER_1.0_Viruses_AssociationsFlatFile.csv")) %>%
  filter(Host %in% unique(species_data$classification))

clover$combined <- bind_rows(clover$bacteria, clover$virus, clover$other) %>%
  select("classification" = Host, "PathogenType" = PathogenType, "pathogen_name" = Pathogen, "pathogen_family" = PathogenFamily) %>%
  distinct() %>%
  mutate(source = "CLOVER") %>%
  arrange(classification, pathogen_name) %>%
  group_by(classification)

write_rds(clover, here("data_clean", "clover_cleaned.rds"))

# We produce a list of pathogens associated with each species, where there is an element per species
clover_by_host <- clover$combined %>%
  group_split()

names(clover_by_host) <- clover$combined %>%
  group_keys() %>%
  pull()

# Host-pathogen associations ----------------------------------------------

# Use the CLOVER dataset to limit to pathogens identified to species level
pathogen_species_taxa <- cleaned_pathogen %>%
  mutate(pathogen_name = str_to_lower(pathogen_tested),
         pathogen_family = str_to_lower(pathogen_family)) %>%
  left_join(clover$pathogen_taxa_species, by = c("pathogen_name", "pathogen_family")) %>%
  drop_na()

# Use the CLOVER dataset to limit to pathogens identified to family level
pathogen_family_taxa <- cleaned_pathogen %>%
  mutate(pathogen_name = str_to_lower(pathogen_tested),
         pathogen_family = str_to_lower(pathogen_family)) %>%
  filter(!pathogen_name %in% pathogen_species_taxa$pathogen_name) %>%
  left_join(clover$pathogen_taxa_family, by = "pathogen_family")

pathogen_host <- bind_rows(pathogen_species_taxa, pathogen_family_taxa)

# Produce a table of confirmed host-pathogen pairs
confirmed_pathogen_host <- pathogen_host %>%
  filter(positive > 0) %>%
  filter(!str_ends(classification, "sp.") & !str_ends(pathogen_name, "sp.")) %>%
  left_join(., clover$combined, by = c("classification", "pathogen_name", "pathogen_family")) %>%
  distinct(classification, pathogen_tested, .keep_all = TRUE)

# Number of host-pathogen pairs
nrow(confirmed_pathogen_host)

# Number of species
length(unique(confirmed_pathogen_host$classification))

# Number of pathogens
length(unique(confirmed_pathogen_host$pathogen_name))

# Produce a table of negative host-pathogen pairs
negative_pathogen_host <- pathogen_host %>%
  group_by(classification, pathogen_name) %>%
  summarise(tested = sum(tested),
            positive = sum(positive)) %>%
  filter(positive == 0) %>%
  filter(!str_ends(classification, "sp.") & !str_detect(classification, "/") & !str_ends(pathogen_name, "sp.")) %>%
  distinct(classification, pathogen_name, .keep_all = TRUE)

# Number of negative host-pathogen pairs
nrow(negative_pathogen_host)

# Number of species
length(unique(negative_pathogen_host$classification))

# Number of pathogens
length(unique(negative_pathogen_host$pathogen_name))

# Join the negative dataset to clover to look for discrepancies
positive_clover_negative_trapping <- negative_pathogen_host %>%
  left_join(., clover$combined, by = c("classification", "pathogen_name")) %>%
  filter(source == "CLOVER")

# Positive host-pathogen associations in CLOVER
matched_hp_pairs <- left_join(confirmed_pathogen_host, clover$combined) %>%
  filter(source == "CLOVER")

nrow(matched_hp_pairs)/nrow(confirmed_pathogen_host)

non_matched_hp_pairs <- anti_join(confirmed_pathogen_host, clover$combined)

(nrow(confirmed_pathogen_host) - nrow(non_matched_hp_pairs))/nrow(confirmed_pathogen_host)


# Family based data
confirmed_pathogen_family <- pathogen_host %>%
  filter(positive > 0) %>%
  filter(!str_ends(classification, "sp.") & !str_detect(classification, "/")) %>%
  left_join(., clover$combined %>% distinct(classification, pathogen_family, source),
            by = c("classification", "pathogen_family"))

confirmed_clover_family <- confirmed_pathogen_family %>%
  filter(source == "CLOVER")

# Finally identify host-pathogen pairs we haven't observed
clover_not_in_trapping <- clover$combined %>%
  filter(classification %in% pathogen_host$classification) %>%
  anti_join(confirmed_pathogen_host, by = c("classification", "pathogen_name")) %>%
  anti_join(confirmed_pathogen_family, by = c("classification", "pathogen_family"))

write_rds(confirmed_pathogen_host, here("data_clean", "host_pathogen_positive_plot_data.rds"))
write_rds(negative_pathogen_host, here("data_clean", "host_pathogen_negative_plot_data.rds"))
write_rds(confirmed_pathogen_family, here("data_clean", "host_pathogen_family_positive_plot_data.rds"))
write_rds(pathogen_host, here("data_clean", "host_pathogen_family_all_plot_data.rds"))

# Figure 4 ----------------------------------------------------------------

plot_4_df <- pathogen_host %>%
  filter(!str_ends(classification, "sp.") & !str_ends(pathogen_name, "sp.")) %>%
  left_join(., clover$combined, by = c("classification", "pathogen_name", "pathogen_family")) %>%
  rowwise() %>%
  mutate(prop = positive/tested * 100) %>%
  select(classification, class, tested, positive, prop, pathogen_name, pathogen_family, source) %>%
  pivot_longer(cols = c("prop")) %>%
  rowwise() %>%
  mutate(species = paste0(str_to_upper(str_sub(unlist(str_split(classification, " "))[1], 1, 1)), ". ", unlist(str_split(classification, " "))[2]),
         pathogen_name = factor(pathogen_name, levels = c("lassa mammarenavirus", "usutu virus", "rift valley fever phlebovirus", "toxoplasma gondii", "coxiella burnetii",
                                                          "escherichia coli", "klebsiella pneumoniae")),
         pathogen_name = factor(str_wrap(str_to_sentence(pathogen_name), width = 10)),
         source = factor(source, labels = c("CLOVER")))

sum_species <- plot_4_df %>%
  group_by(species, class) %>%
  summarise(n_tested = sum(tested)) %>%
  arrange(n_tested) %>%
  ungroup() %>%
  mutate(species = fct_inorder(species)) %>%
  pull(species)

plot_4_df$species <- fct_relevel(plot_4_df$species, levels(sum_species))

plot_4_acute <- plot_4_df %>%
  filter(tested > 1) %>%
  filter(class == "Acute infection") %>%
  mutate(lab = paste0(round(value, 1), "%, N = ", tested)) %>%
  ggplot() +
  geom_tile(aes(x = pathogen_name, y = species, fill = value, colour = source, width = 0.9, height = 0.9), lwd = 1.2) +
  geom_label(aes(x = pathogen_name, y = species, label = lab), size = 3.7) +
  scale_fill_viridis_c(option = "viridis", direction = -1, begin = 0.3, end = 1, limits = c(0, 50)) +
  scale_colour_manual(na.translate = FALSE, values = "black") +
  labs(title = "Acute infection",
       fill = "Infection (%)",
       x = element_blank(),
       y = element_blank(),
       colour = element_blank()) +
  theme_bw()

plot_4_serology <- plot_4_df %>%
  filter(tested > 1) %>%
  filter(class == "Serology") %>%
  mutate(lab = paste0(round(value, 1), "%, N = ", tested)) %>%
  ggplot() +
  geom_tile(aes(x = pathogen_name, y = species, fill = value, colour = source, width = 0.9, height = 0.9), lwd = 1.2) +
  geom_label(aes(x = pathogen_name, y = species, label = lab)) +
  scale_fill_viridis_c(option = "viridis", direction = -1, begin = 0.3, end = 1, limits = c(0, 20)) +
  scale_colour_manual(na.translate = FALSE, values = "black") +
  labs(title = "Serology",
       fill = "Infection (%)",
       x = element_blank(),
       y = element_blank(),
       colour = element_blank()) +
  theme_bw()

save_plot(plot_grid(plot_4_acute), filename = here("figures", "Figure_4_updated.pdf"), base_width = 12, base_height = 10)
save_plot(plot_grid(plot_4_acute), filename = here("figures", "Figure_4_updated.png"), base_width = 12, base_height = 10)
save_plot(plot_grid(plot_4_serology), filename = here("figures", "Figure_5_updated.pdf"), base_width = 12, base_height = 10)
save_plot(plot_grid(plot_4_serology), filename = here("figures", "Figure_5_updated.png"), base_width = 12, base_height = 10)

# Further results ---------------------------------------------------------

# Positive CLOVER, not assessed in trapping
not_assessed_trapping <- clover$combined %>%
  anti_join(., pathogen_host, by = c("classification", "pathogen_name", "pathogen_family")) %>%
  distinct(classification, pathogen_name, .keep_all = TRUE)
