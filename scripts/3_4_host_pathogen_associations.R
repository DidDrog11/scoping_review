# Load data ---------------------------------------------------------------

rodent_spatial <- read_rds(here("data_clean", "rodent_spatial.rds")) %>%
  dplyr::select(-trap_nights)

species_data <- read_rds(here("data_clean", "species_data.rds")) %>%
  filter(!is.na(species_gbif)) %>%
  distinct(classification, species_gbif)

test <- tibble(read_rds(here("data_clean", "long_pathogen.rds"))) %>%
  select(-any_of(contains("habitat"))) %>%
  select(-geometry) %>%
  pivot_wider(names_from = assay, values_from = number) %>%
  mutate(record_id = row_number()) %>%
  group_by(record_id) %>%
  mutate(tested_pcr = case_when(!is.na(pcr_path_1_positive) ~ path_1_tested,
                                !is.na(pcr_path_2_positive) ~ path_2_tested))
  mutate(tested_pcr = case_when(!is.na(pcr_path_1_positive) & is.na(ab_ag_path_1_positive)),
         tested_ab_ag)

pathogen <- read_rds(here("data_clean", "long_pathogen.rds")) %>%
  select(record_id, classification, assay, number, pathogen_tested, geometry) %>%
  mutate(assay = case_when(str_detect(assay, "tested") ~ "n_tested",
                           str_detect(assay, "pcr") ~ "n_positive_pcr",
                           str_detect(assay, "ab_ag") ~ "n_positive_ab_ag",
                           str_detect(assay, "histo") ~ "n_positive_histo",
                           str_detect(assay, "culture") ~ "n_positive_culture")) %>%
  group_by(pathogen_tested) %>%
  group_split()

# Split this dataframe into a list with one element for each pathogen

all_pathogens <- lapply(pathogen, function(x) { x %>%
    pivot_wider(names_from = assay, values_from = number) })

names(all_pathogens) <- lapply(pathogen, function(x) {

  pathogen_names <- x %>%
    pull(pathogen_tested) %>%
    unique(.) }) %>%
  unlist()

# The data for Lassa needs further cleaning as studies tested using both PCR and antibody

all_pathogens[["lassa_mammarenavirus"]] <- all_pathogens[["lassa_mammarenavirus"]] %>%
  unnest_wider(n_tested, names_sep = "_") %>%
  unnest_wider(n_positive_pcr, names_sep = "_") %>%
  unnest_wider(n_positive_ab_ag, names_sep = "_") %>%
  unnest_wider(n_positive_culture, names_sep = "_") %>%
  group_by(record_id) %>%
  mutate(n_tested = max(n_tested_1, n_tested_2, na.rm = TRUE),
         n_positive_ab_ag = case_when(!is.na(n_positive_ab_ag_1) & !is.na(n_positive_ab_ag_2) ~ max(n_positive_ab_ag_1, n_positive_ab_ag_2, na.rm = TRUE),
                                      is.na(n_positive_ab_ag_1) & !is.na(n_positive_ab_ag_2) ~ n_positive_ab_ag_2,
                                      !is.na(n_positive_ab_ag_1) & is.na(n_positive_ab_ag_2) ~ n_positive_ab_ag_1,
                                      TRUE ~ as.numeric(NA))) %>%
  rename("n_positive_pcr" = n_positive_pcr_1,
         "n_positive_culture" = n_positive_culture_1) %>%
  select(record_id, classification, pathogen_tested, geometry, n_tested, n_positive_pcr, n_positive_ab_ag, n_positive_culture)

# We can now bring this data together and reshape so we have a single row for each host-pathogen pair

host_pathogen <- bind_rows(all_pathogens) %>%
  filter(n_tested != 0) %>%
  group_by(classification, pathogen_tested) %>%
  summarise(n_tested = sum(n_tested, na.rm = TRUE),
            n_positive_pcr = sum(n_positive_pcr, na.rm = TRUE),
            n_positive_ab_ag = sum(n_positive_ab_ag, na.rm = TRUE),
            n_positive_histo = sum(n_positive_histo, na.rm = TRUE),
            n_positive_culture = sum(n_positive_culture, na.rm = TRUE)) %>%
  group_by(classification) %>%
  mutate(n_pathogens_tested = n())

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

cleaned_pathogen <- host_pathogen %>%
  mutate(pathogen_family = recode_factor(pathogen_tested, !!!pathogen_family),
         pathogen_tested = recode_factor(pathogen_tested, !!!pathogen_clean)) %>%
  arrange(pathogen_family, pathogen_tested) %>%
  mutate(across(where(is.double), ~ replace_na(.x, 0))) %>%
  group_by(classification, pathogen_tested) %>%
  mutate(acute_infection = max(n_positive_pcr, n_positive_histo, n_positive_culture, na.rm = TRUE),
         prior_infection = max(n_positive_ab_ag, na.rm = TRUE)) %>%
  select(classification, pathogen_family, pathogen_name = pathogen_tested, n_tested, acute_infection, prior_infection, n_pathogens_tested)

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

# We produce a list of pathogens associated with each species, where there is an element per species
clover_by_host <- clover$combined %>%
  group_split()

names(clover_by_host) <- clover$combined %>%
  group_keys() %>%
  pull()

# Host-pathogen associations ----------------------------------------------

# Use the CLOVER dataset to limit to pathogens identified to species level
pathogen_species_taxa <- cleaned_pathogen %>%
  mutate(pathogen_name = str_to_lower(pathogen_name),
         pathogen_family = str_to_lower(pathogen_family)) %>%
  left_join(clover$pathogen_taxa_species, by = c("pathogen_name", "pathogen_family")) %>%
  drop_na()

# Use the CLOVER dataset to limit to pathogens identified to family level
pathogen_family_taxa <- cleaned_pathogen %>%
  mutate(pathogen_name = str_to_lower(pathogen_name),
         pathogen_family = str_to_lower(pathogen_family)) %>%
  filter(!pathogen_name %in% pathogen_species_taxa$pathogen_name) %>%
  left_join(clover$pathogen_taxa_family, by = "pathogen_family")

pathogen_host <- bind_rows(pathogen_species_taxa, pathogen_family_taxa)

# Produce a table of confirmed host-pathogen pairs
confirmed_pathogen_host <- pathogen_host %>%
  filter(acute_infection > 0 | prior_infection > 0) %>%
  filter(!str_ends(classification, "sp.") & !str_ends(pathogen_name, "sp.")) %>%
  left_join(., clover$combined, by = c("classification", "pathogen_name", "pathogen_family"))

# Number of host-pathogen pairs
nrow(confirmed_pathogen_host)

# Number of species
length(unique(confirmed_pathogen_host$classification))

# Number of pathogens
length(unique(confirmed_pathogen_host$pathogen_name))

# Produce a table of negative host-pathogen pairs
negative_pathogen_host <- pathogen_host %>%
  filter(acute_infection == 0 & prior_infection == 0) %>%
  filter(!str_ends(classification, "sp.") & !str_detect(classification, "/") & !str_ends(pathogen_name, "sp."))

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
  filter(acute_infection > 0 | prior_infection > 0) %>%
  filter(!str_ends(classification, "sp.") & !str_detect(classification, "/")) %>%
  left_join(., clover$combined %>% distinct(classification, pathogen_family, source),
            by = c("classification", "pathogen_family"))

confirmed_clover_family <- confirmed_pathogen_family %>%
  left_join(., clover$combined %>%
              distinct(classification, pathogen_family, source), by = c("classification", "pathogen_family"))

# Finally identify host-pathogen pairs we haven't observed
clover_not_in_trapping <- clover$combined %>%
  filter(classification %in% pathogen_host$classification) %>%
  anti_join(confirmed_pathogen_host, by = c("classification", "pathogen_name")) %>%
  anti_join(confirmed_pathogen_family, by = c("classification", "pathogen_family"))

write_rds(confirmed_pathogen_host, here("data_clean", "host_pathogen_positive_plot_data.rds"))
write_rds(negative_pathogen_host, here("data_clean", "host_pathogen_negative_plot_data.rds"))
write_rds(confirmed_pathogen_family, here("data_clean", "host_pathogen_family_positive_plot_data.rds"))

# Figure 4 ----------------------------------------------------------------

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
         lab = paste0(round(value, 1), "%"),
         name = case_when(name == "prop_acute" ~ "Acute infection",
                          name == "prop_prior" ~ "Serology"),
         source = factor(source, labels = c("CLOVER")))

plot_4 <- plot_4_df %>%
  ggplot() +
  geom_tile(aes(x = pathogen_name, y = species, fill = value, colour = source, width = 0.95, height = 0.95), lwd = 1) +
  geom_label(aes(x = pathogen_name, y = species, label = lab)) +
  facet_wrap(~ name) +
  scale_fill_viridis_c(option = "mako", direction = -1) +
  scale_colour_manual(na.translate = FALSE, values = "black") +
  theme_minimal() +
  labs(fill = "Infection (%)",
       x = element_blank(),
       y = element_blank(),
       colour = element_blank()) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        strip.text.x = element_text(size = 14))

save_plot(plot_4, filename = here("figures", "Figure_4_updated.png"), base_width = 12, base_height = 10)
