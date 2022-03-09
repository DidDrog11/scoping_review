
# Load data ---------------------------------------------------------------

rodent_spatial <- read_rds(here("data_clean", "rodent_spatial.rds")) %>%
  dplyr::select(-trap_nights)

pathogen <- read_rds(here("data_clean", "long_pathogen.rds")) %>%
  select(record_id, classification, assay, number, pathogen_tested, geometry) %>%
  mutate(assay = case_when(str_detect(assay, "tested") ~ "n_tested",
                           str_detect(assay, "pcr") ~ "n_positive_pcr",
                           str_detect(assay, "ab_ag") ~ "n_positive_ab_ag",
                           str_detect(assay, "histo") ~ "n_positive_histo",
                           str_detect(assay, "culture") ~ "n_positive_culture")) %>%
  group_by(pathogen_tested) %>%
  group_split()

all_pathogens <- lapply(pathogen, function(x) { x %>%
  pivot_wider(names_from = assay, values_from = number) })

names(all_pathogens) <- lapply(pathogen, function(x) {

  pathogen_names <- x %>%
    pull(pathogen_tested) %>%
    unique(.) }) %>%
  unlist()

all_pathogens[["lassa_mammarenavirus"]] <- all_pathogens[["lassa_mammarenavirus"]] %>%
  unnest_wider(n_tested, names_sep = "_") %>%
  unnest_wider(n_positive_pcr, names_sep = "_") %>%
  unnest_wider(n_positive_ab_ag, names_sep = "_") %>%
  unnest_wider(n_positive_culture, names_sep = "_") %>%
  rowwise(record_id) %>%
  mutate(n_tested = max(n_tested_1, n_tested_2, na.rm = TRUE),
         n_positive_ab_ag = case_when(!is.na(n_positive_ab_ag_1) & !is.na(n_positive_ab_ag_2) ~ max(n_positive_ab_ag_1, n_positive_ab_ag_2, na.rm = TRUE),
                                      TRUE ~ as.numeric(NA))) %>%
  rename("n_positive_pcr" = n_positive_pcr_1,
         "n_positive_culture" = n_positive_culture_1) %>%
  select(record_id, classification, pathogen_tested, geometry, n_tested, n_positive_pcr, n_positive_ab_ag, n_positive_culture)

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

clover$pathogen_taxa_species <- bind_rows(read_csv(here("data_download", "host-pathogen", "clover_1.0", "CLOVER_1.0_Bacteria_AssociationsFlatFile.csv")),
                                  read_csv(here("data_download", "host-pathogen", "clover_1.0", "CLOVER_1.0_HelminthProtozoaFungi_AssociationsFlatFile.csv")),
                                  read_csv(here("data_download", "host-pathogen", "clover_1.0", "CLOVER_1.0_Viruses_AssociationsFlatFile.csv"))) %>%
  select(any_of(starts_with("Pathogen"))) %>%
  rename(type = PathogenType, pathogen_name = Pathogen, pathogen_genus = PathogenGenus, pathogen_family = PathogenFamily,
         pathogen_order = PathogenOrder, pathogen_class = PathogenClass, pathogen_taxID = PathogenTaxID, pathogen_NCBI = PathogenNCBIResolved) %>%
  select(-PathogenOriginal) %>%
  distinct()

clover$pathogen_taxa_family <- bind_rows(read_csv(here("data_download", "host-pathogen", "clover_1.0", "CLOVER_1.0_Bacteria_AssociationsFlatFile.csv")),
                                          read_csv(here("data_download", "host-pathogen", "clover_1.0", "CLOVER_1.0_HelminthProtozoaFungi_AssociationsFlatFile.csv")),
                                          read_csv(here("data_download", "host-pathogen", "clover_1.0", "CLOVER_1.0_Viruses_AssociationsFlatFile.csv"))) %>%
  select(any_of(starts_with("Pathogen"))) %>%
  distinct() %>%
  select(type = PathogenType, pathogen_family = PathogenFamily,
         pathogen_order = PathogenOrder, pathogen_class = PathogenClass) %>%
  distinct()

clover$bacteria <- read_csv(here("data_download", "host-pathogen", "clover_1.0", "CLOVER_1.0_Bacteria_AssociationsFlatFile.csv")) %>%
  filter(Host %in% unique(rodent_spatial$classification))
clover$other <- read_csv(here("data_download", "host-pathogen", "clover_1.0", "CLOVER_1.0_HelminthProtozoaFungi_AssociationsFlatFile.csv")) %>%
  filter(Host %in% unique(rodent_spatial$classification))
clover$virus <- read_csv(here("data_download", "host-pathogen", "clover_1.0", "CLOVER_1.0_Viruses_AssociationsFlatFile.csv")) %>%
  filter(Host %in% unique(rodent_spatial$classification))

clover$combined <- bind_rows(clover$bacteria, clover$virus, clover$other) %>%
  select("classification" = Host, "PathogenType" = PathogenType, "pathogen_name" = Pathogen, "pathogen_family" = PathogenFamily) %>%
  distinct() %>%
  mutate(source = "CLOVER") %>%
  arrange(classification, pathogen_name) %>%
  group_by(classification)

clover_by_host <- clover$combined %>%
  group_split()

names(clover_by_host) <- clover$combined %>%
  group_keys() %>%
  pull()

# Host-pathogen associations ----------------------------------------------

pathogen_species_taxa <- cleaned_pathogen %>%
  mutate(pathogen_name = str_to_lower(pathogen_name),
         pathogen_family = str_to_lower(pathogen_family)) %>%
  left_join(clover$pathogen_taxa_species, by = c("pathogen_name", "pathogen_family")) %>%
  drop_na()

pathogen_family_taxa <- cleaned_pathogen %>%
  mutate(pathogen_name = str_to_lower(pathogen_name),
         pathogen_family = str_to_lower(pathogen_family)) %>%
  filter(!pathogen_name %in% pathogen_species_taxa$pathogen_name) %>%
  left_join(clover$pathogen_taxa_family, by = "pathogen_family")

pathogen_host <- bind_rows(pathogen_species_taxa, pathogen_family_taxa)

confirmed_pathogen_host <- pathogen_host %>%
  filter(acute_infection > 0 | prior_infection > 0) %>%
  filter(!str_ends(classification, "sp.") & !str_ends(pathogen_name, "sp.")) %>%
  left_join(., clover$combined, by = c("classification", "pathogen_name", "pathogen_family"))

negative_pathogen_host <- pathogen_host %>%
  filter(acute_infection == 0 & prior_infection == 0) %>%
  filter(!str_ends(classification, "sp.") & !str_detect(classification, "/") & !str_ends(pathogen_name, "sp."))

negative_clover <- negative_pathogen_host %>%
  left_join(., clover$combined, by = c("classification", "pathogen_name"))

confirmed_pathogen_family <- pathogen_host %>%
  filter(acute_infection > 0 | prior_infection > 0) %>%
  filter(!str_ends(classification, "sp.") & !str_detect(classification, "/")) %>%
  left_join(., clover$combined %>% distinct(classification, pathogen_family, source),
            by = c("classification", "pathogen_family"))

confirmed_clover_family <- confirmed_pathogen_family %>%
  left_join(., clover$combined %>%
              distinct(classification, pathogen_family, source), by = c("classification", "pathogen_family"))

clover_not_in_trapping <- clover$combined %>%
  filter(classification %in% pathogen_host$classification) %>%
  anti_join(confirmed_pathogen_host, by = c("classification", "pathogen_name")) %>%
  anti_join(confirmed_pathogen_family, by = c("classification", "pathogen_family"))

write_rds(confirmed_pathogen_host, here("data_clean", "host_pathogen_positive_plot_data.rds"))
write_rds(negative_pathogen_host, here("data_clean", "host_pathogen_negative_plot_data.rds"))
write_rds(confirmed_pathogen_family, here("data_clean", "host_pathogen_family_positive_plot_data.rds"))
