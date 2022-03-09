# Load data ---------------------------------------------------------------
sf_use_s2(use_s2 = FALSE)

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

pathogen_family <- c("Arenaviridae", "Borreliaceae", "Flaviviridae", "Hantaviridae",
                     "Phenuiviridae", "Phenuiviridae", "Trypanosomatidae", "Arenaviridae",
                     "Trypanosomatidae", "Anaplasmataceae", "Bartonellaceae", "Taeniidae",
                     "Leptospiraceae", "Plagiorchiidae", "Schistosomatidae", "Sarcocystidae",
                     "Trichuridae", "Flaviviridae", "Poxviridae", "Enterobacteriaceae",
                     "Enterobacteriaceae", "Mycobacteriaceae", "Coxiellaceae", "Ehrlichiaceae",
                     "Arenaviridae", "Mycoplasmataceae", "Rickettsiaceae", "Rickettsiaceae",
                     "Babesiidae", "Eimeriidae", "Plasmodiidae", "Strongyloididae")
names(pathogen_family) <- unique(host_pathogen$pathogen_tested)

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

level_2_all <- bind_rows(level_2, non_trapped)

included_countries <- level_0 %>%
  filter(GID_0 %in% continental_countries)

contiguous_boundary <- included_countries %>%
  filter(!GID_0 == "CPV") %>%
  st_union()

rodent_iucn <- st_read(here("data_download", "iucn_data", "data_0.shp")) %>%
  bind_rows(st_read(here("data_download", "iucn_data", "data_1.shp"))) %>%
  bind_rows(st_read(here("data_download", "iucn_data", "data_2.shp"))) %>%
  mutate(classification = str_to_lower(BINOMIAL)) %>%
  select(classification, geometry) %>%
  group_by(classification) %>%
  summarise(geometry = st_union(geometry))

# Analysis ----------------------------------------------------------------


pathogen_space <- bind_rows(all_pathogens) %>%
  filter(n_tested != 0)  %>%
  mutate(pathogen_family = recode(pathogen_tested, !!!pathogen_family))

top_4_pathogens <- pathogen_space %>%
  st_as_sf() %>%
  mutate(x = st_coordinates(geometry)[,1],
         y = st_coordinates(geometry)[,2]) %>%
  drop_na(x, y) %>%
  tibble() %>%
  distinct(pathogen_tested, x, y) %>%
  group_by(pathogen_tested) %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  head(4) %>%
  pull(pathogen_tested)

pathogen_list <- pathogen_space %>%
  filter(pathogen_tested %in% top_4_pathogens) %>%
  drop_na(geometry) %>%
  st_as_sf() %>%
  group_by(pathogen_tested) %>%
  group_split()

names(pathogen_list) <- pathogen_space %>%
  filter(pathogen_tested %in% top_4_pathogens) %>%
  group_by(pathogen_tested) %>%
  group_keys() %>%
  pull()

borrelia <- pathogen_list[[1]] %>%
  tibble() %>%
  filter(!str_detect(classification, "sp.|/")) %>%
  group_by(classification) %>%
  summarise(n_tested = sum(n_tested),
            n_positive_infected = sum(n_positive_pcr, n_positive_histo, n_positive_culture, na.rm = TRUE),
            n_positive_serology = sum(n_positive_ab_ag, na.rm = TRUE),
            n_positive_combined = sum(n_positive_infected, n_positive_serology),
            prop_positive = n_positive_combined/n_tested)

lassa <- pathogen_list[[2]] %>%
  tibble() %>%
  filter(!str_detect(classification, "sp.|/")) %>%
  group_by(classification) %>%
  summarise(n_tested = sum(n_tested),
            n_positive_infected = sum(n_positive_pcr, n_positive_histo, n_positive_culture, na.rm = TRUE),
            n_positive_serology = sum(n_positive_ab_ag, na.rm = TRUE),
            n_positive_combined = sum(n_positive_infected, n_positive_serology),
            prop_positive = n_positive_combined/n_tested)

leptospirosis <- pathogen_list[[3]] %>%
  tibble() %>%
  filter(!str_detect(classification, "sp.|/")) %>%
  group_by(classification) %>%
  summarise(n_tested = sum(n_tested),
            n_positive_infected = sum(n_positive_pcr, n_positive_histo, n_positive_culture, na.rm = TRUE),
            n_positive_serology = sum(n_positive_ab_ag, na.rm = TRUE),
            n_positive_combined = sum(n_positive_infected, n_positive_serology),
            prop_positive = n_positive_combined/n_tested)

toxoplasmosa <- pathogen_list[[4]] %>%
  tibble() %>%
  filter(!str_detect(classification, "sp.|/")) %>%
  group_by(classification) %>%
  summarise(n_tested = sum(n_tested),
            n_positive_infected = sum(n_positive_pcr, n_positive_histo, n_positive_culture, na.rm = TRUE),
            n_positive_serology = sum(n_positive_ab_ag, na.rm = TRUE),
            n_positive_combined = sum(n_positive_infected, n_positive_serology),
            prop_positive = n_positive_combined/n_tested)

pathogen_area_testing <- list()

for(i in 1:length(pathogen_list)) {

  pathogen_testing <- pathogen_list[[i]] %>%
    tibble() %>%
    group_by(classification) %>%
    summarise(n_tested = sum(n_tested),
              n_positive_infected = sum(n_positive_pcr, n_positive_histo, n_positive_culture, na.rm = TRUE),
              n_positive_serology = sum(n_positive_ab_ag, na.rm = TRUE),
              n_positive_combined = sum(n_positive_infected, n_positive_serology))

  top_5_species <- pathogen_testing %>%
    arrange(-n_positive_combined) %>%
    filter(!str_detect(classification, "sp.|/")) %>%
    head(5) %>%
    pull(classification)

  number_sites <- pathogen_list[[i]] %>%
    filter(classification %in% top_5_species) %>%
    mutate(x = st_coordinates(geometry)[,1],
           y = st_coordinates(geometry)[,2]) %>%
    distinct(classification, x, y) %>%
    group_by(classification) %>%
    summarise(n_sites = n())

  level_2_pathogen <- pathogen_list[[i]] %>%
    filter(classification %in% top_5_species) %>%
    rowwise() %>%
    mutate(n_tested = sum(n_tested),
           n_positive_infected = sum(n_positive_pcr, n_positive_histo, n_positive_culture, na.rm = TRUE),
           n_positive_serology = sum(n_positive_ab_ag, na.rm = TRUE),
           n_positive_combined = sum(n_positive_infected, n_positive_serology)) %>%
    st_join(level_2_all, .,
            st_contains) %>%
    st_intersection(contiguous_boundary) %>%
    drop_na(classification) %>%
    distinct(GID_2, classification, pathogen_tested, n_tested, n_positive_combined, geometry) %>%
    group_by(classification) %>%
    summarise(geometry = st_union(geometry),
              n_tested = sum(n_tested),
              n_positive_combined = sum(n_positive_combined)) %>%
    mutate(area_tested = set_units(st_area(geometry), km^2))

  species_ranges <- rodent_iucn %>%
    filter(classification %in% top_5_species) %>%
    st_intersection(contiguous_boundary) %>%
    mutate(area_iucn = set_units(st_area(geometry), km^2)) %>%
    tibble() %>%
    select(classification, area_iucn)

  rodent_distribution <- rodent_spatial %>%
    filter(classification %in% top_5_species) %>%
    select(classification, number, geometry) %>%
    mutate(pres_abs = case_when(number == 0 ~ factor("Non-detection", levels = c("Detection", "Non-detection")),
                                TRUE ~ factor("Detection", levels = c("Detection", "Non-detection")))) %>%
    filter(pres_abs == "Detection") %>%
    st_intersection(contiguous_boundary) %>%
    distinct(classification, geometry) %>%
    st_join(level_2_all, ., st_contains) %>%
    drop_na(classification) %>%
    group_by(classification) %>%
    summarise(geometry = st_union(geometry)) %>%
    mutate(area_trapping = set_units(st_area(geometry), km^2))

  pathogen_area_testing[[i]] <- level_2_pathogen %>%
    tibble() %>%
    select(classification, n_tested, n_positive_combined, area_tested) %>%
    left_join(number_sites) %>%
    left_join(tibble(species_ranges) %>%
                select(classification, area_iucn)) %>%
    mutate(perc_iucn = area_tested/area_iucn * 100) %>%
    left_join(tibble(rodent_distribution) %>%
                select(classification, area_trapping)) %>%
    mutate(perc_trapped = area_tested/area_trapping * 100,
           pathogen = rep(names(pathogen_list[i]), 5)) %>%
    select(pathogen, classification, n_sites, n_tested, n_positive_combined, area_tested, perc_iucn, perc_trapped)

}

table_2 <- pathogen_area_testing %>%
  bind_rows() %>%
  mutate(pathogen = case_when(pathogen == "borrelia_species" ~ "Borrelia sp.",
                              pathogen == "leptospirosis_species" ~ "Leptospira sp.",
                              TRUE ~ str_to_sentence(str_replace_all(pathogen, "_", " "))),
         area_tested = round(as.numeric(area_tested)/1000, 1),
         perc_iucn = as.numeric(round(perc_iucn, 1)),
         perc_iucn = case_when(is.na(perc_iucn) ~ "*",
                               perc_iucn < 1 ~ "< 1",
                               TRUE ~ as.character(perc_iucn)),
         perc_trapped = as.numeric(round(perc_trapped, 1)),
         perc_trapped = case_when(is.na(perc_trapped) ~ "*",
                                  perc_trapped < 1 ~ "< 1",
                                  TRUE ~ as.character(perc_trapped))) %>%
  rowwise() %>%
  mutate(species = paste0(str_to_upper(str_sub(unlist(str_split(classification, " "))[1], 1, 1)), ". ", unlist(str_split(classification, " "))[2])) %>%
  arrange(pathogen, -n_tested, -n_positive_combined) %>%
  select(Pathogen = pathogen, Species = species, `Administrative regions (n)` = n_sites, `Tested (n)` = n_tested, `Positive (n)` = n_positive_combined,
         `Area sampled (1,000 km2)` = area_tested, `IUCN area (%)` = perc_iucn, `Trapped area (%)` = perc_trapped) %>%
  as_grouped_data(groups = "Pathogen")

Table_2 <- flextable(table_2) %>%
  italic(j = "Species", italic = TRUE, part = "body") %>%
  bg(i = 1:6, bg = "grey", part = "body") %>%
  bg(i = 13:18, bg = "grey", part = "body") %>%
  compose(part = "header", j = 6, value = as_paragraph("Area sampled\n (1,000 km", as_sup("2"), ")")) %>%
  align(part = "header", align = "center")

write_rds(Table_2, here("tables", "Table_2_updated.rds"))
