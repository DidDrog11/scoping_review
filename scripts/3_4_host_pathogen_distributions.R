# Load data ---------------------------------------------------------------

contiguous_boundary <- read_rds(here("data_clean", "WA_continental_boundary.rds"))

contiguous_boundary_v <- vect(contiguous_boundary)

rodent_spatial <- read_rds(here("data_clean", "rodent_spatial.rds")) %>%
  dplyr::select(-trap_nights)

rodent_iucn <- read_rds(here("data_clean", "rodent_iucn.rds"))

tested_pcr <- tibble(read_rds(here("data_clean", "long_pathogen.rds"))) %>%
  select(-any_of(contains("habitat"))) %>%
  drop_na(geometry) %>%
  filter(!geometry %in% "c(NA, NA)") %>%
  pivot_wider(names_from = assay, values_from = number) %>%
  filter(!is.na(pcr_path_1_positive) | !is.na(pcr_path_2_positive) | !is.na(pcr_path_3_positive) |
           !is.na(pcr_path_4_positive) | !is.na(pcr_path_5_positive) | !is.na(pcr_path_6_positive)) %>%
  select(-any_of(c(contains("ab_ag"), contains("culture"), contains("histo")))) %>%
  mutate(record_id = row_number(),
         assay = "PCR") %>%
  group_by(record_id) %>% # use the following to coalesce row-wise
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
  select(-any_of(contains("path_"))) %>%
  filter(tested > 0) %>%
  st_as_sf(crs = crs(rodent_spatial))

tested_serology <- tibble(read_rds(here("data_clean", "long_pathogen.rds"))) %>%
  select(-any_of(contains("habitat"))) %>%
  drop_na(geometry) %>%
  filter(!geometry %in% "c(NA, NA)") %>%
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
  select(-any_of(contains("path_"))) %>%
  filter(tested > 0) %>%
  st_as_sf(crs = crs(rodent_spatial))

tested_culture <- tibble(read_rds(here("data_clean", "long_pathogen.rds"))) %>%
  select(-any_of(contains("habitat"))) %>%
  drop_na(geometry) %>%
  pivot_wider(names_from = assay, values_from = number) %>%
  filter(!is.na(culture_path_1_positive)) %>%
  select(-any_of(c(contains("pcr"), contains("ab_ag"), contains("histo")))) %>%
  mutate(record_id = row_number(),
         assay = "Culture") %>%
  group_by(record_id) %>%
  mutate(tested = case_when(!is.na(culture_path_1_positive) ~ path_1_tested),
         positive = case_when(!is.na(path_1_tested) ~ culture_path_1_positive)) %>%
  select(-any_of(contains("path_"))) %>%
  filter(tested > 0) %>%
  st_as_sf(crs = crs(rodent_spatial))

tested_histo <- tibble(read_rds(here("data_clean", "long_pathogen.rds"))) %>%
  select(-any_of(contains("habitat"))) %>%
  drop_na(geometry) %>%
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
  select(-any_of(contains("path_"))) %>%
  filter(tested > 0) %>%
  st_as_sf(crs = crs(rodent_spatial))

pathogen <- bind_rows(tested_pcr, tested_serology, tested_culture, tested_histo) %>%
  mutate(class = case_when(assay != "Serology" ~ "Acute infection",
                           TRUE ~ "Serology"),
         record_ID = row_number()) %>%
  distinct(unique_id, year_trapping, month, country, iso3c, region, town_village, classification, geometry, pathogen_tested, tested, positive, class) %>%
  vect(.) %>%
  terra::intersect(., contiguous_boundary_v) %>%
  st_as_sf()

if(!file.exists(here("data_clean", "hp_associations_dictionary.rds"))) {

  source(here("scripts", "3_3_host_pathogen_associations.R"))

} else {

  pathogen_family <- read_rds(here("data_clean", "hp_associations_dictionary.rds"))[[1]]
  pathogen_clean <- read_rds(here("data_clean", "hp_associations_dictionary.rds"))[[2]]

}

cleaned_pathogen <- pathogen %>%
  mutate(pathogen_family = recode_factor(pathogen_tested, !!!pathogen_family),
         pathogen_tested = recode_factor(pathogen_tested, !!!pathogen_clean)) %>%
  arrange(pathogen_family, pathogen_tested)

# Load in the raster and aggregate to ~20km cell sizes
ref_rast <- rast(here("data_download", "habitat_2005", "wa_hab_2005.tif")) %>%
  mask(., contiguous_boundary_v) %>%
  aggregate(fact = 15) %>%
  cellSize(unit = "km")

# Produce data for the extent of all hosts species trapped
rodent_trap <- rodent_spatial %>%
  filter(!str_detect(classification, "sp.")) %>%
  group_by(classification) %>%
  select(classification, number, geometry) %>%
  mutate(presence = case_when(number > 0 ~ 1,
                              TRUE ~ 0)) %>%
  filter(presence != 0) %>%
  select(classification, presence, geometry) %>%
  group_split()
names(rodent_trap)  <- lapply(rodent_trap, function(x) unique(x$classification))

rodent_trap_v <- lapply(rodent_trap, function(x) vect(x))

rodent_trap_r <- vector("list", length = length(rodent_trap_v))
names(rodent_trap_r) <- names(rodent_trap_v)
rodent_trap_r <- lapply(rodent_trap_v, function(x)  rasterize(x, ref_rast, fun = "max", field = "presence"))

rodent_iucn_sf <- rodent_iucn %>%
  filter(classification %in% unique(cleaned_pathogen$classification)) %>%
  group_by(classification) %>%
  mutate(presence = 1) %>%
  group_split()

rodent_iucn_v <- lapply(rodent_iucn_sf, function(x) vect(x))
names(rodent_iucn_v) <- lapply(rodent_iucn_v, function(x) unique(x$classification))

rodent_iucn_r <- vector("list", length = length(rodent_iucn_sf))
names(rodent_iucn_r) <- lapply(rodent_iucn_v, function(x) unique(x$classification))
rodent_iucn_r <- lapply(rodent_iucn_v, function(x) rasterize(x, ref_rast, fun = "max", field = "presence"))

# Function to analyse proportion of coveration for host-pathogen testing
hp_analysis_proportion_pixel <- function(n_pathogens, n_species, trap_data = rodent_spatial, pathogen_data = cleaned_pathogen,
                                         rodent_trap = rodent_trap_r, rodent_iucn = rodent_iucn_r) {

  # Produce a vector of the pathogen names in order of number species assayed
  pathogens <- tibble(cleaned_pathogen) %>%
    tabyl(pathogen_tested) %>%
    arrange(-n) %>%
    head(n = n_pathogens) %>%
    pull(pathogen_tested)

  # Produce a vector of detections and non-detections for each pathogen
  pathogen_list <- pathogen_data %>%
    filter(pathogen_tested %in% pathogens) %>%
    group_by(pathogen_tested) %>%
    group_split()

  # For each pathogen we want to identify the 5 most commonly tested rodent species
  pathogen_r_detection <- vector("list", n_pathogens)
  names(pathogen_r_detection) <- pathogens
  pathogen_r_detection <- lapply(pathogen_r_detection, function(x) {x <- vector("list", n_species)})

  area_tested <- tibble(pathogen_name = as.character(NULL),
                        species_name = as.character(NULL),
                        tested = as.numeric(NULL),
                        positive = as.numeric(NULL),
                        area_tested = as.numeric(NULL),
                        perc_trapped = as.numeric(NULL),
                        perc_iucn = as.numeric(NULL))

  for(i in 1:n_pathogens) {

    species <- tibble(pathogen_list[[i]]) %>%
      filter(!str_detect(classification, "sp.|crocidura")) %>%
      group_by(classification) %>%
      summarise(n = sum(tested)) %>%
      arrange(-n) %>%
      head(n = n_species) %>%
      pull(classification)

    pathogen_list[[i]] <- pathogen_list[[i]] %>%
      filter(classification %in% species) %>%
      group_by(classification) %>%
      group_split()

    # Produce a vector of tested locations for each species for each pathogen
    for(j in 1:n_species) {

      pathogen_v <- pathogen_list[[i]][[j]] %>%
        select(pathogen_tested, classification, tested, positive, geometry) %>%
        vect()

      # Convert this into a raster using the ref_rast with cell sizes of ~ 20km
      pathogen_r_detection[[i]][[j]] <- rasterize(pathogen_v, ref_rast, fun = "max", field = "tested")
      names(pathogen_r_detection[[i]][[j]]) <- unique(pathogen_v$classification)

      rodent_trap_raster <- rodent_trap_r[grep(names(pathogen_r_detection[[i]][[j]]), names(rodent_trap))]

      perc_trapped <- round(expanse(pathogen_r_detection[[i]][[j]], unit = "km")/
                              expanse(rodent_trap_raster[[1]], unit = "km"), 4) * 100

      if(names(pathogen_r_detection[[i]][[j]]) %in% c("mus musculus", "crocidura olivieri", "rattus norvegicus")) { perc_iucn = NA } else
      {
        rodent_iucn_raster <- rodent_iucn_r[grep(names(pathogen_r_detection[[i]][[j]]), names(rodent_iucn))]

        perc_iucn <- round(expanse(pathogen_r_detection[[i]][[j]], unit = "km")/
                             expanse(rodent_iucn_raster[[1]], unit = "km"), 4) * 100 }

      # Calculate the area for the pathogen tested within this species
      area_tested <-  bind_rows(area_tested,
                                tibble(pathogen_name = unique(pathogen_v$pathogen_tested),
                                       species_name = unique(pathogen_v$classification),
                                       tested = sum(pathogen_v$tested),
                                       positive = sum(pathogen_v$positive),
                                       area_tested = round(pathogen_r_detection[[i]][[j]] %>%
                                                             expanse(unit = "km"), 2)/1000,
                                       perc_trapped = perc_trapped,
                                       perc_iucn = perc_iucn))
    }

    area_tested <- bind_rows(area_tested)
  }

  return(area_tested)
}

table_2_data <- hp_analysis_proportion_pixel(n_pathogens = 5, n_species = 5)
table_2_sorted <- table_2_data %>%
  group_by(pathogen_name) %>%
  arrange(-tested, -positive, .by_group = TRUE)

table_2_formated <- table_2_sorted %>%
  mutate(species_name = str_to_sentence(species_name),
         positive = paste0(positive, " (", round(positive/tested, 2) * 100, "%)"),
         area_tested = round(area_tested, 2),
         perc_trapped = paste0(perc_trapped, "%"),
         perc_iucn = case_when(perc_iucn < 0.01 ~ paste0("<0.01%"),
                               is.na(perc_iucn) ~ "*",
                               TRUE ~ paste0(perc_iucn, "%"))) %>%
  as_grouped_data(groups = "pathogen_name")

write_rds(table_2_sorted, here("data_clean", "table_2_pixel.rds"))

Table_2_pixel <- flextable(table_2_formated) %>%
  bg(j = 2, bg = "grey", part = "all") %>%
  bg(j = 4, bg = "grey", part = "all") %>%
  bg(j = 6, bg = "grey", part = "all") %>%
  set_header_labels(values = list(pathogen_name = "Pathogen",
                                  species_name = "Species",
                                  tested = "Tested",
                                  positive = "Positive",
                                  area_tested = "Pathogen testing area \n(1,000 km_2_)",
                                  perc_trapped = "Pathogen testing area \nwithin trapped area (%)",
                                  perc_iucn = "Pathogen testing area \nwithin IUCN range (%)")) %>%
  italic(j = "species_name", italic = TRUE, part = "body") %>%
  compose(part = "header", j = 5, value = as_paragraph("Pathogen testing area \n(1,000 km", as_sup("2"), ")")) %>%
  align(part = "all", align = "center")

write_rds(Table_2_pixel, here("tables", "Table_2_updated_pixel.rds"))

n_species_tested <- tibble(cleaned_pathogen) %>%
  select(-geometry) %>%
  group_by(pathogen_tested, classification) %>%
  summarise(n_tested = sum(tested),
            n_positive = sum(positive))

# Number of species tested
tabyl(n_species_tested$pathogen_tested)

# Number of positive species
n_species_tested %>%
  filter(n_positive > 0) %>%
  tabyl(pathogen_tested)

# Number of individuals positive for arenaviruses
n_species_tested %>%
  filter(str_detect(pathogen_tested, "renav")) %>%
  filter(n_positive > 0) %>%
  arrange(-n_positive)
