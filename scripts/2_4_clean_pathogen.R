source(here::here("scripts", "libraries.R"))

genus_dictionary <- read_rds(here("data_clean", "genus_dictionary.rds"))
species_dictionary <- read_rds(here("data_clean", "species_dictionary.rds"))


# Pathogen ----------------------------------------------------------------

# Cleaning species names
pathogen_data <- read_rds(here("data_raw", "pathogen.rds")) %>%
  mutate(country = as_factor(country),
         record_id = 1:nrow(.),
         unique_id = as_factor(unique_id),
         genus = recode(genus, !!!genus_dictionary),
         classification = paste(genus, ifelse(species == "-", "sp.", species), sep = " "),
         classification = recode(classification, !!!species_dictionary)) %>%
  separate(col = classification, into = c("genus", "species"), sep = " ", remove = F) %>%
  mutate(species = ifelse(species %in% c("sp.", "sp.1", "sp.2"), "", species))

# Matching to GBIF
rodent_data <- read_rds(here("data_clean", "species_data.rds")) %>%
  distinct(classification, gbif_id)

pathogen_data <- pathogen_data %>%
  left_join(., rodent_data,
            by = "classification")

# Cleaning habitat
habitat_dictionary <- read_rds(here("data_clean", "habitat_dictionary.rds"))
habitat_split <- c("habitat_1", "habitat_2", "habitat_3", "habitat_4", "habitat_5", "habitat_6", "habitat_7")

pathogen_data <- pathogen_data %>%
  separate(habitat, into = all_of(habitat_split), sep = ", ") %>%
  mutate(across(.cols = all_of(habitat_split), ~recode(., !!!habitat_dictionary)))

# Cleaning pathogen
pathogen_tested <- c("path_1", "path_2", "path_3", "path_4", "path_5", "path_6")

source(here("scripts", "pathogen_dictionary.r"))
pathogen_name <- read_rds(here("data_clean", "pathogen_dictionary.rds"))

pathogen_data <- pathogen_data %>%
  mutate(across(
    all_of(pathogen_tested),
    ~recode(., !!!pathogen_name)
    ))

# Cleaning location data
pathogen_data %<>%
  separate(col = longitude_DMS_W, into = c("long_degrees", "long_minutes", "long_seconds"), "_", remove = F) %>%
  separate(col = latitude_DMS_N, into = c("lat_degrees", "lat_minutes", "lat_seconds"), "_") %>%
  mutate(across(all_of(c("long_degrees", "long_minutes", "long_seconds","lat_degrees", "lat_minutes", "lat_seconds")), as.double),
         long_hemi = ifelse(long_degrees<0, "E", #assign -ve numbers to E
                            ifelse(substring(longitude_DMS_W, 1, 1) == "-", "E", "W")), #as 0 cannot be -ve we can check the sign on the text entry
         lat_hemi = ifelse(lat_degrees<0, "S", "N"),
         gps_dms = ifelse(is.na(lat_degrees|long_degrees), F, T),
         long_dms = ifelse(gps_dms == T,
                           paste(long_hemi, long_degrees, " ",
                                 ifelse(is.na(long_minutes), 0, long_minutes), '.',
                                 ifelse(is.na(long_seconds), 0, long_seconds),
                                 sep = ""), NA),
         lat_dms = ifelse(gps_dms == T,
                          paste(lat_hemi, lat_degrees, " ",
                                ifelse(is.na(lat_minutes), 0, lat_minutes), '.',
                                ifelse(is.na(lat_seconds), 0, lat_seconds),
                                sep = ""), NA),
         long_dms = gsub("-", "", long_dms),
         lat_dms = gsub("-", "", lat_dms),
         iso3c = countrycode(as.character(country), "country.name", "iso3c")) %>%
  dplyr::select(-longitude_DMS_W)

dms_path <- pathogen_data %>%
  drop_na(long_dms, lat_dms)

dms_path <- dms_path %>%
  mutate(lon_dd = parzer::parse_lon(long_dms),
         lat_dd = parzer::parse_lat(lat_dms)) %>%
  st_as_sf(coords = c("lon_dd", "lat_dd"), crs = "+proj=longlat +datum=WGS84")

dd_path <- pathogen_data %>%
  drop_na(longitude_D_E,latitude_D_N) %>%
  mutate(lon_dd = longitude_D_E,
         lat_dd = latitude_D_N) %>%
  st_as_sf(coords = c("lon_dd", "lat_dd"), crs = "+proj=longlat +datum=WGS84")

utm_path <- pathogen_data %>%
  drop_na(UTM_coordinates) %>%
  separate(col = UTM_coordinates, into = c("zone", "easting", "northing"), "_")

utm_q_path <- utm_path %>%
  filter(zone == "28Q") %>%
  st_as_sf(coords = c("easting", "northing"), crs = "+proj=utm +zone=28Q +datum=WGS84") %>%
  st_transform(crs = "+proj=longlat +datum=WGS84")

utm_p_path <- utm_path %>%
  filter(zone == "28P") %>%
  st_as_sf(coords = c("easting", "northing"), crs = "+proj=utm +zone=28P +datum=WGS84") %>%
  st_transform(crs = "+proj=longlat +datum=WGS84")

pathogen_gps <- bind_rows(dms_path, dd_path) %>%
  bind_rows(utm_q_path, utm_p_path)

st_crs(pathogen_gps) = 4326

path_no_gps <- pathogen_data %>%
  filter(!record_id %in% pathogen_gps$record_id) %>%
  mutate(long_dms = as.character(long_dms),
         lat_dms = as.character(lat_dms))

path_all <- bind_rows(pathogen_gps, path_no_gps) %>%
  dplyr::select(unique_id, year_trapping, month, country, iso3c, region, town_village, all_of(habitat_split), classification, gbif_id,
                all_of(pathogen_tested), path_1_tested, path_2_tested, path_3_tested, path_4_tested, path_5_tested, path_6_tested,
                pcr_path_1_positive, pcr_path_2_positive, pcr_path_3_positive, pcr_path_4_positive, pcr_path_5_positive, pcr_path_6_positive,
                ab_ag_path_1_positive, ab_ag_path_2_positive, ab_ag_path_3_positive, ab_ag_path_4_positive, ab_ag_path_5_positive,
                culture_path_1_positive, culture_path_2_positive, culture_path_3_positive, histo_path_1_positive, histo_path_2_positive,
                histo_path_3_positive, histo_path_4_positive, histo_path_5_positive, histo_path_6_positive, record_id)

write_rds(path_all, here("data_clean", "pathogen.rds"))
