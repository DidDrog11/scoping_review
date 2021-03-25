source(here::here("scripts", "libraries.R"))

genus_synonym <- tibble(syn = c("myomys", "nannomys"), acc = c("praomys", "mus"))
species_synonym <- tibble(syn = c("mus domesticus", "aethomys hypoxanthus", "crocidura megalura", "dasymys bentleyae", "erinaceus albiventris", "gerbilliscus gambianus", "tatera guineae", "gerbilliscus guineae", "gerbilliscus kempi", "gerbillus campestris", "gerbillus gambianus", "grammomys rutilans", "graphiurus hueti", "graphiurus parvus", "hemiechinus aethiopicus", "lophuromys flavipunctatus", "massouteria mzabi", "mastomys hildebrandtii", "steatomys caurianus", "tatera gambiana", "tatera guinea", "tatera guineae", "tatera kempi", "tatera robusta", "taterillus gracillis", "thamnomys rutilans"),
                          acc = c("mus musculus", "oenomys hypoxanthus", "suncus megalura", "dasymys incomtus", "atelerix albiventris", "gerbilliscus gambiana", "gerbilliscus guinea", "gerbilliscus guinea", "gerbilliscus kempii", "dipodillus campestris", "gerbilliscus gambiana", "grammomys poensis", "graphiurus nagtglasii", "graphiurus kelleni", "paraechinus aethiopicus", "lophuromys flavopunctatus", "massoutiera mzabi", "mastomys natalensis", "steatomys caurinus", "gerbilliscus kempii", "gerbilliscus guineae", "gerbilliscus guineae", "gerbilliscus kempii", "gerbilliscus robusta", "taterillus gracilis", "grammomys poensis"))

# Pathogen ----------------------------------------------------------------

# Cleaning species names
pathogen_data <- read_rds(here("data_raw", "pathogen.rds")) %>%
  mutate(country = as_factor(country),
         record_id = 1:nrow(.),
         unique_id = as_factor(unique_id))
pathogen_data$genus <- plyr::mapvalues(pathogen_data$genus, from = genus_synonym$syn, to = genus_synonym$acc)
pathogen_data %<>%
  mutate(classification = paste(genus, ifelse(species == "-", "sp.", species), sep = " "))
pathogen_data$classification <- plyr::mapvalues(pathogen_data$classification, from = species_synonym$syn, to = species_synonym$acc)
pathogen_data %<>%
  separate(col = classification, into = c("genus", "species"), sep = " ", remove = F) %>%
  mutate(species = ifelse(species %in% c("sp.", "sp.1", "sp.2"), "", species))
# Matching to GBIF
rodent_data <- read_rds(here("data_clean", "species_data.rds")) %>%
  distinct(classification, gbif_id)
pathogen_data <- pathogen_data %>%
  left_join(., rodent_data,
            by = "classification")

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

path_all <- bind_rows(pathogen_gps, path_no_gps)

write_rds(path_all, here("data_clean", "pathogen.rds"))
