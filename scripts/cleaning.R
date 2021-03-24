source(here::here("scripts", "libraries.r"))
source(here("scripts", "update_data.r"))

# Studies -----------------------------------------------------------------

studies <- read_rds(here("data_raw", "studies.rds")) %>%
  mutate(unique_id = as_factor(unique_id))

write_rds(studies, here("data_clean", "studies.rds"))

# Rodent data -------------------------------------------------------------

rodent_data <- read_rds(here("data_raw", "rodent_data.rds")) %>%
  mutate(country = as_factor(country),
         record_id = 1:nrow(.))

retain_columns <- c("unique_id", "year_trapping", "month_trapping", "country", "iso3c", "region", "town_village", "habitat", "intensity_use",
                    "genus", "species", "classification", "genus_gbif", "species_gbif",  "number", "trap_nights", "trap_night_unit", "record_id")

# Cleaning coordinates
rodent_data %<>%
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

# Cleaning species names and matching to GBIF
rodent_data %<>%
  mutate(genus = ifelse(genus == "nannomys" & species != "-", "mus", genus), # in GBIF nonnomys are classified as mus
         classification = paste(genus, ifelse(species == "-", "sp.", species), sep = " "))

genus <- tibble(rodent_data %>%
                  distinct(genus)) %>%
  arrange(genus)

genus$gbif_id <- get_gbifid(snakecase::to_sentence_case(genus$genus), ask = T)

genera <- classification(snakecase::to_sentence_case(genus$genus), db = "gbif")
genera <- as_tibble(do.call(rbind,c(genera, make.row.names = T)), rownames = "genera") %>%
  mutate(genera = str_sub(genera, 1, -3)) %>%
  pivot_wider(id_cols = genera, names_from = rank, values_from = name) %>%
  dplyr::select(-genera) %>%
  write_rds(here("data_clean", "trapped_genera.rds"))

species <- tibble(rodent_data %>%
                  filter(species != "-") %>%
                  distinct(classification)) %>%
  arrange(classification)

species$gbif_id <- get_gbifid(species$classification, ask = T)

rodent_data %<>%
  full_join(., genus %>%
              rename("genus_gbif" = gbif_id), by = "genus") %>%
  full_join(., species %>%
              rename("species_gbif" = gbif_id), by = "classification")

rodent_data %>%
  dplyr::select(all_of(retain_columns)) %>%
  write_rds(here("data_clean", "species_data.rds"))

# Converting coordinate types into consistent decimal degrees
dms <- rodent_data %>%
  drop_na(long_dms, lat_dms)

dms <- dms %>%
  mutate(lon_dd = parzer::parse_lon(long_dms),
         lat_dd = parzer::parse_lat(lat_dms)) %>%
  st_as_sf(coords = c("lon_dd", "lat_dd"), crs = "+proj=longlat +datum=WGS84")

dd <- rodent_data %>%
  drop_na(longitude_D_E,latitude_D_N) %>%
  mutate(lon_dd = longitude_D_E,
         lat_dd = latitude_D_N) %>%
  st_as_sf(coords = c("lon_dd", "lat_dd"), crs = "+proj=longlat +datum=WGS84")

utm <- rodent_data %>%
  drop_na(UTM_coordinates) %>%
  separate(col = UTM_coordinates, into = c("zone", "easting", "northing"), "_")

utm_q <- utm %>%
  filter(zone == "28Q") %>%
  st_as_sf(coords = c("easting", "northing"), crs = "+proj=utm +zone=28Q +datum=WGS84") %>%
  st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  dplyr::select(all_of(retain_columns))

utm_p <- utm %>%
  filter(zone == "28P") %>%
  st_as_sf(coords = c("easting", "northing"), crs = "+proj=utm +zone=28P +datum=WGS84") %>%
  st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  dplyr::select(all_of(retain_columns))

rodent_gps <- bind_rows(dms, dd) %>%
  dplyr::select(all_of(retain_columns)) %>%
  bind_rows(utm_q, utm_p)

st_crs(rodent_gps) = 4326

no_gps <- rodent_data %>%
  filter(!record_id %in% rodent_gps$record_id) %>%
  mutate(long_dms = as.double(long_dms),
         lat_dms = as.double(lat_dms))

all_rodent <- bind_rows(rodent_gps, no_gps) %>%
  dplyr::select(all_of(c(retain_columns, "long_dms", "lat_dms"))) %>%
  rename("longitude" = long_dms,
         "latitude" = lat_dms)

write_rds(rodent_gps, here("data_clean", "rodent_spatial.rds"))
write_rds(no_gps, here("data_clean", "rodent_missing_spatial.rds"))
write_rds(all_rodent, here("data_clean", "rodent_df.rds"))

# Pathogen ----------------------------------------------------------------

pathogen_data <- read_rds(here("data_raw", "pathogen.rds")) %>%
  mutate(unique_id = as_factor(unique_id))

# can copy process for managing locations from rodent data

write_rds(pathogen_data, here("data_clean", "pathogen.rds"))
