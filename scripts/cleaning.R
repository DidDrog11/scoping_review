source(here::here("scripts", "libraries.r"))
source(here("scripts", "update_data.r"))

studies <- read_rds(here("data_raw", "studies.rds")) %>%
  mutate(unique_id = as_factor(unique_id))

rodent_data <- read_rds(here("data_raw", "rodent_data.rds")) %>%
  mutate(country = as_factor(country),
         record_id = 1:nrow(rodent_data))

ggplot(studies) +
  geom_bar(aes(x = year_publication)) +
  theme_minimal() +
  labs(x = "Publication year",
       y = "Number of publications",
       title = "Studies reporting rodent trapping in West African countries",
       caption = paste("N =", length(unique(studies$unique_id)), sep = " "))

studies %<>%
  full_join(., rodent_data %>%
              distinct(unique_id, country),
            by = "unique_id")
studies$iso2 <- countrycode(as.character(studies$country), "country.name", "iso2c")

studies %>%
  group_by(country, iso2) %>%
  summarise(n = n()) %>%
  drop_na(country) %>%
  ggplot(aes(x = reorder(country, n), y = n)) +
  geom_col() +
  geom_flag(aes(image = iso2, y = -2)) +
  coord_flip() +
  theme_minimal() +
  labs(
    x = "Number of studies",
    y = "Country",
    title = "Countries with rodent trapping studies",
    caption = paste("N =", length(unique(studies$unique_id)), sep = " ")
  )

rodent_data %<>%
  separate(col = longitude_DMS_W, into = c("long_degrees", "long_minutes", "long_seconds"), "_") %>%
  separate(col = latitude_DMS_N, into = c("lat_degrees", "lat_minutes", "lat_seconds"), "_") %>%
  mutate(across(all_of(c("long_degrees", "long_minutes", "long_seconds","lat_degrees", "lat_minutes", "lat_seconds")), as.double),
         long_hemi = ifelse(long_degrees<0, "E", "W"),
         lat_hemi = ifelse(lat_degrees<0, "S", "N"),
         gps_dms = ifelse(is.na(lat_degrees|long_degrees), F, T),
         long_dms = ifelse(gps_dms == T,
                           paste(long_degrees, 'd',
                                 ifelse(is.na(long_minutes), 0, long_minutes), '\'',
                                 ifelse(is.na(long_seconds), 0, long_seconds), '" ',
                                 long_hemi,
                                 sep = ""), NA),
         lat_dms = ifelse(gps_dms == T,
                           paste(lat_degrees, 'd',
                                 ifelse(is.na(lat_minutes), 0, lat_minutes), '\'',
                                 ifelse(is.na(lat_seconds), 0, lat_seconds), '" ',
                                 lat_hemi,
                                 sep = ""), NA))

dms <- rodent_data %>%
  drop_na(long_dms, lat_dms)
dms$long_dms <- as.numeric(char2dms(dms[["long_dms"]]))
dms$lat_dms <- as.numeric(char2dms(dms[["lat_dms"]]))

dd <- rodent_data %>%
  drop_na(longitude_D_E,latitude_D_N)
dd$long_dms <- as.numeric(dd2dms(dd[["longitude_D_E"]]))
dd$lat_dms <- as.numeric(dd2dms(dd[["latitude_D_N"]]))

utm <- rodent_data %>%
  drop_na(UTM_coordinates) %>%
  separate(col = UTM_coordinates, into = c("zone", "first", "second"), "_")
utm_q <- utm %>%
  filter(zone == "28Q")
utm_q_transform <- SpatialPoints(cbind(as.double(utm_q$first), as.double(utm_q$second)), proj4string = CRS("+proj=utm +zone=28Q +datum=WGS84"))
utm_q_transform <- spTransform(utm_q_transform, CRS("+proj=longlat +datum=WGS84"))
utm_q <- utm_q %>%
  mutate(long_dms = utm_q_transform@coords[,1],
         lat_dms = utm_q_transform@coords[,2])
utm_p <- utm %>%
  filter(zone == "28P")
utm_p_transform <- SpatialPoints(cbind(as.double(utm_p$first), as.double(utm_p$second)), proj4string = CRS("+proj=utm +zone=28P +datum=WGS84"))
utm_p_transform <- spTransform(utm_p_transform, CRS("+proj=longlat +datum=WGS84"))
utm_p <- utm_p %>%
  mutate(long_dms = utm_p_transform@coords[,1],
         lat_dms = utm_p_transform@coords[,2])

gps_data <- bind_rows(dms, dd, utm_q, utm_p)

no_gps <- rodent_data %>%
  filter(!record_id %in% gps_data$record_id) %>%
  mutate(long_dms = as.double(long_dms),
         lat_dms = as.double(lat_dms))

rodent_gps <- bind_rows(gps_data, no_gps) %>%
  select(unique_id, year_trapping, month_trapping, country, region, long_dms, lat_dms, town_village, habitat, intensity_use, genus, species, number, trap_night_unit, record_id) %>%
  rename("x" = long_dms,
         "y" = lat_dms)
