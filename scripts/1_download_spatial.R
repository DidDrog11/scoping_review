source(here::here("scripts", "libraries.R"))
rodent_spatial <- read_rds(here("data_clean", "rodent_spatial.rds"))

countries <- rodent_spatial %>%
  drop_na(country) %>%
  mutate(iso3 = countrycode(as.character(country), "country.name", "iso3c"))
countries <- unique(countries$iso3)

# Level 0 -----------------------------------------------------------------
#WA
BEN_0 <- getData("GADM", country = "Benin", level = 0, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
BFA_0 <- getData("GADM", country = "BFA", level = 0, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
CPV_0 <- getData("GADM", country = "CPV", level = 0, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
GMB_0 <- getData("GADM", country = "GMB", level = 0, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
GHA_0 <- getData("GADM", country = "Ghana", level = 0, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
GIN_0 <- getData("GADM", country = "Guinea", level = 0, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
GNB_0 <- getData("GADM", country = "GNB", level = 0, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
CIV_0 <- getData("GADM", country = "CIV", level = 0, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
LBR_0 <- getData("GADM", country = "LBR", level = 0, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
MLI_0 <- getData("GADM", country = "Mali", level = 0, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
MRT_0 <- getData("GADM", country = "Mauritania", level = 0, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
NER_0 <- getData("GADM", country = "Niger", level = 0, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
NGA_0 <- getData("GADM", country = "Nigeria", level = 0, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
SEN_0 <- getData("GADM", country = "Senegal", level = 0, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
SLE_0 <- getData("GADM", country = "SL", level = 0, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
TGO_0 <- getData("GADM", country = "Togo", level = 0, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()

#non-WA
TCD_0 <- getData("GADM", country = "Chad", level = 0, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
MAR_0 <- getData("GADM", country = "Morocco", level = 0, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
CMR_0 <- getData("GADM", country = "Cameroon", level = 0, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
ESH_0 <- getData("GADM", country = "ESH", level = 0, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
DZA_0 <- getData("GADM", country = "Algeria", level = 0, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()

level_zero <- ls(pattern = "_0")
countries_level_zero <- do.call("list", mget(level_zero))

write_rds(bind_rows(countries_level_zero), here("data_download", "admin_spatial", "level_0_admin.rds"))

# Level 1 -------------------------------------------------

BEN_1 <- getData(name = "GADM", country ="BEN", level = 1, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
BFA_1 <- getData(name = "GADM", country ="BFA", level = 1, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
CIV_1 <- getData(name = "GADM", country ="CIV", level = 1, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
GHA_1 <- getData(name = "GADM", country ="GHA", level = 1, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
GIN_1 <- getData(name = "GADM", country ="GIN", level = 1, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
GNB_1 <- getData(name = "GADM", country ="GNB", level = 1, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
LBR_1 <- getData(name = "GADM", country ="LBR", level = 1, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
MLI_1 <- getData(name = "GADM", country ="MLI", level = 1, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
MRT_1 <- getData(name = "GADM", country ="MRT", level = 1, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
NER_1 <- getData(name = "GADM", country ="NER", level = 1, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
NGA_1 <- getData(name = "GADM", country ="NGA", level = 1, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
SEN_1 <- getData(name = "GADM", country ="SEN", level = 1, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
SLE_1 <- getData(name = "GADM", country ="SLE", level = 1, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
CPV_1 <- getData("GADM", country = "CPV", level = 1, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()

level_one <- ls(pattern = "_1$")
countries_level_one <- do.call("list",mget(level_one))
write_rds(bind_rows(countries_level_one), here("data_download", "admin_spatial", "level_1_admin.rds"))

# Level 2 -------------------------------------------------

BEN_2 <- getData(name = "GADM", country ="BEN", level = 2, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
BFA_2 <- getData(name = "GADM", country ="BFA", level = 2, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
CIV_2 <- getData(name = "GADM", country ="CIV", level = 2, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
GHA_2 <- getData(name = "GADM", country ="GHA", level = 2, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
GIN_2 <- getData(name = "GADM", country ="GIN", level = 2, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
GNB_2 <- getData(name = "GADM", country ="GNB", level = 2, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
LBR_2 <- getData(name = "GADM", country ="LBR", level = 2, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
MLI_2 <- getData(name = "GADM", country ="MLI", level = 2, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
MRT_2 <- getData(name = "GADM", country ="MRT", level = 2, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
NER_2 <- getData(name = "GADM", country ="NER", level = 2, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
NGA_2 <- getData(name = "GADM", country ="NGA", level = 2, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
SEN_2 <- getData(name = "GADM", country ="SEN", level = 2, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
SLE_2 <- getData(name = "GADM", country ="SLE", level = 2, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()
CPV_2 <- CPV_1 %>%
  st_as_sf()

level_two <- ls(pattern = "_2")
countries_level_two <- do.call("list",mget(level_two))
write_rds(bind_rows(countries_level_two), here("data_download", "admin_spatial", "level_2_admin.rds"))


# Non-trapped level 2 -----------------------------------------------------

nt_TGO <- getData(name = "GADM", country ="TGO", level = 2, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()

nt_GMB <- getData(name = "GADM", country ="GMB", level = 2, path = here("data_download", "admin_spatial")) %>%
  st_as_sf()

write_rds(bind_rows(nt_GMB, nt_TGO), here("data_download", "admin_spatial", "level_2_TGOGMB.rds"))
