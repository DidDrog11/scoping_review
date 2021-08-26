source(here::here("scripts", "libraries.r"))
rodent_spatial <- read_rds(here("data_clean", "rodent_spatial.rds"))


countries <- rodent_spatial %>%
  drop_na(country) %>%
  mutate(iso3 = countrycode(as.character(country), "country.name", "iso3c"))
countries <- unique(countries$iso3)

# Level 0 -----------------------------------------------------------------
#WA
BEN_0 <- getData("GADM", country = "Benin", level = 0, path = here("data_download", "admin_spatial"))
BFA_0 <- getData("GADM", country = "BFA", level = 0, path = here("data_download", "admin_spatial"))
CPV_0 <- getData("GADM", country = "CPV", level = 0, path = here("data_download", "admin_spatial"))
GMB_0 <- getData("GADM", country = "GMB", level = 0, path = here("data_download", "admin_spatial"))
GHA_0 <- getData("GADM", country = "Ghana", level = 0, path = here("data_download", "admin_spatial"))
GIN_0 <- getData("GADM", country = "Guinea", level = 0, path = here("data_download", "admin_spatial"))
GNB_0 <- getData("GADM", country = "GNB", level = 0, path = here("data_download", "admin_spatial"))
CIV_0 <- getData("GADM", country = "CIV", level = 0, path = here("data_download", "admin_spatial"))
LBR_0 <- getData("GADM", country = "LBR", level = 0, path = here("data_download", "admin_spatial"))
MLI_0 <- getData("GADM", country = "Mali", level = 0, path = here("data_download", "admin_spatial"))
MRT_0 <- getData("GADM", country = "Mauritania", level = 0, path = here("data_download", "admin_spatial"))
NER_0 <- getData("GADM", country = "Niger", level = 0, path = here("data_download", "admin_spatial"))
NGA_0 <- getData("GADM", country = "Nigeria", level = 0, path = here("data_download", "admin_spatial"))
SEN_0 <- getData("GADM", country = "Senegal", level = 0, path = here("data_download", "admin_spatial"))
SLE_0 <- getData("GADM", country = "SL", level = 0, path = here("data_download", "admin_spatial"))
TGO_0 <- getData("GADM", country = "Togo", level = 0, path = here("data_download", "admin_spatial"))

#non-WA
TCD_0 <- getData("GADM", country = "Chad", level = 0, path = here("data_download", "admin_spatial"))
MAR_0 <- getData("GADM", country = "Morocco", level = 0, path = here("data_download", "admin_spatial"))
CMR_0 <- getData("GADM", country = "Cameroon", level = 0, path = here("data_download", "admin_spatial"))
ESH_0 <- getData("GADM", country = "ESH", level = 0, path = here("data_download", "admin_spatial"))
DZA_0 <- getData("GADM", country = "Algeria", level = 0, path = here("data_download", "admin_spatial"))

level_zero <- ls(pattern = "_0")
countries_level_0 <- do.call("list", mget(level_zero))
write_rds(countries_level_0, here("data_download", "admin_spatial", "level_0_admin.rds"))

# Level 1 -------------------------------------------------

BEN_1 <- getData(name = "GADM", country ="BEN", level = 1, path = here("data_download", "admin_spatial"))
BFA_1 <- getData(name = "GADM", country ="BFA", level = 1, path = here("data_download", "admin_spatial"))
CIV_1 <- getData(name = "GADM", country ="CIV", level = 1, path = here("data_download", "admin_spatial"))
GHA_1 <- getData(name = "GADM", country ="GHA", level = 1, path = here("data_download", "admin_spatial"))
GIN_1 <- getData(name = "GADM", country ="GIN", level = 1, path = here("data_download", "admin_spatial"))
GNB_1 <- getData(name = "GADM", country ="GNB", level = 1, path = here("data_download", "admin_spatial"))
LBR_1 <- getData(name = "GADM", country ="LBR", level = 1, path = here("data_download", "admin_spatial"))
MLI_1 <- getData(name = "GADM", country ="MLI", level = 1, path = here("data_download", "admin_spatial"))
MRT_1 <- getData(name = "GADM", country ="MRT", level = 1, path = here("data_download", "admin_spatial"))
NER_1 <- getData(name = "GADM", country ="NER", level = 1, path = here("data_download", "admin_spatial"))
NGA_1 <- getData(name = "GADM", country ="NGA", level = 1, path = here("data_download", "admin_spatial"))
SEN_1 <- getData(name = "GADM", country ="SEN", level = 1, path = here("data_download", "admin_spatial"))
SLE_1 <- getData(name = "GADM", country ="SLE", level = 1, path = here("data_download", "admin_spatial"))
CPV_1 <- getData("GADM", country = "CPV", level = 1, path = here("data_download", "admin_spatial"))

level_one <- ls(pattern = "_1")
countries_level_1 <- do.call("list",mget(level_one))
write_rds(countries_level_1, here("data_download", "admin_spatial", "level_1_admin.rds"))

# Level 2 -------------------------------------------------

BEN_2 <- getData(name = "GADM", country ="BEN", level = 2, path = here("data_download", "admin_spatial"))
BFA_2 <- getData(name = "GADM", country ="BFA", level = 2, path = here("data_download", "admin_spatial"))
CIV_2 <- getData(name = "GADM", country ="CIV", level = 2, path = here("data_download", "admin_spatial"))
GHA_2 <- getData(name = "GADM", country ="GHA", level = 2, path = here("data_download", "admin_spatial"))
GIN_2 <- getData(name = "GADM", country ="GIN", level = 2, path = here("data_download", "admin_spatial"))
GNB_2 <- getData(name = "GADM", country ="GNB", level = 2, path = here("data_download", "admin_spatial"))
LBR_2 <- getData(name = "GADM", country ="LBR", level = 2, path = here("data_download", "admin_spatial"))
MLI_2 <- getData(name = "GADM", country ="MLI", level = 2, path = here("data_download", "admin_spatial"))
MRT_2 <- getData(name = "GADM", country ="MRT", level = 2, path = here("data_download", "admin_spatial"))
NER_2 <- getData(name = "GADM", country ="NER", level = 2, path = here("data_download", "admin_spatial"))
NGA_2 <- getData(name = "GADM", country ="NGA", level = 2, path = here("data_download", "admin_spatial"))
SEN_2 <- getData(name = "GADM", country ="SEN", level = 2, path = here("data_download", "admin_spatial"))
SLE_2 <- getData(name = "GADM", country ="SLE", level = 2, path = here("data_download", "admin_spatial"))
CPV_2 <- CPV_1

level_two <- ls(pattern = "_2")
countries_level_2 <- do.call("list",mget(level_two))
write_rds(countries_level_2, here("data_download", "admin_spatial", "level_2_admin.rds"))
