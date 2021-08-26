source(here::here("scripts", "libraries.R"))
source(here("scripts", "update_data.R"))

# Studies -----------------------------------------------------------------

studies <- read_rds(here("data_raw", "studies.rds")) %>%
  mutate(unique_id = as_factor(unique_id)) %>%
  separate(col = aim_detail, into = c("aim_detail_1", "aim_detail_2"), sep = ", ")

write_rds(studies, here("data_clean", "studies.rds"))
