source(here::here("scripts", "libraries.r"))
studies <- read_rds(here("data_clean", "studies.rds"))
rodent_data <- read_rds(here("data_clean", "rodent_df.rds"))


# Publication year --------------------------------------------------------
ggplot(studies) +
  geom_bar(aes(x = year_publication)) +
  theme_minimal() +
  labs(x = "Publication year",
       y = "Number of publications",
       title = "Studies reporting rodent trapping in West African countries",
       caption = paste("N =", length(unique(studies$unique_id)), sep = " "))


# Study aim ---------------------------------------------------------------
table(studies$aim)
ecology_studies <- studies %>% filter(aim == "Ecology")
zoonoses_studies <- studies %>% filter(aim == "Zoonoses risk")


# Trap type and setup -----------------------------------------------------
trap_type <- studies %>%
  separate(col = trap_types, into = c("trap_1", "trap_2", "trap_3", "trap_4"), sep = ", ", remove = T) %>%
  pivot_longer(cols = c("trap_1", "trap_2", "trap_3", "trap_4"), values_to = "trap_type") %>%
  drop_na(trap_type)

# Study location ----------------------------------------------------------
countries <- studies %>%
  full_join(., rodent_data %>%
              distinct(unique_id, country),
            by = "unique_id") %>%
  distinct(unique_id, country)

countries$iso3 <- countrycode(as.character(countries$country), "country.name", "iso3c")

countries %>%
  group_by(country, iso3) %>%
  summarise(n = n()) %>%
  drop_na(country) %>%
  ggplot(aes(x = reorder(country, n), y = n)) +
  geom_col() +
  #geom_flag(aes(image = iso3, y = -2)) +
  coord_flip() +
  theme_minimal() +
  labs(
    x = "Number of studies",
    y = "Country",
    title = "Location of trapping activities for included studies",
    caption = paste("N =", length(unique(studies$unique_id)), sep = " ")
    )

#ggsave(plot = last_plot(),filename = here("figures", "studies_country.png"), device = "png")

a <- countries %>%
  filter(!country %in% c("Cameroon", "Chad", "Morocco")) %>%
  group_by(unique_id) %>%
  summarise(n = n())
table(a$n) # calculate the number of countries trapped in by each study

sites <- studies %>%
  full_join(., rodent_data  %>%
              filter(!country %in% c("Cameroon", "Chad", "Morocco")) %>%
              distinct(unique_id, region, town_village, habitat, geometry),
            by = "unique_id")

a <- sites %>%
  group_by(unique_id) %>%
  summarise(n = n())
table(a$n)

