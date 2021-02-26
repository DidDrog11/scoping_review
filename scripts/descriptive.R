source(here::here("scripts", "libraries.r"))

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
