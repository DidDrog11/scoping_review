
# Supplementary figure 1 --------------------------------------------------

study_start <- rodent_data %>%
  distinct(unique_id, year_trapping) %>%
  separate(year_trapping, into = c("year_start", "year_end"), sep = "-") %>%
  group_by(unique_id) %>%
  mutate(year_start = as.numeric(year_start),
         year_end = case_when(is.na(year_end) ~ max(year_start),
                              TRUE ~ as.numeric(year_end))) %>%
  summarise(year_start = min(year_start, na.rm = T),
            year_end = max(year_end, na.rm = T)) %>%
  mutate(year_publication = as.numeric(substring(unique_id, 4, 7)),
         year_end = case_when(is.infinite(year_end) & !is.na(year_start) ~ year_start,
                              TRUE ~ year_end)) %>%
  mutate(reported = factor(case_when(is.infinite(year_start) ~ "No",
                                     TRUE ~ "Yes"), levels = c("No", "Yes")),
         year_start = case_when(is.infinite(year_start) ~ as.numeric(substring(unique_id, 4, 7)),
                                TRUE ~ year_start),
         year_end = case_when(is.infinite(year_end) ~ as.numeric(substring(unique_id, 4, 7)),
                              TRUE ~ year_end),
         publishing_delay = year_publication - year_end) %>%
  arrange(year_start) %>%
  mutate(unique_id = as_factor(fct_inorder(unique_id)))

study_timings <- ggplot(study_start) +
  geom_segment(aes(x = unique_id, xend = unique_id, y = year_start, yend = year_end), colour = "grey") +
  geom_point(aes(x = unique_id, y = year_start, alpha = reported), size = 3, colour = "#008b46") +
  geom_point(aes(x = unique_id, y = year_end, alpha = reported), size = 3, colour = "#00468b") +
  geom_point(aes(x = unique_id, y = year_publication), size = 1, colour = "#8B0000") +
  coord_flip() +
  theme_minimal() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.ticks.x = element_line(colour = "black")) +
  scale_y_continuous(breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020),
                     limits = c(1960, 2022)) +
  labs(y = "Year",
       x = element_blank(),
       alpha = "Study dates reported")

save_plot(plot = study_timings, filename = here("figures", "Supplementary_figure_1.png"), dpi = 300, base_height = 8, base_width = 10)
