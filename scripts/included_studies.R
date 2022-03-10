
# Supplementary table 2 ---------------------------------------------------

studies <- read_rds(here("data_raw", "studies.rds"))

s_table_2 <- studies %>%
  rename("Author" = first_author,
         "Title" = title,
         "Journal" = journal_name,
         "DOI/ISBN/ISSN" = reference_uid,
         "Year" = year_publication) %>%
  mutate(Year = as.numeric(Year),
         `Publication link` = "link") %>%
  select(`Publication link`, Year, Author, Title, Journal, `DOI/ISBN/ISSN`, link) %>%
  flextable() %>%
  compose(j = "Publication link",
  value = as_paragraph(hyperlink_text(url = link, x = `Publication link`))) %>%
  colformat_num(big.mark = "") %>%
  set_caption(caption = "Supplementary Table 2: Included studies") %>%
  write_rds(here("tables", "supplementary_table_2.rds"))
