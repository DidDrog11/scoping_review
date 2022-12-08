source(here::here("scripts", "manuscript_library.R"))

tbl_3 <- readRDS(here("data_clean", "studies.rds")) %>%
  filter(!is.na(reference_uid))

bib <- bib2df(here("citations", "include_final.bib")) %>%
  select(BIBTEXKEY, DOI, DOI.1, PMID, PMCID, ISSN, ISBN, URL) %>%
  mutate(reference = as.character(coalesce(DOI, PMID, ISSN, ISBN)))

matched_ref <- left_join(tbl_3, bib,
                         by = c("reference_uid" = "reference")) %>%
  select(year_publication, first_author, unique_id, reference_uid, BIBTEXKEY, DOI.1, PMID, PMCID, URL, link) %>%
  mutate(year_publication = as_date(year_publication, format = "%Y"),
         reference = paste("@", BIBTEXKEY, sep = "")) %>%
  select(-c(BIBTEXKEY, DOI.1, PMID, PMCID, reference_uid, link)) %>%
  select(reference, year_publication, first_author, unique_id, URL) %>%
  rename("Reference" = reference,
         "Year publication" = year_publication,
         "Author" = first_author,
         "Study unique identifier" = unique_id,
         "Link" = URL)

supplementary_table <- matched_ref %>%
  arrange(`Year publication`)

write_rds(supplementary_table, here("data_clean", "supplementary_table_2.rds"))


