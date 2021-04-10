source(here::here("scripts", "libraries.R"))

citations <- bib2df(here("citations", "manuscript_citations.bib")) #load citations from bibtex file exported from zotero and convert to df

missing_uid <- citations %>%
  filter(is.na(DOI) & is.na(ISSN) & is.na(ISBN) & is.na(PMID)) #identify records that do not contain a unique identifier in the form of a DOI, ISSN, ISBN or PMID

citations_uid <- citations %>%
  mutate(unique_id = case_when(!is.na(DOI) ~ DOI,
                               !is.na(ISSN) ~ ISSN,
                               !is.na(ISBN) ~ ISBN,
                               !is.na(PMID) ~ PMID))

included_studies <- bib2df(here("citations", "include_final.bib"))%>%
  mutate(unique_id = case_when(!is.na(DOI) ~ DOI,
                               !is.na(ISSN) ~ ISSN,
                               !is.na(ISBN) ~ ISBN,
                               !is.na(PMID) ~ PMID)) #load citations from the studies that were found to be suitable for inclusion in the review
