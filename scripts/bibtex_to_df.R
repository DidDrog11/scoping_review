source(here::here("scripts", "libraries.R"))

citations <- bib2df(here("citations", "final_search.bib")) #load citations from bibtex file exported from zotero and convert to df

missing_uid <- citations %>%
  filter(is.na(DOI) & is.na(ISSN) & is.na(ISBN) & is.na(PMID)) #identify records that do not contain a unique identifier in the form of a DOI, ISSN, ISBN or PMID

citations_uid <- citations %>%
  mutate(unique_id = ifelse(!is.na(DOI), DOI,
                            ifelse(!is.na(ISSN), ISSN,
                                   ifelse(!is.na(ISBN), ISBN,
                                          ifelse(!is.na(PMID), PMID, NA)))))

included_studies <- bib2df(here("citations", "included_studies.bib"))%>%
  mutate(unique_id = ifelse(!is.na(DOI), DOI,
                            ifelse(!is.na(ISSN), ISSN,
                                   ifelse(!is.na(ISBN), ISBN,
                                          ifelse(!is.na(PMID), PMID, NA))))) #load citations from the studies that were found to be suitable for inclusion in the review

expanded_search <- bib2df(here("citations", "v4_search.bib"))

search_4 <- expanded_search %>%
  mutate(unique_id = ifelse(!is.na(DOI), DOI,
                            ifelse(!is.na(ISSN), ISSN,
                                   ifelse(!is.na(ISBN), ISBN,
                                          ifelse(!is.na(BIBTEXKEY), BIBTEXKEY, NA)))))
