library("bib2df")
library("here")
library("tidyverse")

citations <- bib2df(here("citations", "initial_search.bib")) #load citations from bibtex file exported from zotero and convert to df

missing_uid <- citations %>%
  filter(is.na(DOI) & is.na(ISSN) & is.na(ISBN) & is.na(PMID)) #identify records that do not contain a unique identifier in the form of a DOI, ISSN, ISBN or PMID

