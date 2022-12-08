source(here::here("scripts", "libraries.R"))

# double_checking <- bib2df(here("citations", "v4_search.bib")) %>%
#   sample_n(size = nrow(.)/10, replace = F)
#
# double_checking %>%
#   dplyr::select(BIBTEXKEY, JOURNAL, TITLE, ABSTRACT, ISSN, ISBN, DOI) %>%
#   write_csv(file = here("citations", "double_checks", "title_abstract.csv"))
#
# data_extraction <-  bib2df(here("citations", "include_final.bib")) %>%
#   sample_n(size = nrow(.)/10, replace = F)
#
# data_extraction %>%
#   dplyr::select(BIBTEXKEY, JOURNAL, TITLE, ABSTRACT, ISSN, ISBN, DOI) %>%
#   write_csv(file = here("citations", "double_checks", "data_extraction.csv"))
#
# title_abstract <- read_csv(here("citations", "double_checks", "title_abstract.csv"))
#
# double_checking <- bib2df(here("citations", "v4_search.bib"))
#
# double_checking %>%
#   filter(BIBTEXKEY %in% title_abstract$BIBTEXKEY) %>%
#   df2bib(file = here("citations", "double_checks", "title_abstract.bib"))
#
# double_checking %>%
#   filter(BIBTEXKEY %in% data_extraction$BIBTEXKEY) %>%
#   df2bib(file = here("citations", "double_checks", "data_extraction.bib"))

