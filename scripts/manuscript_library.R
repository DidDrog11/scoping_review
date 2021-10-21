if (!require("pacman")) install.packages("pacman")
pkgs =
  c("here",
    "tidyverse",
    "gt",
    "gtsummary",
    "countrycode",
    "cowplot",
    "flextable",
    "ftExtra",
    "bib2df",
    "lubridate"
  )
pacman::p_load(pkgs, character.only = T)
