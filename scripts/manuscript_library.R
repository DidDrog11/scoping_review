devtools::install_github("yutannihilation/ggsflabel")

if (!require("pacman")) install.packages("pacman")
pkgs =
  c("here",
    "tidyverse",
    "ggsflabel",
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

