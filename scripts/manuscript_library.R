if (!require("pacman")) install.packages("pacman")
pkgs =
  c("here",
    "tidyverse",
    "gt",
    "gtsummary",
    "countrycode"
  )
pacman::p_load(pkgs, character.only = T)
