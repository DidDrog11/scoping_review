if (!require("pacman")) install.packages("pacman")
pkgs =
  c("here",
    "tidyverse",
    "magrittr",
    "bib2df",
    "googledrive",
    "readxl",
    "countrycode",
    "ggimage",
    "sf",
    "OpenStreetMap",
    "tmap",
    "maptools",
    "raster",
    "mapview",
    "ggmap"
  )
pacman::p_load(pkgs, character.only = T)

if(!exists("google_api")) {
google_api <- rstudioapi::askForSecret("Google API Key")
}

source(here::here("scripts", "functions.R"))
