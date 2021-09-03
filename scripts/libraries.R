if (!require("pacman")) install.packages("pacman")
pkgs =
  c("here",
    "tidyverse",
    "mgcv",
    "magrittr",
    "bib2df",
    "googledrive",
    "readxl",
    "countrycode",
    "ggimage",
    "ggspatial",
    "corrplot",
    "sf",
    "OpenStreetMap",
    "tmap",
    "cowplot",
    "maptools",
    "raster",
    "parzer",
    "terra",
    "mapview",
    "ggmap",
    "taxize",
    "distill",
    "leaflet",
    "RColorBrewer",
    "lubridate",
    "gt",
    "gtsummary",
    "lubridate",
    "see",
    "scales"
  )
pacman::p_load(pkgs, character.only = T)

if(!exists("google_api")) {
google_api <- rstudioapi::askForSecret("Google API Key")
}

if(!exists("ENTREZ_KEY")) {
  ENTREZ_KEY <- rstudioapi::askForSecret("Entrez API")
}

source(here::here("scripts", "functions.R"))
