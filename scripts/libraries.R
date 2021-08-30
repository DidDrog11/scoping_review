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
    "lubridate"
  )
pacman::p_load(pkgs, character.only = T)

if(!exists("google_api")) {
google_api <- rstudioapi::askForSecret("Google API Key")
}

if(!exists("ENTREZ_KEY")) {
  ENTREZ_KEY <- rstudioapi::askForSecret("Entrez API")
}

source(here::here("scripts", "functions.R"))
