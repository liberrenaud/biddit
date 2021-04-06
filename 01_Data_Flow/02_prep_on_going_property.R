library(here)
library(tidyverse)
library(readr)
library(leaflet)
library(reactable)
library(crosstalk)
library(gmt)

source("00_Functions/prepped_property.R")


fresh_pull <-  read_csv("00_Data/fresh_pull.csv")
key_cities <- read_csv("00_Data/key_cities_prepped.csv")
location_short <- read_csv("00_Data/postal_code_prepped.csv")

favor_type <- c("Maison","Bungalow","Terrain à bâtir","Bois","Chalet","Ferme","Fermette","Manoir","Maison bel-étage")
favor_location <- c(13,14,50,51,53,42,62,56)

fresh_pull %>% 
  prepare_property()



# Apply then the algorithm to get the price prediction