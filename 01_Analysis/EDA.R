library(here)
library(tidyverse)
library(readr)
library(leaflet)


# Import Data----
raw_property_tbl <- read_csv("00_Data/Property_details.csv")
location <- read_delim("00_Data/code-postaux-belge.csv",delim = ";")

# Prep Commune Data----

location_short <- location %>% 
  group_by(Code) %>% 
  summarise(Longitude=first(Longitude),
            Latitude=first(Latitude),
            Coordonnees=first(Coordonnees)) 


# Define favourites

favor_type <- c("Maison","Bungalow","Terrain à bâtir","Bois","Chalet","Ferme","Fermette","Manoir","Maison bel-étage")
favor_location <- c(68,88,67,47)

prepped_property <- raw_property_tbl %>% 
  filter(type %in% favor_type) %>% 
  mutate(zip=str_extract(location,"[[:digit:]]+") %>% as.numeric(),
         county=str_sub(zip,1L,2L),
         location_prime=case_when(county %in% favor_location ~ 1,
                                  TRUE ~ 0)
         ) %>% 
  left_join(location_short,
            by=c("zip"="Code"))



# Map ----

#https://stackoverflow.com/questions/32940617/change-color-of-leaflet-marker

prepped_property %>%
  filter(location_prime==1) %>% 
  leaflet() %>%
  addTiles() %>%
  addMarkers(lng = ~Longitude, lat = ~Latitude, popup = ~description )
