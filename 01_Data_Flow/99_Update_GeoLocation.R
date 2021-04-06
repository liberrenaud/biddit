library(here)
library(tidyverse)


key_cities_lat_long <- read_csv("00_Data/Key_Cities_Lat_Long.csv")
location <- read_delim("00_Data/code-postaux-belge.csv",delim = ";")





# Prep Commune Data----

location_short <- location %>% 
  group_by(Code) %>% 
  summarise(Longitude=first(Longitude),
            Latitude=first(Latitude),
            Coordonnees=first(Coordonnees)) 


# Set the Key Cities in country----

key_cities <- key_cities_lat_long %>% 
  pivot_longer(!City,names_to = "type", values_to = "value") %>% 
  unite(type,type,City) %>% 
  pivot_wider(names_from=type,values_from = value) 



location_short   %>% 
  write_csv("00_Data/postal_code_prepped.csv")

key_cities%>% 
  write_csv("00_Data/key_cities_prepped.csv")
