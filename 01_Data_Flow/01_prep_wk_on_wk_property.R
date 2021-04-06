library(here)
library(tidyverse)
library(readr)
library(leaflet)
library(reactable)
library(crosstalk)
library(gmt)




# Import Data----


# Previous pull - to be used to flag new and old property
previous_pull <- read_csv("00_Data/previous_pull.csv") %>% 
  select(propertyID) %>% 
  mutate(Fresh_flag="Old_property",
         propertyID=as.character(propertyID))


# Fresh pull
raw_property_tbl <- read_csv("00_Data/Property_details.csv")%>%
  mutate(propertyID = str_remove(propertyID, "Code: ")) %>% 
  left_join(previous_pull,
            by = "propertyID") %>% 
  mutate(
    Fresh_flag = if_else(is.na(Fresh_flag), "New_property", Fresh_flag)
  ) 


raw_property_tbl  %>% 
  write_csv("00_Data/fresh_pull.csv")

raw_property_tbl  %>% 
  write_csv("00_Data/previous_pull.csv")





