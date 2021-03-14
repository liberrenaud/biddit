library(here)
library(tidyverse)
library(readr)
library(leaflet)
library(reactable)
library(crosstalk)


# Import Data----
raw_property_tbl <- read_csv("00_Data/Property_details.csv")

raw_property_tbl %>% 
  select(propertyID) %>% 
  distinct()

previous_pull <- read_csv("00_Data/export_last_pull.csv") %>% 
  select(propertyID) %>% 
  mutate(Fresh_flag="Old_property",
         propertyID=as.character(propertyID))

location <- read_delim("00_Data/code-postaux-belge.csv",delim = ";")

# Prep Commune Data----

location_short <- location %>% 
  group_by(Code) %>% 
  summarise(Longitude=first(Longitude),
            Latitude=first(Latitude),
            Coordonnees=first(Coordonnees)) 


# Define favourites

favor_type <- c("Maison","Bungalow","Terrain à bâtir","Bois","Chalet","Ferme","Fermette","Manoir","Maison bel-étage")
favor_location <- c(13,14,50,51,53,42,62,56)

prepped_property <- raw_property_tbl %>%
  filter(type %in% favor_type) %>%
  mutate(
    zip = str_extract(location, "[[:digit:]]+") %>% as.numeric(),
    county = str_sub(zip, 1L, 2L),
    location_prime = case_when(county %in% favor_location ~ 1,
                               TRUE ~ 0),
    price_num = str_remove(price, "€") %>% str_remove("\\.") %>% as.numeric(),
    text = str_glue(
      "<b>{price} euros</b><br/>
                        <a href='{uRL}'>{type}</a><br/>
                       {description}"
    ),
    auction_status = case_when(
      statusAuction %>% str_detect("Commence") ~ "Not started",
      TRUE ~ "Running Auction"
    ),
    propertyID = str_replace(propertyID, "Code: ", "")
    
  ) %>%
  left_join(location_short,
            by = c("zip" = "Code")) %>%
  left_join(previous_pull, 
            by = "propertyID") %>%
  mutate(
    Fresh_flag = if_else(is.na(Fresh_flag), "New_property", Fresh_flag),
    last_pull = format(Sys.time(), "%d %m %Y")
  )





# Export to project folder
prepped_property  %>% 
  write_csv("00_Data/export_last_pull.csv")


# Export to shiny folder
prepped_property  %>% 
  write_csv("ShinyApp/00_Data/export_last_pull.csv")


# Map ----

#https://stackoverflow.com/questions/32940617/change-color-of-leaflet-marker

prepped_property %>%  
  filter(location_prime==1) %>% 
  leaflet() %>%
  addTiles() %>%
  addMarkers(lng = ~Longitude, lat = ~Latitude, popup = ~text )  
  

prepped_property %>% 
  select(price_num,type) %>% 
  reactable(
    columns = list(
      price_num= colDef(
        name = "price",
        format = colFormat(separators = TRUE,prefix = "€")
      ),
      type = colDef(
        name = "type",
        defaultSortOrder = "desc",
        html = TRUE, 
        cell = function(value, index){
          sprintf('<a href="%s" target="_blank">%s</a>', prepped_property$url[index], value)
        }
      )
      )
    )


reactable(
  data,
  defaultSorted = "exclusive_followers_pct",
  columns = list(
    account = colDef(
      name = "Account",
      format = colFormat(prefix = "@")
    ),
    followers = colDef(
      name = "Followers",
      defaultSortOrder = "desc",
      format = colFormat(separators = TRUE)
    ),
    exclusive_followers_pct = colDef(
      name = "Exclusive Followers",
      defaultSortOrder = "desc",
      format = colFormat(percent = TRUE, digits = 1)
    )
  )
)
  
