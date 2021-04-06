library(here)
library(tidyverse)
library(readr)
library(leaflet)
library(reactable)
library(crosstalk)
library(gmt)




# Import Data----

raw_property_tbl <- read_csv("00_Data/Property_details.csv")
bid_hist_tbl <- read_csv("00_Data/Extract_Archives.bidhis.csv")
old_property_tbl <- read_csv("00_Data/Extract_Archives.csv")


data <- raw_property_tbl


# Prepare Historical bids ---- 

n_split <- max(sapply(strsplit(as.character(bid_hist_tbl$history),'\n'),length))

bid_hist_tbl %>% 
  separate(history, paste0("Y",1:n_split),sep = "\n") %>% 
  pivot_longer(contains("Y"),names_to = "Name",values_to = "value") %>% 
  filter(str_detect(value,"€")) %>% 
  mutate(value=str_remove(value,"€ ")%>% str_remove("[[:punct:]]"),
         value=as.integer(value)) %>% 
  group_by(id) %>% 
  mutate(Max_bid=max(value),
         start_price=min(value),
         n_bid=n()) %>% 
  ungroup() %>% 
  select(-Name,-value) %>% 
  distinct()
  
raw_property_tbl %>% select(statusAuction)

raw_property_tbl %>% 
  select(propertyID) %>% 
  distinct()

#

previous_pull <- read_csv("00_Data/previous_pull.csv") %>% 
  select(propertyID) %>% 
  mutate(Fresh_flag="Old_property",
         propertyID=as.character(propertyID))








# Define favourites----

favor_type <- c("Maison","Bungalow","Terrain à bâtir","Bois","Chalet","Ferme","Fermette","Manoir","Maison bel-étage")
favor_location <- c(13,14,50,51,53,42,62,56)

prepped_property <- raw_property_tbl 


# Function to prepare the porperties----
prepare_property <- function(data) {
  data %>%
    
    
    filter(type %in% favor_type) %>%
    
    # Extract Key features
    mutate(
      zip = str_extract(location, "[[:digit:]]+") %>% as.numeric(),
      county = str_sub(zip, 1L, 2L),
      building_year = str_extract(details, "Année de construction\n[[:digit:]]+\n") %>%
        str_extract("[[:digit:]]+") %>% as.numeric() ,
      house_surface = str_extract(details, "Surface habitable\n[[:digit:]]+ m²\n") %>%
        str_extract("[[:digit:]]+") %>% as.numeric(),
      land_surface = str_extract(details, "Surface terrain\n[[:digit:]]+ m²\n") %>%
        str_extract("[[:digit:]]+") %>% as.numeric() ,
      non_shared_walls = str_extract(details, "Nombre de façades\n[[:digit:]]+\n") %>%
        str_extract("[[:digit:]]+") %>% as.numeric(),
      building_state = str_extract(
        details,
        "État du bâtiment\n[:alpha:]+\n|État du bâtiment\n[:alpha:]+[:blank:][:alpha:]+\n"
      ) ,

      # Create Favorite location and description items
      
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
      )
    ) %>% 
    left_join(location_short,
            by = c("zip" = "Code")) %>%
    left_join(previous_pull,
              by = "propertyID") %>%

    mutate(
      
      # Calculate distance from key cities
      
      test=(lat_long_key_cities$Lat_Bruxelles),
      test2=(lat_long_key_cities$Long_Bruxelles),
      dist_Bru=geodist( Latitude , Longitude ,key_cities$Lat_Bruxelles, key_cities$Long_Bruxelles, units="km"),
      dist_Ghe=geodist( Latitude , Longitude ,key_cities$Lat_Ghent, key_cities$Long_Ghent, units="km"),
      dist_Antw=geodist( Latitude , Longitude ,key_cities$Lat_Antwerp, key_cities$Long_Antwerp, units="km"),
      dist_Leuv=geodist( Latitude , Longitude ,key_cities$Lat_Leuven, key_cities$Long_Leuven, units="km"),
      dist_Charl=geodist( Latitude , Longitude ,key_cities$Lat_Charleroi, key_cities$Long_Charleroi, units="km"),
      dist_Nam=geodist( Latitude , Longitude ,key_cities$Lat_Namue, key_cities$Long_Namue, units="km"),
      dist_Lieg=geodist( Latitude , Longitude ,key_cities$Lat_Liege, key_cities$Long_Liege, units="km"),
      dist_Mons=geodist( Latitude , Longitude ,key_cities$Lat_Mons, key_cities$Long_Mons, units="km"),
      #dist_Bru=distm(c(Longitude , Latitude ), c(lat_long_key_cities$Long_Bruxelles, lat_long_key_cities$Lat_Bruxelles), fun = distHaversine),
      
      
      Fresh_flag = if_else(is.na(Fresh_flag), "New_property", Fresh_flag),
      last_pull = format(Sys.time(), "%d %m %Y")
    ) %>% view()
}




%>% old_property_tbl %>% 
  prepare_property() %>% 
  leaflet() %>%
  addTiles() %>%
  addMarkers(lng = ~Longitude, lat = ~Latitude, popup = ~text )  


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
  
c("prepare_property") %>% 
  dump(file = "00_Functions/prepped_property.R",
       append = FALSE)
