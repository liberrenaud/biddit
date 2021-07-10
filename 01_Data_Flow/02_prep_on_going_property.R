library(here)
library(tidyverse)
library(readr)
library(leaflet)
library(reactable)
library(crosstalk)
library(gmt)
library(DataExplorer)

source("00_Functions/prepped_property.R")
source("00_Functions/prepped_school distance.R")

#Import data

fresh_pull <-  read_csv("00_Data/fresh_pull.csv")
key_cities <- read_csv("00_Data/key_cities_prepped.csv")
location_short <- read_csv("00_Data/postal_code_prepped.csv")
immersion_school_tbl <- read_rds("00_Data/immersion_school.rds") 
city_name <-  read.csv("00_Data/code-postaux-belge.csv",sep=";") %>% 
  select(Code,Localite)


#Define the parameters

#Favourite type of "property"
favor_type_s <- c("Maison","Bungalow","Terrain à bâtir","Bois","Chalet","Ferme","Fermette","Manoir","Maison bel-étage")

#Define the favorite location to live (2 digit area of the postal code)
favor_location_s <- c(13,14,15,16,17,18,19,50,51,53,55,56,42,62,69,67,66,68,69,56)


# Prepare biddit pull data set (feature engineering)
cleaned_property <- fresh_pull %>% 
  prepare_property() 

#Needed sometime for testing
data <- fresh_pull

#Define the Belgium postal codes that are within 10Km of an Immersion school
distance_schools <-prepare_school_distance() 


#Cross the city close to immersion school within the favourite locations
city_immersion_school <- distance_schools %>% 
  mutate(
    county = str_sub(postcode_city , 1L, 2L),
    location_prime = case_when(county %in% favor_location_s ~ 1,
                               TRUE ~ 0)
  ) %>% 
  filter(location_prime==1) %>% 
  group_by(postcode_city) %>% 
  summarise(dist_school=min(dist_school)) %>% 
  ungroup() %>% 
  left_join(location_short,
            by=c('postcode_city'='Code'))



# nested_city_immersion <- distance_schools %>% 
#   select(-dist_school) %>% 
#   group_by(postcode_city) %>% 
#   nest()
#cleaned_property %>%  DataExplorer::plot_missing()


# Prepare here a format so that it can be translated to a web url
immersion_column <- city_immersion_school %>% 
  distinct(postcode_city) %>% 
  mutate(postcode_city=str_glue('BE-{postcode_city},'))
  
immersion_column[['postcode_city']]


#BE-3500,BE-4300

# FUTURE IDEAS Apply then the algorithm to get the price prediction
distance_schools %>% filter(postcode_city==4030)


# Export to the R app

cleaned_property %>%
  write_csv("ShinyApp/00_Data/export_last_pull.csv")

install.packages("ShinyQuickStarter")


























### SOME STUFF TO TURN IN FUNCTIONS?

cleaned_property %>%
  leaflet(height = 600) %>%
  addTiles() %>%
  addMarkers(lng = ~Longitude, lat = ~Latitude, popup = ~text)



city_immersion_school %>%
  leaflet(height = 600) %>%
  addTiles() %>%
  addMarkers(lng = ~Longitude, lat = ~Latitude, popup = ~text)


tbl_details <- cleaned_property %>% 
  select(id,type,price_num,land_surface,building_year,house_surface,building_state)





# reactable(
#   high_level,
#   columns = list(
#     id=colDef(show = FALSE)),
#   details = function(index) {
#     houses <- filter(tbl_details, id == high_level$id[index]) %>% select(-id)
#     tbl <- reactable(houses, 
#                      outlined = TRUE,
#                      highlight = TRUE, 
#                      fullWidth = FALSE,
#                      columns = list(
#                        land_surface = colDef(
#                          na = "–",
#                          "Size Land",
#                          format = colFormat(separators = TRUE,suffix = " m²")),
#                        building_year= colDef(
#                          na = "–",
#                          "Year Built"),
#                        house_surface= colDef(
#                          na = "–",
#                          "Surface House",
#                          format = colFormat(separators = TRUE,suffix = " m²"))
#                      ))
#     htmltools::div(style = list(margin = "12px 45px"), tbl)
#   },
#   onClick = "expand",
#   rowStyle = list(cursor = "pointer")
# )
# 
high_level <- cleaned_property %>%
  select(type, Fresh_flag,price,land_surface,building_year,house_surface,uRL,price_num) %>%
  mutate(Fresh_flag=if_else(Fresh_flag=="Old_property","Old","New"),
         price_num=price_num/1000)
# 
# status_badge <- function(color = "#aaa", width = "9px", height = width) {
#   span(style = list(
#     display = "inline-block",
#     marginRight = "8px",
#     width = width,
#     height = height,
#     backgroundColor = color,
#     borderRadius = "50%"
#   ))
# }



reactable(
  high_level,
  striped = TRUE,
  columns = list(
    
    # type=colDef(
    #   cell = function(value, index)  {
    #     # Render as a link
    #     url <- sprintf("https://www.biddit.be/fr/catalog/detail/%s", data[index, "propertyID"], value)
    #     htmltools::tags$a(href = url, target = "_blank", as.character(value))
    #   }),
    price_num=colDef(
      "Price",
      format = colFormat(separators = TRUE,suffix = " k€")
    ),
    Fresh_flag= colDef(
      "Recency"
      # ,
      # cell = function(value) {
      #   color <- switch(
      #     value,
      #     New = "hsl(214, 45%, 50%)",
      #     Old = "hsl(3, 69%, 50%)"
      #   )
      #   badge <- status_badge(color = color)
      #   tagList(badge, value)
      # }
    ),
    land_surface = colDef(
      na = "–",
      "Size Land",
      format = colFormat(separators = TRUE,suffix = " m²")),
    building_year= colDef(
      na = "–",
      "Year Built"),
    house_surface= colDef(
      na = "–",
      "Surface House",
      format = colFormat(separators = TRUE,suffix = " m²")),
    uRL=colDef(show = FALSE)
    ))
