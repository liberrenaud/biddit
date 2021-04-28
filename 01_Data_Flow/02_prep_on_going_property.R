library(here)
library(tidyverse)
library(readr)
library(leaflet)
library(reactable)
library(crosstalk)
library(gmt)
library(DataExplorer)

source("00_Functions/prepped_property.R")


fresh_pull <-  read_csv("00_Data/fresh_pull.csv")
key_cities <- read_csv("00_Data/key_cities_prepped.csv")
location_short <- read_csv("00_Data/postal_code_prepped.csv")

favor_type <- c("Maison","Bungalow","Terrain à bâtir","Bois","Chalet","Ferme","Fermette","Manoir","Maison bel-étage")
favor_location <- c(13,14,50,51,53,42,62,56)

cleaned_property <- fresh_pull %>% 
  prepare_property() 

data <- fresh_pull


cleaned_property %>%  DataExplorer::plot_missing()

# Apply then the algorithm to get the price prediction



# Export to the R app

cleaned_property %>%
  write_csv("ShinyApp/00_Data/export_last_pull.csv")


cleaned_property %>%
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
