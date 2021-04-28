library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(here)
library(tidyverse)
library(readr)
library(leaflet)
library(reactable)
library(crosstalk)
library(pins)


raw_property_tbl <- read_csv("00_Data/export_last_pull.csv") 
location <- read_delim("00_Data/code-postaux-belge.csv",delim = ";")



ui <- dashboardPage(
  dashboardHeader(title="Biddit Dashboard"),
  dashboardSidebar(
    
    pickerInput(inputId = 'fav_loc',
                label = 'Favourite location',
                choices = c("Favourite","All"),
                options = list(`style` = "btn-info")),
    
    pickerInput(inputId = 'new_old',
                label = 'New/All Property',
                choices = c("New","All"),
                options = list(`style` = "btn-info"))
  ),
  dashboardBody(
    fluidRow(
      box(
      leafletOutput("leaflet")
      ),
    box(
      reactableOutput("propertytbl")
    )
    )
  )
)

server <- function(input, output) {
  
  
  # Prep Commune Data----
  
  location_short <- location %>% 
    group_by(Code) %>% 
    summarise(Longitude=first(Longitude),
              Latitude=first(Latitude),
              Coordonnees=first(Coordonnees)) 
  
  

  prepped_property <- reactive({
    

    if(input$fav_loc =="Favourite") 
      raw_property_tbl <- raw_property_tbl %>%
        filter(location_prime==1)
    else raw_property_tbl
    
    
    if(input$new_old =="New") 
      raw_property_tbl <- raw_property_tbl %>%
        filter(Fresh_flag =="New_property")
    else raw_property_tbl
    

    })
  

  
  # Render Map
  
  output$leaflet <- renderLeaflet({
    
    req(prepped_property)
    
    prepped_property() %>%
      leaflet(height = 600) %>%
      addTiles() %>%
      addMarkers(lng = ~Longitude, lat = ~Latitude, popup = ~text)
  })
 
  
  
  # Render Table
  
  
  
  output$propertytbl <- renderReactable({

    req(prepped_property)

    prepped_property() %>%
      
      select(type, Fresh_flag,price_num,land_surface,building_year,house_surface,uRL) %>% 
      mutate(Fresh_flag=if_else(Fresh_flag=="Old_property","Old","New"),
             price_num=price_num/1000) %>% 
      
      reactable(
        striped = TRUE,
        selection = "multiple",
        onClick = "select",
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
   })
  
  
 
  
}

shinyApp(ui, server)