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


raw_property_tbl <- read_csv("00_Data/export_last_pull.csv") %>% 
  filter(propertyID =="194094")
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
  
  output$leaflet <- renderLeaflet({
    
    req(prepped_property)
    
    prepped_property() %>%
      leaflet(height = 600) %>%
      addTiles() %>%
      addMarkers(lng = ~Longitude, lat = ~Latitude, popup = ~text)
  })
 
  output$propertytbl <- renderReactable({
    
    req(prepped_property)
    
    prepped_property() %>% 
  select(price_num,type) %>%
  reactable(
  columns = list(
    price_num= colDef(
      name = "price",
      format = colFormat(separators = TRUE,prefix = "€")
    ),
    type = colDef(
      name = "type",
      defaultSortOrder = "desc"
      # ,
      # html = TRUE,
      # cell = function(value, index){
      #   sprintf('<a href="%s" target="_blank">%s</a>', prepped_property$uRL[index], value)
      # }
  )
        )
      )
   }) 
  
}

shinyApp(ui, server)