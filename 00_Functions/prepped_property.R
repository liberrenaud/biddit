prepare_property <-
  function(data,
           favor_type=favor_type_s,
           favor_location=favor_location_s
           ) {
    
    # Parameter used to taken action in data cleansing step
    land_type <- c("Terrain à bâtir", "Bois")
    
    
    
    data %>%
      
      
      filter(type %in% favor_type) %>%
      
      # Extract Key features from strings
      mutate(
        zip = str_extract(location, "[[:digit:]]+") %>% as.numeric(),
        county = str_sub(zip, 1L, 2L),
        building_year = str_extract(details, "de construction\n[[:digit:]]+\n") %>%
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
        ) %>% 
          str_remove("État du bâtiment"),
        
        # Fill in dummy value when key feature not relevant
        
        building_year = case_when(type %in% land_type ~ 0,
                                  TRUE                 ~ building_year),
        house_surface = case_when(type %in% land_type ~ 0,
                                  TRUE                 ~ house_surface),
        non_shared_walls = case_when(type %in% land_type ~ 0,
                                     TRUE                 ~ non_shared_walls),
        building_state = case_when(type %in% land_type ~ "No House",
                                     TRUE                 ~ building_state),

        # Create Favorite location and description items
        
        location_prime = case_when(county %in% favor_location ~ 1,
                                   TRUE ~ 0),
        price_num = str_remove(price,"\\.") %>% str_remove("\\€") %>% as.numeric(),
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
      
      mutate(
        # Calculate distance from key cities
        
        dist_Bru = geodist(
          Latitude ,
          Longitude ,
          key_cities$Lat_Bruxelles,
          key_cities$Long_Bruxelles,
          units = "km"
        ),
        dist_Ghe = geodist(
          Latitude ,
          Longitude ,
          key_cities$Lat_Ghent,
          key_cities$Long_Ghent,
          units = "km"
        ),
        dist_Antw = geodist(
          Latitude ,
          Longitude ,
          key_cities$Lat_Antwerp,
          key_cities$Long_Antwerp,
          units = "km"
        ),
        dist_Leuv = geodist(
          Latitude ,
          Longitude ,
          key_cities$Lat_Leuven,
          key_cities$Long_Leuven,
          units = "km"
        ),
        dist_Charl = geodist(
          Latitude ,
          Longitude ,
          key_cities$Lat_Charleroi,
          key_cities$Long_Charleroi,
          units = "km"
        ),
        dist_Nam = geodist(
          Latitude ,
          Longitude ,
          key_cities$Lat_Namue,
          key_cities$Long_Namue,
          units = "km"
        ),
        dist_Lieg = geodist(
          Latitude ,
          Longitude ,
          key_cities$Lat_Liege,
          key_cities$Long_Liege,
          units = "km"
        ),
        dist_Mons = geodist(
          Latitude ,
          Longitude ,
          key_cities$Lat_Mons,
          key_cities$Long_Mons,
          units = "km"
        ),
        last_pull = format(Sys.time(), "%d %m %Y")
      ) 
  }
