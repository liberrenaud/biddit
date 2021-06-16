prepare_school_distance <- function(data){
  
  
  # Preparation of the integration of the school
  
  
  # Retrieve school master
  school_coordinate_tbl <- immersion_school_tbl %>% 
    left_join(location_short 
              ,by=c('postal_code'='Code')) %>% 
    filter(!is.na(Latitude))
  
  #Create a short table
  school_coordinate_short_tbl <- school_coordinate_tbl %>% 
    select(postal_code,Longitude,Latitude) %>% 
    distinct()
  
  # Expension of the data fram
  
  ## Retrieve postal code (BE and school) as vector 
  vec_BE_PostCode <-  pull(location_short, Code) %>% unique()
  vec_School_PostCode <-  pull(immersion_school_tbl, postal_code) %>% unique()
  ## Create a cross reference to expand
  grid_post_code <- expand.grid(vec_BE_PostCode,vec_School_PostCode)
  
  school_coordinate_short_tbl %>% DataExplorer::plot_missing()
  
  
  # Calculating distance between each postal code and school location
  distance_schools <- grid_post_code %>% as_tibble() %>% 
    rename('postcode_city'='Var1',
           'postcode_school'='Var2') %>% 
    left_join(location_short,
              by=c("postcode_city"="Code")) %>% 
    rename('long_city'='Longitude',
           'lat_city'='Latitude') %>% 
    select(-Coordonnees) %>% 
    left_join(school_coordinate_short_tbl,
              by=c("postcode_school"="postal_code")) %>% 
    rename('long_school'='Longitude',
           'lat_school'='Latitude') %>% 
    #filter(postcode_city ==1050) %>%
    mutate(
      dist_school = geodist(
        lat_city ,
        long_city ,
        lat_school,
        long_school,
        units = "km"
      )
    ) %>% 
    filter(dist_school<=15) %>% 
    select(postcode_city, postcode_school,dist_school)
  
}
