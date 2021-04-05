favor_type <- c("Maison","Bungalow",
                #"Terrain à bâtir","Bois",
                "Chalet","Ferme","Fermette","Manoir","Maison bel-étage")


old_property_tbl %>% 
  filter(!is.na(price)) %>% 
  prepare_property() %>% 
  view()
