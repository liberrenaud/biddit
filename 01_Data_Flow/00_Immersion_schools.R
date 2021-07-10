library(tidyverse)
library(pdftools)
library(tabulizer)
library(purr)

install.packages(c('pdftools','tabulizer'))

pdf_location="00_Data/Ecoles en immersion - annee scolaire 2020-2021 - Fondamental (ressource 15109).pdf"
col_names=c('Zone','Nom','Rue_1','Postal_Code_1','Localité_1','Rue_2','Postal_Code_2',"Localité_2","Langue")
old_names=c('V1','V2','v3','V4','V5','V6','v7','V8','V9') # Review the creation here

# area1 <-  locate_areas(pdf_location,pages =1)
# area2 <-  locate_areas(pdf_location,pages =2)
#must find how to extract vector from the list. Curently getting double

#Retrieve the data from the pdf file 
immersion_sch_table <- extract_tables(pdf_location,
                            output = "data.frame",
                            pages = c(1,2,3,4,5),
                            # area = list(
                            #   c(98.14221,  34.49431, 703.67143, 520.88405 ),
                            #   c(80.10378,  37.10312, 706.67315, 519.93815 ),
                            #   c(80.10378,  37.10312, 706.67315, 519.93815 ),
                            #   c(80.10378,  37.10312, 706.67315, 519.93815 ),
                            #   c(80.10378,  37.10312, 706.67315, 519.93815 )
                            # ),
                            guess = TRUE)


# PDF Extraction - Method to test with - automatic recognition
#test <- extract_tables(pdf_location, method = "lattice")


#Convert headless list to a tiblble df 
immersion_frame <- immersion_sch_table %>% 
  map(~set_names(., 
                 nm = col_names)) %>% 
  map(~as.data.frame(.,col.names=col_names)) %>% 
  bind_rows( .id = "column_label")%>% 
  as_tibble() %>% 
  select(!contains("_1"),-'column_label') %>% 
  janitor::clean_names() %>% 
  set_names(names(.) %>% str_remove("_2")) %>% 
  mutate(langue=if_else(str_detect(langue,"NL"),"NL",langue)) %>% 
  filter(!is.na(postal_code),
         langue=="NL")


#Checks for duplicates
immersion_frame %>% 
  distinct()

immersion_frame[duplicated(immersion_frame),] %>% view()

immersion_frame %>% count(langue)

#Export
immersion_frame %>% write_rds("00_Data/immersion_school.rds")
