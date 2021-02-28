library(rvest)
library(tidyverse)
library(progress)
library(here)
library(reticulate)

# Scraping Functions ------------------------------------------------------


"https://www.biddit.be/fr/catalog/detail/191652" %>% 
  read_html() %>% 
  html_nodes(".property-ref") %>% 
  html_text()  %>% 
  tibble("cards" = .)
}

reticulate::conda_list()
conda create -n py3.8 python=3.8 scikit-learn pandas numpy matplotlib

reticulate::py_config()
