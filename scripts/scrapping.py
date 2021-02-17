import pandas as pd
import selenium as s
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import time


PATH = "C:\Program Files (x86)\chromedriver_win32\chromedriver.exe"
driver = webdriver.Chrome(PATH)

driver.get("https://www.biddit.be/fr/catalog/landing")
#print(driver.title)

#https://www.biddit.be/fr/catalog/landing
#

try:
  main=WebDriverWait(driver,10).until(
    EC.presence_of_element_located((By.CLASS_NAME,"bid-container"))
  )
  
  
  
  #data=main.text
  print(main.text)
  
  articles =main.find_elements_by_class_name("col-xl-4 col-lg-6 col-md-6 col-sm-12")
  for article in articles
      header=article.find_element_by_class_name("property-detail-info w-100")
      data=header.text
      print(header.text)

finally :
  driver.quit()
   
print(main.text)
search=driver.
driver.quit()
