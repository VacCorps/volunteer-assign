library(tidyverse)
library(geosphere)
library(rstudioapi)

source("R/zip_distance.R")
#read the us zip data
zip_data <- read_delim("Database/us-zip-code-latitude-and-longitude.csv",delim = ";") %>%
  select("Zip","State","Latitude","Longitude") %>%
  mutate(longlat = map2(Longitude,Latitude,list),
         Latitude = NULL,
         Longitude = NULL) %>%
  rename(zip = Zip)

# select the most recent volunteer form data
file_path <- selectFile()
vt_data <- read_csv(file_path) %>% mutate(ID = paste0("vt",row_number()))
colnames(vt_data)<- c("roles","certified","lang","more.lang",
                      "hours","days","community","distance",
                      "vaccine","exp","cv","name","email",
                      "phone","zip","about","waiver","date","vtID")
vt_close_zip <- vt_data %>% 
  select("vtID","zip","distance") %>% 
  mutate(zip = as.character(zip)) %>% 
  left_join(zip_data,by = "zip",keep = T) %>%
  select("vtID","longlat","distance","State")%>%
  mutate(state = State,vtLonglat = longlat, longlat = NULL, State = NULL) %>% 
  pmap(get_closest_zips,zip_data)
  

