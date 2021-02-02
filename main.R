library(tidyverse)
library(geosphere)
library(rstudioapi)

source("R/zip_distance.R")
#read the us zip data
zip_data <- read_delim("Database/us-zip-code-latitude-and-longitude.csv",delim = ";") %>%
  select("Zip","State","Latitude","Longitude") %>%
  mutate(longlat = map2(Longitude,Latitude,c),
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


# a long tibble of two columns, volunteer ID and Zip code that lies within their preferred driving distance
vt_close_zip <- vt_data %>% 
  select("vtID","zip","distance") %>% 
  mutate(zip = as.character(zip)) %>% 
  left_join(zip_data,by = "zip",keep = T) %>%
  select("vtID","longlat","distance","State")%>%
  mutate(state = State,vtLonglat = longlat, longlat = NULL, State = NULL) %>% 
  pmap(get_closest_zips,zip_data)%>% 
  map_df(~as.data.frame(.x)) %>% 
  pivot_longer(cols = vt_data$vtID,values_to = "zip",names_to = "vtID") %>% 
  filter(!is.na(zip))

# a long tibble of two columns, volunteer ID and their preferred role
vt_roles <- vt_data %>% 
  select("vtID","roles") %>% 
  pmap_dfc(~set_names(str_split(..2,"\n"),..1)) %>% 
  pivot_longer(cols =everything(), names_to = "vtID",values_to = "role")

# a long tibble of three columns, volunteer ID, the duration they wish to volunteer and their preferred days
vt_day_time <- vt_data %>%
  select("vtID","hours","days") %>% 
  separate(days,into = c("A","B","C","D","E","F","G","H"),sep = "\n",remove = T,fill = "right") %>%
  pivot_longer(cols = everything()[-1:-2],values_to = "days") %>% filter(!(is.na(days))) %>% select(-name)
  

