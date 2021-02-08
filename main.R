library(tidyverse)
library(magrittr)
library(geosphere)
library(rstudioapi)

source("R/utils.R")
#read the us zip data
zip_data <- read_delim("Database/us-zip-code-latitude-and-longitude.csv",delim = ";") %>%
  select("Zip","State","Latitude","Longitude") %>%
  mutate(longlat = map2(Longitude,Latitude,c),
         Latitude = NULL,
         Longitude = NULL) %>%
  rename(zip = Zip)

# select the most recent volunteer form data
file_path <- selectFile()
vt_base <- read_csv(file_path) %>% 
  mutate(ID = paste0("vt",row_number())) %>%
  set_colnames(c("roles","certified","lang","more.lang",
                      "hours","days","community","distance",
                      "vaccine","exp","cv","name","email",
                      "phone","zip","about","waiver","date","vtID"))


# a long tibble of two columns, volunteer ID and Zip code that lies within their preferred driving distance
vt_close_zip <- vt_base %>% 
  select("vtID","zip","distance") %>% 
  mutate(zip = as.character(zip)) %>% 
  left_join(zip_data,by = "zip",keep = T) %>%
  select("vtID","longlat","distance","State")%>%
  mutate(state = State,vtLonglat = longlat, longlat = NULL, State = NULL) %>% 
  pmap(get_closest_zips,zip_data)%>% 
  map_df(~as.data.frame(.x)) %>% 
  pivot_longer(cols = vt_base$vtID,values_to = "zip",names_to = "vtID") %>% 
  filter(!is.na(zip))

# a long tibble of two columns, volunteer ID and their preferred role
vt_roles <- vt_base %>% 
  select("vtID","roles") %>% 
  pmap_dfc(~set_names(str_split(..2,"\n"),..1)) %>% 
  pivot_longer(cols =everything(), names_to = "vtID",values_to = "role")

# a long tibble of three columns, volunteer ID, the duration they wish to volunteer and their preferred days
vt_day_time <- vt_base %>%
  select("vtID","hours","days") %>% 
  separate(days,into = c("A","B","C","D","E","F","G","H"),sep = "\n",remove = T,fill = "right") %>%
  pivot_longer(cols = everything()[-1:-2],values_to = "days") %>% filter(!(is.na(days))) %>% select(-name)
  
# combine all VT data into one DF for use with join functions on VC data
vt_data <- vt_close_zip %>% 
  inner_join(vt_roles,by = "vtID") %>%
  inner_join(vt_day_time,by="vtID")

# read vaccination data
fpath = "test/Test_form_vaccination centers.csv" # replace with recent file
vc_base <- read_csv(fpath,col_types ="cccccccccccccccccccccccc--") %>% 
  set_colnames(c("roles","other_roles","edu","interp_lang","other_interp_lang",
                 "reimburse","ppe","num_events","dt1","dt2","dt3","dt4","dt5",
                 "dt6","dt7","dt8","dt9","dt10","deets","reqs","name","email",
                 "phone","addr")) %>%
  mutate(ID = paste0("vc",row_number()),
         roles = paste(roles,other_roles,sep = "\n"),
         other_roles = NULL,
         interp_lang = paste(interp_lang,other_interp_lang,"\n"),
         other_interp_lang = NULL,
         num_events = NULL)
# get all VC data to combine with VT data
vc_data <- vc_base %>%
  mutate(reqs = NULL,deets =NULL,name = NULL, email = NULL,phone = NULL,
         interp_lang = NULL,edu = NULL, reimburse = NULL, ppe = NULL)%>%
  separate(addr,sep = ",",into = c("addr1","addr2")) %>% 
  mutate(addr1 = NULL, zip = str_extract(addr2,"\\d{5}"),addr2 = NULL)%>%
  select(where(function(x){any(!is.na(x))}))%>% 
  mutate(across(starts_with("dt"),str_extract,"\\d{1}/\\d{2}/\\d{4}")) %>% 
  mutate(across(starts_with("dt"),as.Date,"%m/%d/%Y")) %>% 
  mutate(across(starts_with("dt"),wday,TRUE,FALSE))%>% 
  pivot_longer(starts_with("dt"),values_to = "days") %>% 
  mutate(roles = str_split(roles,"\n")) %>% 
  unnest(roles) %>% 
  drop_na() %>%
  filter(roles != "Other") %>% 
  mutate(name = NULL)

# Combine VC abd VT data to get specific volunteer lists
vc_vt_matches <- vc_data %>% 
  inner_join(vt_data,by = c("zip","days","roles"="role")) %>%
  inner_join(vt_base%>%select(name,email,phone,vtID),by = "vtID") %>% 
  select(!zip & !vtID) %>%
  inner_join(vc_base %>% select(name,email,phone,addr,ID),by = "ID") %>%
  rename(volunteer.email = email.x,
         volunteer.name = name.x,
         volunteer.phone = phone.x)
