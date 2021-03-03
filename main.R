library(tidyverse)
library(magrittr)
library(lubridate)
library(geosphere)
library(rstudioapi)

source("R/utils.R")
#read the us zip data
zip_data <- loadZipDistData()

# select the most recent volunteer form data
file_path <- "Database/Volunteer Data.csv"
vt_base <- loadVolunteerData(file_path)
vt_data <- parseVolunteerData(zip_data,vt_base)

# read vaccination data
fpath = "test/Test_form_vaccination centers.csv" # replace with recent file
vc_base <- loadVaccinationCenterData(fpath)

# parse VC data in a format that can be combined with VT data
vc_data <- vc_base %>%
  mutate(reqs = NULL,deets =NULL,name = NULL, email = NULL,phone = NULL,
         interp_lang = NULL,edu = NULL, reimburse = NULL, ppe = NULL)%>%
  separate(addr,sep = ",",into = c("addr1","addr2")) %>% 
  mutate(addr1 = NULL, zip = str_extract(addr2,"\\d{5}"),addr2 = NULL)%>%
  select(where(function(x){any(!is.na(x))}))%>% 
  mutate(across(starts_with("dt"),str_extract,"\\d{1}/\\d{2}/\\d{4}")) %>% 
  mutate(across(starts_with("dt"),as.Date,"%m/%d/%Y")) %>% 
  pivot_longer(starts_with("dt"),values_to = "dates") %>% 
  mutate(days = wday(dates,TRUE,FALSE)) %>%  
  mutate(roles = str_split(roles,"\n")) %>% 
  unnest(roles) %>% 
  drop_na() %>%
  filter(roles != "Other") %>% 
  mutate(name = NULL)

# Combine VC abd VT data to get specific volunteer lists
vc_vt_matches <- vc_data %>% 
  full_join(vt_data,by = c("zip","days","roles"="role")) %>%
  inner_join(vt_base%>%select(name,email,phone,vtID),by = "vtID") %>% 
  select(!zip & !vtID) %>%
  inner_join(vc_base %>% select(name,email,phone,addr,ID),by = "ID") %>%
  rename(volunteer.email = email.x,
         volunteer.name = name.x,
         volunteer.phone = phone.x)
