# Get distance between a set of latitutdes longtidude pairs
getZipDistance <- function(zip1,longlat1,longlat2){
  distance <- distGeo(longlat1,longlat2)/1609.344
  return(zip1,distance)
}

get_closest_zips <- function(vtID,distance, state,vtLonglat,zip_data){
  distance  <- ifelse(distance == 0,10,distance)
  closest_zips <- zip_data %>% 
    filter(State == state) %>% 
    select("zip","longlat") %>%
    pmap(function(zip,longlat,y){return(list(zip,
                                             distGeo(longlat,y)/1609.344)
                                        )},vtLonglat) %>%
    as_tibble_col("result") %>%
    unnest_wider("result",names_sep = "_") %>%
    set_names(c("zip","dist")) %>%
    filter(dist <= distance) %>%
    arrange(dist) %>% 
    select(zip) %>% set_names(vtID) %>% c()
    
  return(closest_zips)
}


loadZipDistData <- function(fpath=NULL){
  if(is.null(fpath)){
    fpath <- "Database/zip_dist.csv"
  }
  zip_dist <- read_csv(fpath,col_names = T, col_types = "ccd")
}

# get zip data from the database
loadZipData <- function(fpath = NULL){
  if(is.null(fpath)){
    fpath <- "Database/us-zip-code-latitude-and-longitude.csv"
  }
  #read the us zip data
  zip_data <- read_delim(fpath,delim = ";") %>%
    select("Zip","State","Latitude","Longitude") %>%
    mutate(longlat = map2(Longitude,Latitude,c),
           Latitude = NULL,
           Longitude = NULL) %>%
    rename(zip = Zip)
  return(zip_data)
}

loadVolunteerData <- function(fpath){
  vt_base <- read_csv(fpath,skip = 1,
                      col_names = c("roles","certified","lang","more.lang",
                                    "hours","days","community","distance",
                                    "vaccine","exp","name","email",
                                    "phone","zip","age_consent", "about",
                                    "waiver","entryid","uid")) %>%
    drop_na(uid) %>%
    mutate(vtID = paste0("vt",row_number()))
  return(vt_base)
}

parseVolunteerData <- function(zip_dist,vt_base){
  # a long tibble of two columns, volunteer ID and Zip code that lies within their preferred driving distance
  vt_close_zip <- vt_base %>% 
    select("vtID","zip","distance") %>% 
    mutate(zip = as.character(zip)) %>% 
    inner_join(zip_dist,by = c("zip"="zip1")) %>% 
    filter(distance.y < distance.x) %>% 
    select("vtID","zip2") %>% 
    rename("zip"="zip2")
    
  #   
  # vt_close_zip <- vt_base %>% 
  #   select("vtID","zip","distance") %>% 
  #   mutate(zip = as.character(zip)) %>% 
  #   left_join(zip_data,by = "zip",keep = T) %>%
  #   select("vtID","longlat","distance","State")%>%
  #   drop_na() %>%
  #   mutate(state = State,vtLonglat = longlat, longlat = NULL, State = NULL) %>% 
  #   pmap(get_closest_zips,zip_data)%>% 
  #   map_df(~as.data.frame(.x)) %>% 
  #   pivot_longer(cols = everything(),values_to = "zip",names_to = "vtID") %>% 
  #   filter(!is.na(zip))
  # a long tibble of two columns, volunteer ID and their preferred role
  vt_roles <- vt_base %>% 
    select("vtID","roles") %>% 
    pmap_dfr(~set_names(str_split(..2,"\n"),..1)) %>% 
    pivot_longer(cols = starts_with("vt"),names_to = "vtID",values_to= "role") %>% 
    drop_na(role)
  # a long tibble of three columns, volunteer ID, the duration they wish to volunteer and their preferred days
  vt_day_time <- vt_base %>%
    select("vtID","hours","days") %>% 
    separate(days,into = c("A","B","C","D","E","F","G","H"),sep = "\n",remove = T,fill = "right") %>%
    pivot_longer(cols = everything()[-1:-2],values_to = "days") %>% 
    filter(!(is.na(days))) %>% 
    select(-name)
  # combine all VT data into one DF for use with join functions on VC data
  vt_data <- vt_close_zip %>% 
    inner_join(vt_roles,by = "vtID") %>%
    inner_join(vt_day_time,by="vtID")
  return(vt_data)
}

loadVaccinationCenterData <- function(fpath){
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
  return(vc_base)
}

# createa a database of zip distances in NC
createZipDistDb <- function(zip_data){
  short_zip <- zip_data %>% filter(State == "NC") %>% select(zip,longlat)
  zip_dist <- pmap(short_zip,function(zip=.x,longlat=.y,short_zip){
      dist_mat_long <- short_zip %>%
        pmap(function(zip=.x,longlat=.y,zip2,longlat2){
          distance <- distGeo(longlat,longlat2)/1609.344
          return(list("zip1"= zip,"zip2" = zip2,"distance" = distance))
          },zip,longlat)
      return(dist_mat_long)
    },short_zip) %>%
    flatten %>%
    as_tibble_col %>%
    unnest_wider(col = "value",names_repair = "minimal") %>% 
    filter(distance > 0)
}