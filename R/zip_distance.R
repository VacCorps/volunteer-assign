# Get distance between a set of latitutdes longtidude pairs


get_closest_zips <- function(vtID,distance, state,vtLonglat,zip_data){
  print(vtLonglat)
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
    select(zip) %>% c()
    
  return(closest_zips)
}