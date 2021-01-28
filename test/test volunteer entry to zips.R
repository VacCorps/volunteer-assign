
# VACCORPS DATA MANAGEMENT

rm(list=ls())
require(ggplot2)
require(zipcodeR)
require(geosphere)
require(tidyverse)
library(stringr)

setwd('C:/Users/Cathrine Leonowens/Desktop/VacCorps/DATA/TEST')

dat <- read.csv('mock_form.csv',colClasses = 'character')

zips <- read.csv('C:/Users/Cathrine Leonowens/Desktop/VacCorps/DATA/us-zip-code-latitude-and-longitude.csv', sep=';')# colClasses = "character")
zips$ZIP <- str_pad(zips$Zip, 5, pad = "0") # add back zeros to zip codes that start with zero

NCzip <- zips[zips$State=="NC",c("ZIP","City","State","Latitude","Longitude")]

# merge latitude and longitude to volunteer data
dat <- left_join(dat,NCzip,by='ZIP')

# calculate radius in degrees using miles entered
# this is where I need to use one of the methods in geosphere because it
# depends on the direction from your starting point.



get_geo_distance = function(long1, lat1, long2, lat2, units = "miles") {
  longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
  longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
  distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
  distance_m = list_extract(distance_list, position = 1)
  if (units == "km") {
    distance = distance_m / 1000.0;
  }
  else if (units == "miles") {
    distance = distance_m / 1609.344
  }
  else {
    distance = distance_m
    # This will return in meter as same way as distHaversine function. 
  }
  return(distance)
}