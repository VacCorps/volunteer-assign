#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(DT)
library(waiter)
library(shinydashboard)
library(tidyverse)
library(magrittr)
library(lubridate)
library(geosphere)
library(shinybusy)
library(rstudioapi)

source("../R/utils.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
    
    w <- Waiter$new(id = "tble_vt_data",html = spin_1())
    zip_data <- loadZipData("../Database/us-zip-code-latitude-and-longitude.csv")
    vt_base <- loadVolunteerData("../Database/Volunteer Data.csv")
    vt_data <- parseVolunteerData(zip_data,vt_base)
    output$tble_vt_data <- renderDT(datatable(vt_data))
    
})
