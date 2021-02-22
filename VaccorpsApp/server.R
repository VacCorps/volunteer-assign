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
    
    zip_dist <- loadZipDistData("../Database/zip_dist.csv")
    vt_base <- loadVolunteerData("../Database/Volunteer Data.csv")
    vt_data <- parseVolunteerData(zip_dist,vt_base) %>% group_by("vtID")
    observe({
        zip_code <- input$txt_zip
        vt_output <- vt_data %>% 
            filter(zip == zip_code) %>% 
            select(vtID) %>%
            distinct(vtID) %>%
            inner_join(vt_base, by = "vtID") %>%
            ungroup %>%
            select("name","email","phone","lang","roles","days","hours")
        output$tble_vt_data <- renderDT(datatable(vt_output,rownames = F,
                                                  colnames = c("Name","E-Mail","Phone","Languages","Roles","Days","Hours"),
                                                  extensions = 'Buttons', 
                                                  options = list(
                                                      dom = 'frtipB',
                                                      buttons = c('copy', 'csv', 'excel')),
                                                  filter=list(position = "top",clear = T, plain = F)),server = F)
        
    })
    
    
})
