#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(tidyverse)
library(magrittr)
library(geosphere)
library(shinybusy)
library(shinyTime)
library(lubridate)

source("../R/utils.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
    
    observeEvent(input$sel_numevnts,{
        nevnts <- input$sel_numevnts
        col_width <- 1
        output$ui_dt <- renderUI({
            ui_list <- imap(1:nevnts, ~{
                                 tagList(
                                     column(col_width,
                                            dateInput(paste0("date_",.y),"Select Date",width = validateCssUnit("100%")),
                                            timeInput(paste0("timest_",.y),"Start Time", minute.steps = 30),
                                            timeInput(paste0("timeend_",.y),"End Time", minute.steps = 30)
                                            )
                                 )
                             })
            tagList(ui_list)
        })
    })
    
    
    
    zip_dist <- loadZipDistData("../Database/zip_dist.csv")
    vt_file <- reactive({
        vt_file <- input$file_vtdata
    })
    vt_list <- eventReactive(input$btn_getdata,{
        validate(need(input$file_vtdata,"Volunteer Data Not Uploaded"))
        fpath <- input$file_vtdata$datapath
        vt_base <- loadVolunteerData(fpath)
        vt_data  <- parseVolunteerData(zip_dist,vt_base)
        # get vaccination event details
        zip_str <- input$txt_zip
        roles_str <- paste(input$sel_roles,collapse = "|")
        dates_ids <- names(input)[grepl("^(date)",names(input))]
        days_vector <- c()
        for (each_name in dates_ids){
            day <- as.character(wday(input[[each_name]],label = T, abbr = F))
            days_vector <- c(days_vector,day)
        }
        days_str <- paste(days_vector,collapse = "|")
        vt_list <- vt_data %>%
            filter(zip == zip_str) %>%
            filter(str_detect(role,roles_str))%>%
            filter(str_detect(days,days_str)) %>%
            select(vtID) %>% 
            distinct() %>% 
            inner_join(vt_base, by="vtID") %>% 
            select(name,email,phone,roles,lang,certified,vaccine,exp)
        return(vt_list)
            
        
        
    })
    output$tble_vt_data <- renderDT(datatable(vt_list(),rownames = F,
                                              colnames = c("Name","E-Mail","Phone","Roles",
                                                           "Languages","Certified","Vaccine","Experience"),
                                                  extensions = 'Buttons', 
                                                  options = list(
                                                      dom = 'frtipB',
                                                      buttons = c('copy', 'csv', 'excel')),
                                                  filter=list(position = "top",clear = T, plain = F)),server = F)
    
    observeEvent(input$pagenav,{
        if(input$pagenav == "off"){
            stopApp()
        }
    })
   
    
    
})
