#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinybusy)
library(DT)
# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage("VacCorps Volunteer Dashboard",inverse = T,id="pagenav",
                         navbarMenu("File",menuName = "file",
                                    tabPanel("Upload Volunteer Data",value = "vtdataup",
                                             fileInput("file_vtdata",""))),
                         
                   tabPanel("Vaccination Event Details",value = "evt_dets",selected = T,
                            add_busy_spinner("dots",position = "full-page"),
                            fluidPage(
                              fluidRow(
                                column(3, 
                                       textInput("txt_zip","5 Digit Zip for the Vaccination Center",
                                                 value = "27703",width = validateCssUnit("100%"))
                                       ),
                                column(6, selectizeInput("sel_roles","Select volunteer roles",
                                                         choices = c("Welcome/greeter",
                                                                    "Medical Interpreter",
                                                                    "Check in/check out",
                                                                    "Office and cleaning supplies receiving/stocking",
                                                                    "Vaccine supplies receiving/stocking",
                                                                    "Vaccine preparation",
                                                                    "Cleaning/wiping surfaces",
                                                                    "Remote role - Appointment scheduling",
                                                                    "Remote role - Volunteer scheduling"),
                                                         multiple = T,
                                                         width = validateCssUnit("100%")
                                                         )
                                       ),
                                column(3, 
                                       selectInput("sel_numevnts","Select Number of Vaccination events",
                                                   choices = 1:12,multiple = F)
                                       )
                                ),
                              fluidRow(
                                       uiOutput("ui_dt")
                                       
                              ),
                              fluidRow(
                                column(4, offset = 1,
                                       actionButton("btn_getdata","Get Volunteer List"))
                              ),
                              fluidRow(
                                column( 12,  
                                        DT::DTOutput("tble_vt_data")
                                )
                              ))),
                   tabPanel(title = NULL, icon = icon("power-off"),value = "off")
                   )
        )
