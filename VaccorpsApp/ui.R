#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinybusy)
library(waiter)
library(DT)
# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage("VacCorps Volunteer Dashboard",inverse = T,id="pagenav",
                         
                   tabPanel("Volunteer Data",value = "data",
                            add_busy_spinner(position = "full-page"),
                            fluidPage(
                                sidebarLayout(
                                    sidebarPanel = fileInput("btn_load_vt","Load Volunteer Data",accept = c(".csv")),
                                    mainPanel = DT::DTOutput("tble_vt_data")
                                )
                            ))
                   )
        )
