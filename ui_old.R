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
library(plotly)

library(shiny)
library(flexdashboard)
library(readr)
library(leaflet)
library(DT)
library(shinydashboard)
library(htmlwidgets)
library(ggplot2)
library(lubridate)
library(icesDatras)
library(dplyr)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(
        theme = shinythemes::shinytheme("superhero"),
        titlePanel("DATRAS data mining app"),
        
        sidebarLayout(
                sidebarPanel(
                        #For surveys
                        selectInput("survey", "Survey", icesDatras::getSurveyList()),
                        
                        # For years
                        uiOutput("years"),
                        
                        # For quarters
                        uiOutput("quarters"),
                        
                        # for debugging
                        verbatimTextOutput("debug_text")
                        ),
                
                mainPanel(
                        
                        tabsetPanel(type = "tabs",
                                    tabPanel("Haul information",
                                             tabsetPanel(
                                                     tabPanel("Tab 1",
                                                              fluidRow(valueBoxOutput("haulValueBox"),
                                                                       valueBoxOutput("validValueBox"),
                                                                       valueBoxOutput("statrecValueBox")
                                                              ),
                                                              
                                                              textOutput("mapDivider1"),
                                                              plotlyOutput("haulPlot",width = "100%", height = "350px"),
                                                              textOutput("mapDivider2"),
                                                              plotlyOutput("statrecPlot", width = "100%", height = "350px")
                                                     ),
                                                     tabPanel("Tab 2", 
                                                              fluidRow(
                                                                      splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("HHoutplot1"), plotlyOutput("HHoutplot2"))
                                                              ),
                                                              fluidRow(
                                                                      splitLayout(cellWidths = c("50%", "50%"),plotlyOutput("HHoutplot3"),plotlyOutput("HHoutplot4"))
                                                     )),
                                                     tabPanel("Tab 3", "This panel is intentionally left blank"))
                                    ),
                                    tabPanel("HL properties",
                                             tabsetPanel(
                                                     tabPanel("Tab 1",
                                                              valueBoxOutput("speciesHLValueBox"),
                                                              tableOutput("HLspeciesrecords")),
                                                     tabPanel("Tab 2",
                                                              plotOutput("HLoutplot1")
                                                              ))
                                    ),
                                    tabPanel("CA properties",
                                             tabsetPanel(
                                            tabPanel("Tab 1",
                                                     valueBoxOutput("speciesCAValueBox"),
                                                     tableOutput("CAspeciesrecords")),
                                            tabPanel("Tab 2",
                                                     plotOutput("CAoutplot1")),
                                            tabPanel("Tab 3",
                                                     plotOutput("CAoutplot2"))
                                            )
                                    )
                        )
                        )
        )
)

                                            