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
library(icesVocab)
library(tidyr)
library(xml2)
library(XML)
library(data.table)
library(htmlwidgets)
library(shinyWidgets)
library(shinycssloaders)
library(shinyjs)
library(xml2)
library(XML)
library(shinythemes)

source("utilities_download_data.r")


title_html <- tags$a(
    href = "https://ices-taf.shinyapps.io/DATRAS-data-mining/",
    target = "_blank",
        tags$img(
            src = "https://www.ices.dk/SiteCollectionImages/ICES%20logos/NEGATIVE%20ICES-logo.png",
            style = "margin-top: -10px; padding-right:10px;padding-bottom:10px",
            height = "50px"
        )
)

# df <- read_xml("https://datras.ices.dk/WebServices/DATRASWebService.asmx/getSubmissionStatus_FishTrawlData")
# df_xml <- xmlParse(df)
# df_r <- xmlToDataFrame(df_xml)
# new_df <- df_r %>% select(Survey, Year, Quarter, Country)

tagList(
    useShinyjs(),
#     introjsUI(),    
    tags$head(tags$script(type="text/javascript", src = "code.js")),

navbarPage(

        # tab title
        windowTitle = "DATRAS data mining app",
        id = "tabset",
        fluid = TRUE,
        # navbar title
        title = title_html,
        sidebarLayout(
                sidebarPanel(
                        width = 4, style = "max-height: 90vh; overflow-y: auto;",
                        panel(
                                # For surveys
                                selectizeInput(
                                        inputId = "survey",
                                        label = "Surveys :",
                                        choices = new_df$Survey,
                                        selected = "BITS",
                                        multiple = FALSE,
                                        width = "100%"
                                        #                         options = list(
                                        #                         placeholder = "Select Ecoregion(s)"
                                        # )
                                ),
                                selectizeGroupUI(
                                        id = "my-filters",
                                        params = list(                                                
                                                Year = list(inputId = "Year", title = "Years:"),
                                                Quarter = list(inputId = "Quarter", title = "Quarters:"),
                                                Country = list(inputId = "Country", title = "Countries:")
                                        ),
                                        inline = FALSE
                                ),
                                heading = "Data filtering",
                                status = "primary",
                                tags$head(tags$script(src = "message-handler.js")),
                                actionButton(inputId = "filter_data", label = "Filter", width = "100%")
                        )
                ),




                # # selectInput("survey", "Survey", icesDatras::getSurveyList()),

                # # For years
                # uiOutput("years"),

                # # For quarters
                # uiOutput("quarters"),

                # # for debugging
                # verbatimTextOutput("debug_text")
                # ),

                mainPanel(
                        tabsetPanel(
                                type = "tabs",
                                tabPanel(
                                        "Haul information",
                                        tabsetPanel(
                                                tabPanel(
                                                        "Tab 1",
                                                        fluidRow(
                                                                valueBoxOutput("haulValueBox"),
                                                                valueBoxOutput("validValueBox"),
                                                                valueBoxOutput("statrecValueBox")
                                                        ),
                                                        textOutput("mapDivider1"),
                                                        withSpinner(plotlyOutput("haulPlot", width = "100%", height = "350px")),
                                                        textOutput("mapDivider2"),
                                                        withSpinner(plotlyOutput("statrecPlot", width = "100%", height = "350px"))
                                                ),
                                                tabPanel(
                                                        "Tab 2",
                                                        fluidRow(
                                                                splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("HHoutplot1"), plotlyOutput("HHoutplot2"))
                                                        ),
                                                        fluidRow(
                                                                splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("HHoutplot3"), plotlyOutput("HHoutplot4"))
                                                        )
                                                ),
                                                tabPanel("Tab 3", "This panel is intentionally left blank")
                                        )
                                ),
                                tabPanel(
                                        "HL properties",
                                        tabsetPanel(
                                                tabPanel(
                                                        "Tab 1",
                                                        valueBoxOutput("speciesHLValueBox"),
                                                        tableOutput("HLspeciesrecords")
                                                ),
                                                tabPanel(
                                                        "Tab 2",
                                                        plotOutput("HLoutplot1")
                                                )
                                        )
                                ),
                                tabPanel(
                                        "CA properties",
                                        tabsetPanel(
                                                tabPanel(
                                                        "Tab 1",
                                                        valueBoxOutput("speciesCAValueBox"),
                                                        tableOutput("CAspeciesrecords")
                                                ),
                                                tabPanel(
                                                        "Tab 2",
                                                        plotOutput("CAoutplot1")
                                                ),
                                                tabPanel(
                                                        "Tab 3",
                                                        plotOutput("CAoutplot2")
                                                )
                                        )
                                )
                        )
                )
        ),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
        theme = shinytheme("cerulean"),  ##### need to work on this, the orange is part of the css theme united, check bslib in forked repo
        position = "fixed-top"
)
)