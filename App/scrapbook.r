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

survey_list <- icesDatras::getSurveyList()
survey_Year_list <- icesDatras::getSurveyYearList("BITS")
survey_Year_quarter_list <- icesDatras::getSurveyYearQuarterList("BITS", 2021)

df <- icesDatras::getSubmissionStatus_FishTrawlData()

df <- getDATRAS(record = "HL", "BITS", 2021, 4)
tibble(df)


library(xml2)
library(XML)
df <- read_xml("https://datras.ices.dk/WebServices/DATRASWebService.asmx/getSubmissionStatus_FishTrawlData")
df_xml <- xmlParse(df)
df_r <- xmlToDataFrame(df_xml)
df_filter <- df_r %>% select(Survey, Year, Quarter, Country)
library(data.table)
fwrite(df_filter, file = "df_filter.csv")
new_df <- fread(file = "df_filter.csv", sep = ",")

df_HH <- getDATRAS(record = "HH", "BITS", c(2018,2019,2020,2021,2022), c(1,4))
df_HL <- getDATRAS(record = "HL", "BITS", c(2018,2019,2020,2021,2022), c(1,4))
df_CA <- getDATRAS(record = "CA", "BITS", c(2018,2019,2020,2021,2022), c(1,4))
fwrite(df_HH, file = "df_HH.csv")
fwrite(df_HL, file = "df_HL.csv")
fwrite(df_CA, file = "df_CA.csv")


library(shiny)
library(ggplot2)
# Define UI for miles per gallon app ----
new_df_HH <- fread(file = "App/df_HH.csv", sep = ",")
df <- new_df_HH %>% select(c(HaulNo, Country, Distance, HaulDur))
df_long <- df %>% pivot_longer(cols = starts_with(df[,3]),
               names_to = 'Variable',
               values_to = 'Value',
               values_drop_na = TRUE)

ui <- fluidPage(

  # Application title
  titlePanel("Group fairness analysis"),

  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "var_to_plot", label ="HH variables:", 
                  choices = c(names(new_df_HH)), multiple = TRUE, selected = "Distance")
      ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic----
server <- function(input, output) {
HH<- reactive({
                          fread(file = "App/df_HH.csv", sep = ",")
                        #   icesDatras::getHLdata(survey(),years(), quarters())
                          })
  output$distPlot <- renderPlot({   
      print(input$var_to_plot)
      df <- HH() %>% select(c(HaulNo,Country, input$group))
      print(df)
      df %>%
#   select(-Country) %>%
        tidyr::gather(variable, value, -c(HaulNo, Country)) %>%
        ggplot(aes(x = HaulNo, y = value,  color = Country)) +
        geom_point(size = 0.5) +
        facet_wrap(~ variable, scales = "free_y")

    #   ggplotly(ggplot(HH(), aes(HaulNo,HH()$input$group, color = Country)) + geom_point()) 
    #   facet_wrap(~get(input$group))
#   gg <- ggplot(df, aes(x=HaulNo))+
#       geom_histogram(breaks=seq(0,100,10))+
#       facet_wrap(~get(input$group))
#       gg

  })

}

shinyApp(ui, server)


df %>%
#   select(-Country) %>%
  tidyr::gather(variable, value, -c(HaulNo,Country)) %>%
  ggplot(aes(x = HaulNo, y = value,  color = Country)) +
  geom_point(size = 0.5) +
  facet_wrap(~ variable, scales = "free_y")
