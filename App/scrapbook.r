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
