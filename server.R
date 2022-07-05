#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

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

# Define server logic required to draw a histogram
server <- function(input, output) {
        
        # useful for debugging :)
        output$debug_text <- renderText({
                txt <-
                        lapply(setdiff(names(input), "table"),
                               function(x) c(paste0(x, ":"), capture.output(str(input[[x]])), ""))
                
                txt <- unlist(txt)
                
                txt <- c("Debug window:", "-------------\n", txt)
                
                paste(txt, collapse = "\n")
        })
        
        # get values or default values
        survey <- reactive({
                old_surv <- input$survey
                if (is.null(old_surv)) old_surv <- icesDatras::getSurveyList()[1]
                old_surv
        })
        
        years <- reactive({
                old_year <- input$years
                if (is.null(old_year)) old_year <- icesDatras::getSurveyYearList(survey())[1]
                old_year
        })
        
        quarters <- reactive({
                old_qrtr <- input$quarters
                if (is.null(old_old_qrtr)) old_old_qrtr <- icesDatras::getSurveyYearQuarterList(survey(), years())[1]
                old_qrtr
        })
        
        output$years <- renderUI({
                selectInput("years", "Years", icesDatras::getSurveyYearList(survey()), multiple = TRUE)
        })
        
        output$quarters <- renderUI({
                selectInput("quarters", "Quarters", icesDatras::getSurveyYearQuarterList(survey(), years()), multiple = TRUE)
        })
        
        
        years <- reactive({as.integer(input$years)})
        quarters <- reactive({as.integer(input$quarters)})
        

        HH <- reactive({getDATRAS(record = "HH", survey(),years(), quarters())})

        # output$haulPlot <- renderLeaflet({
        #         
        #   HH()%>% mutate(longitude = HaulLong, latitude = HaulLat) %>%
        #           dplyr::group_by(HaulNo, latitude,longitude) %>%
        #           count() %>%
        #           leaflet() %>%
        #           addTiles() %>%
        #           addCircles()
        #   })
        
        output$haulPlot <- renderPlotly({
                d <- HH() %>% filter(HaulVal == "V") 
        europe_shape <- 
                rnaturalearth::ne_countries(
                        scale = 10, type = "countries", 
                        continent = "europe", 
                        returnclass = "sf")
        europe_shape <- europe_shape[, c("iso_a3", "iso_n3", "admin", "geometry")]
        
        # get plot extent
        
        xlims <- c(min(d$ShootLong), 
                   max(d$ShootLong))
        ylims <- c(min(d$ShootLat), 
                   max(d$ShootLat))
        
        # make plot
        ggplotly(ggplot2::ggplot() +
                ggplot2::theme_bw(base_size = 10) +
                ggplot2::geom_sf(data = europe_shape, fill = "grey80", color = "grey90") +
                ggplot2::coord_sf(xlim = xlims, ylim = ylims) +
                ggplot2::geom_point(ggplot2::aes(x = d$ShootLong, y = d$ShootLat, colour = factor(d$Country)))+
                ggplot2::theme(plot.caption = ggplot2::element_text(size = 9),
                               plot.subtitle = ggplot2::element_text(size = 9),
                               axis.title.x = ggplot2::element_blank(),
                               axis.title.y = ggplot2::element_blank(),
                               legend.title = ggplot2::element_blank()))
        })
        
        
        output$statrecPlot <- renderPlotly({
                
                crs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

                # get europe coastline polygon
                europe_shape <-
                        rnaturalearth::ne_countries(
                                scale = 10, type = "countries",
                                continent = "europe",
                                returnclass = "sf")
                europe_shape <- europe_shape[, c("iso_a3", "iso_n3", "admin", "geometry")]

                get_map <- function(URL) {
                        tmp_file <- tempfile(fileext = ".zip")
                        download.file(url = URL,
                                      destfile = tmp_file,
                                      mode = "wb", quiet = TRUE)
                        unzip(tmp_file, exdir = tmp_path)
                }

                tmp_path <- tempdir()
                get_map("http://gis.ices.dk/shapefiles/ICES_rectangles.zip")
                stat_rec <- sf::st_read(dsn = tmp_path, quiet = FALSE)
                stat_rec <- sf::st_transform(stat_rec, crs = crs)
                a <- HH() %>% filter(HaulVal == "V")
                d <- a %>% group_by(StatRec) %>% count()
                stat_rec2 <- stat_rec %>% filter(ICESNAME %in% d$StatRec)
                
               
                xlims <- c(min(a$ShootLong), 
                           max(a$ShootLong))
                ylims <- c(min(a$ShootLat), 
                           max(a$ShootLat))
        
                # make plot
                ggplotly(ggplot2::ggplot() +
                        ggplot2::theme_bw(base_size = 10) +
                        ggplot2::geom_sf(data = europe_shape, fill = "grey80", color = "grey90") +
                        ggplot2::geom_sf(data = stat_rec2, aes(fill = d$n)) +
                        ggplot2::scale_fill_viridis_c(alpha = 0.2)+
                        ggplot2::coord_sf(xlim = xlims, ylim = ylims) +
                        ggplot2::theme(plot.caption = ggplot2::element_text(size = 6),
                                       plot.subtitle = ggplot2::element_text(size = 7),
                                       axis.title.x = ggplot2::element_blank(),
                                       axis.title.y = ggplot2::element_blank()))
                })
        
        output$mapDivider1 = renderText({"Location of hauls coloured by country"})
        output$mapDivider2 = renderText({"Coverage of hauls by statistical rectangle"})
        
        output$haulValueBox <- renderValueBox({
          num_hauls <- nrow(HH())
          num_hauls <- as.numeric(num_hauls)
          valueBox(num_hauls,
                   "Total number of hauls", icon = icon("ship"),
                   color = 'aqua')
  })
        output$validValueBox <- renderValueBox({
                num_hauls <- length(HH()$HaulVal == "V")
                num_hauls <- as.numeric(num_hauls)
                valueBox(num_hauls,
                         "valid hauls",
                         icon = icon("check"))
        })
        output$statrecValueBox <- renderValueBox({
                num_hauls <- length(unique(HH()$StatRec))
                num_hauls <- as.numeric(num_hauls)
                valueBox(num_hauls,
                         "Statistical rectangles sampled",
                         icon = icon("map"))
        })

          output$HHoutplot1 <- renderPlotly({
                  ggplotly(ggplot(HH(), aes(HaulNo, Distance, color = Country)) + geom_point())
          })
          output$HHoutplot2 <- renderPlotly({
                  ggplotly(ggplot(HH(), aes(HaulNo, HaulDur, color = Country)) + geom_point())
          })
          output$HHoutplot3 <- renderPlotly({
                  ggplotly(ggplot(HH(), aes(HaulNo, DoorSpread, color = Country)) + geom_point())
          })
          output$HHoutplot4 <- renderPlotly({
                  ggplotly(ggplot(HH(), aes(HaulNo, WingSpread, color = Country)) + geom_point())
          })
          
          HL <- reactive({icesDatras::getHLdata(survey(),years(), quarters())})
          
          output$speciesHLValueBox <- renderValueBox({
                  num_specs <- length(unique(HL()$SpecCode))
                  num_specs <- as.numeric(num_specs)
                  valueBox(num_specs,
                           "species recorded",
                           icon = icon("fish"))
          })
          output$HLspeciesrecords <- renderTable({
                  # AphiaID <- read.csv("AphiaIDs.csv")
                  sps <- AphiaID[,4:5]
                  dat <- HL() 
                  dat <- dat %>% filter(HLNoAtLngt >0)
                  dat <- dat %>% filter(!is.na(SpecCode))
                  dat <- dat %>% group_by(SpecCode) %>% mutate(HL_HaulNo = length(unique(HaulNo))) %>%
                          select(c("SpecCode", "HL_HaulNo"))
                  dat <- unique(dat)
                  colnames(dat) <- c("WoRMS_AphiaID", "HL_HaulNo")
                  dat <- left_join(dat,sps)
                  dat <- dat %>% select(c("ScientificName_WoRMS", "HL_HaulNo"))
                  colnames(dat) <- c("Species", "Number of hauls")
                  dat <- unique(dat)
                  dat
          })
          output$HLoutplot1 <- renderPlot({
                  dat <- HL()
                  dat <-rbind(dat %>%filter(LngtCode%in%c(".", "0"))%>%mutate(LngtClass=LngtClass/10),
                             dat %>%filter(!LngtCode%in%c(".", "0")))
                  dat$HLNoAtLngt <- dat$HLNoAtLngt*dat$SubFactor
                  dat <- dat %>% filter(HLNoAtLngt > 0)
                  dat <- dat %>% filter(!is.na(SpecCode))
                  dat <- dat %>% group_by(SpecCode, LngtClass) %>% mutate(HLNoAtLngtTot = sum(HLNoAtLngt))
                  dat <- dat %>% select(c(SpecCode, LngtClass, HLNoAtLngtTot))
                  dat <- unique(dat)
                  colnames(dat) <- c("WoRMS_AphiaID", "LngtClass", "HLNoAtLngtTot")
                  dat <- left_join(dat,sps)
                  dat <- dat %>% group_by(ScientificName_WoRMS) %>% mutate(freq = n()) %>% ungroup() %>% filter(freq > 1) %>%select(-freq)
                  ggplot(dat, aes(LngtClass, HLNoAtLngtTot)) + geom_area()+facet_wrap(~ScientificName_WoRMS, scales = "free")
          }, height = 550, width = 750 )

  
        CA <- reactive({icesDatras::getCAdata(survey(),years(), quarters())})

        output$speciesCAValueBox <- renderValueBox({
                num_specs <- length(unique(CA()$SpecCode))
                num_specs <- as.numeric(num_specs)
                valueBox(num_specs,
                        "species recorded",
                        icon = icon("fish"))
                })
        output$CAspeciesrecords <- renderTable({
                # AphiaID <- read.csv("AphiaIDs.csv")
                sps <- AphiaID[,4:5]
                dat <- CA() 
                dat <- dat %>% filter(NoAtALK >0)
                dat <- dat %>% filter(!is.na(SpecCode))
                dat <- dat %>% group_by(SpecCode) %>% mutate(CA_HaulNo = length(unique(HaulNo))) %>%
                        select(c("SpecCode", "CA_HaulNo"))
                dat <- unique(dat)
                colnames(dat) <- c("WoRMS_AphiaID", "CA_HaulNo")
                dat <- left_join(dat,sps)
                dat <- dat %>% select(c("ScientificName_WoRMS", "CA_HaulNo"))
                colnames(dat) <- c("Species", "Number of hauls")
                dat <- unique(dat)
                dat
        })
        
        #some issue with scales = free for plotly
        output$CAoutplot1 <- renderPlot({
                dat <- left_join(CA(),sps, by= c("SpecCode"="WoRMS_AphiaID"))
                dat <- dat %>% filter(IndWgt >0)
                dat <-rbind(dat %>%filter(LngtCode%in%c(".", "0"))%>%mutate(LngtClass=LngtClass/10),
                           dat %>%filter(!LngtCode%in%c(".", "0")))
                ggplot(dat, aes(LngtClass, IndWgt, color = Country)) + geom_point()+ facet_wrap("ScientificName_WoRMS", scales = "free")
        })
        output$CAoutplot2 <- renderPlot({
                dat <- left_join(CA(),sps, by= c("SpecCode"="WoRMS_AphiaID"))
                dat <- dat %>% filter(Age >0)
                dat <-rbind(dat %>%filter(LngtCode%in%c(".", "0"))%>%mutate(LngtClass=LngtClass/10),
                            dat %>%filter(!LngtCode%in%c(".", "0")))
               ggplot(dat, aes(Age, LngtClass, color = Country)) + geom_point()+ facet_wrap("ScientificName_WoRMS", scales = "free")
        })
}
