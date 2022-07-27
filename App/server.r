source("utilities_download_data.r")


server <- function(input, output, session) {
        
        
        
        
        
        # useful for debugging :)
        # output$debug_text <- renderText({
        #         txt <-
        #                 lapply(setdiff(names(input), "table"),
        #                        function(x) c(paste0(x, ":"), capture.output(str(input[[x]])), ""))

        #         txt <- unlist(txt)

        #         txt <- c("Debug window:", "-------------\n", txt)

        #         paste(txt, collapse = "\n")
        # })

        # eco_filter <- reactive({
        #         # new_df <- fread(file = "df_filter.csv", sep = ",")
        #         df_survey <- new_df %>% filter(Survey == input$survey)
        #         # df_survey <- new_df %>% filter(Survey == "BITS")
        #         print(df_survey)
        # })

        res_mod <- callModule(
                module = selectizeGroupServer,
                id = "my-filters",
                data =  new_df_HH,
                vars = c(
                        "Survey", "Year", "Quarter", "Country", "Ship", "Gear"
                )
        )

        HH <- eventReactive(req(input$filter_data), {

                
                if (isTRUE(input$valid_hauls)){
                       res_mod <- res_mod() %>% filter(HaulVal == "V")
                } else {
                       res_mod <- res_mod() 
                }
                #     session$sendCustomMessage(type = 'testmessage',
                #       message = 'Thank you for clicking')
                # print(input$survey)
                # print(c(as.numeric(input[["my-filters-Year"]])))
                # print(c(as.numeric(input[["my-filters-Quarter"]])))
                
                # getDATRAS(record = "HH", input$survey, c(as.numeric(input[["my-filters-Year"]])), c(as.numeric(input[["my-filters-Quarter"]])))
                
                # fwrite(df, file = "df.csv")
                # print(tibble(HH))
                # res_mod()
        })





        #         # get values or default values
        #         survey <- reactive({
        #                 old_surv <- input$survey
        #                 if (is.null(old_surv)) old_surv <- icesDatras::getSurveyList()[1]
        #                 old_surv
        #         })

        #         years <- reactive({
        #                 old_year <- input$years
        #                 if (is.null(old_year)) old_year <- icesDatras::getSurveyYearList(survey())[1]
        #                 old_year
        #         })

        #         quarters <- reactive({
        #                 old_qrtr <- input$quarters
        #                 if (is.null(old_old_qrtr)) old_old_qrtr <- icesDatras::getSurveyYearQuarterList(survey(), years())[1]
        #                 old_qrtr
        #         })

        #         output$years <- renderUI({
        #                 selectInput("years", "Years", icesDatras::getSurveyYearList(survey()), multiple = TRUE)
        #         })

        #         output$quarters <- renderUI({
        #                 selectInput("quarters", "Quarters", icesDatras::getSurveyYearQuarterList(survey(), years()), multiple = TRUE)
        #         })


        #         years <- reactive({as.integer(input$years)})
        #         quarters <- reactive({as.integer(input$quarters)})


        # HH <- reactive({
        #         getDATRAS(record = "HH", input$survey, input[["my-filters-Year"]], input[["my-filters-Quarter"]])
        # })

        # # output$haulPlot <- renderLeaflet({
        # #
        # #   HH()%>% mutate(longitude = HaulLong, latitude = HaulLat) %>%
        # #           dplyr::group_by(HaulNo, latitude,longitude) %>%
        # #           count() %>%
        # #           leaflet() %>%
        # #           addTiles() %>%
        # #           addCircles()
        # #   })

        output$haulPlot <- renderPlotly({
                d <- HH() #%>% filter(HaulVal == "V")
                # europe_shape <-
                #         rnaturalearth::ne_countries(
                #                 scale = 10, type = "countries",
                #                 continent = "europe",
                #                 returnclass = "sf"
                #         )
                # europe_shape <- europe_shape[, c("iso_a3", "iso_n3", "admin", "geometry")]

                # get plot extent

                xlims <- c(
                        min(d$ShootLong),
                        max(d$ShootLong)
                )
                ylims <- c(
                        min(d$ShootLat),
                        max(d$ShootLat)
                )
                map1(d, europe_shape, xlims, ylims)
                # make plot
                # ggplotly(ggplot2::ggplot() +
                #         ggplot2::theme_bw(base_size = 10) +
                #         ggplot2::geom_sf(data = europe_shape, fill = "grey80", color = "grey90") +
                #         ggplot2::coord_sf(xlim = xlims, ylim = ylims) +
                #         ggplot2::geom_point(ggplot2::aes(x = d$ShootLong, y = d$ShootLat, colour = factor(d$Country))) +
                #         ggplot2::theme(
                #                 plot.caption = ggplot2::element_text(size = 9),
                #                 plot.subtitle = ggplot2::element_text(size = 9),
                #                 axis.title.x = ggplot2::element_blank(),
                #                 axis.title.y = ggplot2::element_blank(),
                #                 legend.title = ggplot2::element_blank()
                #         ))
        })


        output$statrecPlot <- renderPlotly({
                # crs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

                # # get europe coastline polygon
                # europe_shape <-
                #         rnaturalearth::ne_countries(
                #                 scale = 10, type = "countries",
                #                 continent = "europe",
                #                 returnclass = "sf"
                #         )
                # europe_shape <- europe_shape[, c("iso_a3", "iso_n3", "admin", "geometry")]

                # get_map <- function(URL) {
                #         tmp_file <- tempfile(fileext = ".zip")
                #         download.file(
                #                 url = URL,
                #                 destfile = tmp_file,
                #                 mode = "wb", quiet = TRUE
                #         )
                #         unzip(tmp_file, exdir = tmp_path)
                # }

                # tmp_path <- tempdir()
                # get_map("http://gis.ices.dk/shapefiles/ICES_rectangles.zip")
                # stat_rec <- sf::st_read(dsn = tmp_path, quiet = FALSE)
                # stat_rec <- sf::st_transform(stat_rec, crs = crs)
                a <- HH() #%>% filter(HaulVal == "V")
                
                d <- a %>%
                        group_by(StatRec) %>%
                        count()
                stat_rec2 <- stat_rec %>% filter(ICESNAME %in% d$StatRec)


                xlims <- c(
                        min(a$ShootLong),
                        max(a$ShootLong)
                )
                ylims <- c(
                        min(a$ShootLat),
                        max(a$ShootLat)
                )

                map2(d, europe_shape, xlims, ylims, stat_rec2)
                # make plot
                # ggplotly(ggplot2::ggplot() +
                #         ggplot2::theme_bw(base_size = 10) +
                #         ggplot2::geom_sf(data = europe_shape, fill = "grey80", color = "grey90") +
                #         ggplot2::geom_sf(data = stat_rec2, aes(fill = d$n)) +
                #         ggplot2::scale_fill_viridis_c(alpha = 0.2) +
                #         ggplot2::coord_sf(xlim = xlims, ylim = ylims) +
                #         ggplot2::theme(
                #                 plot.caption = ggplot2::element_text(size = 6),
                #                 plot.subtitle = ggplot2::element_text(size = 7),
                #                 axis.title.x = ggplot2::element_blank(),
                #                 axis.title.y = ggplot2::element_blank()
                #         ))
        })

        #         output$mapDivider1 = renderText({"Location of hauls coloured by country"})
        #         output$mapDivider2 = renderText({"Coverage of hauls by statistical rectangle"})

                output$haulValueBox <- renderValueBox({
                  num_hauls <- nrow(HH())
                  num_hauls <- as.numeric(num_hauls)
                  valueBox(num_hauls,
                           "Total number of hauls", 
                           color = 'aqua')
          })
                output$validValueBox <- renderValueBox({
                        num_hauls <- length(HH()$HaulVal == "V")
                        num_hauls <- as.numeric(num_hauls)
                        valueBox(num_hauls,
                                 "valid hauls"
                                 )
                })
                output$statrecValueBox <- renderValueBox({
                        num_hauls <- length(unique(HH()$StatRec))
                        num_hauls <- as.numeric(num_hauls)
                        valueBox(num_hauls,
                                 "Statistical rectangles sampled"
                                 )
                })

                #   output$HHoutplot1 <- renderPlotly({
                #         #   print(input$group)
                #           df <- HH() %>% select(c(HaulNo, Country, input$var_to_plot))
                #         #   print(df)
                #           df_long <- df %>%
                #                   #   select(-Country) %>%
                #                   tidyr::gather(variable, value, -c(HaulNo, Country))
                                  
                #                   gg <- ggplot(data = df_long, aes(x = HaulNo, y = value, color = Country)) +
                #                   geom_point(size = 1) +
                #                   facet_wrap(~variable, scales = "free_y")
                #                 ggplotly(gg)
                          #   ggplotly(ggplot(HH(), aes(HaulNo, Distance, color = Country)) + geom_point())
                #   })
                  output$HHoutplot1 <- renderPlotly({                          
                          df <- HH() %>% select(c(input$plot1_x_axis, input$plot1_y_axis, input$plot1_groupby))                          
                          gg <- ggplot(data = df, aes(x = df[,1], y = df[,2], color = df[,3])) + geom_point(size = 1) + labs(x = input$plot1_x_axis, y = input$plot1_y_axis, colour = input$plot1_groupby)
                          ggplotly(gg)
                  })
                  output$HHoutplot2 <- renderPlotly({                          
                          df <- HH() %>% select(c(input$plot2_x_axis, input$plot2_y_axis, input$plot2_groupby))                          
                          gg <- ggplot(data = df, aes(x = df[,1], y = df[,2], color = df[,3])) + geom_point(size = 1) + labs(x = input$plot2_x_axis, y = input$plot2_y_axis, colour = input$plot2_groupby)
                          ggplotly(gg)
                  })
                  output$HHoutplot3 <- renderPlotly({                          
                          df <- HH() %>% select(c(input$plot3_x_axis, input$plot3_y_axis, input$plot3_groupby))                          
                          gg <- ggplot(data = df, aes(x = df[,1], y = df[,2], color = df[,3])) + geom_point(size = 1) + labs(x = input$plot3_x_axis, y = input$plot3_y_axis, colour = input$plot3_groupby)
                          ggplotly(gg)
                  })
                  output$HHoutplot4 <- renderPlotly({                          
                          df <- HH() %>% select(c(input$plot4_x_axis, input$plot4_y_axis, input$plot4_groupby))                          
                          gg <- ggplot(data = df, aes(x = df[,1], y = df[,2], color = df[,3])) + geom_point(size = 1) + labs(x = input$plot4_x_axis, y = input$plot4_y_axis, colour = input$plot4_groupby)
                          ggplotly(gg)
                  })
                #   output$HHoutplot3 <- renderPlotly({
                #           ggplotly(ggplot(HH(), aes(HaulNo, DoorSpread, color = Country)) + geom_point())
                #   })
                #   output$HHoutplot4 <- renderPlotly({
                #           ggplotly(ggplot(HH(), aes(HaulNo, WingSpread, color = Country)) + geom_point())
                #   })

                  HL <- reactive({
                          fread(file = "df_HL.csv", sep = ",")
                        #   icesDatras::getHLdata(survey(),years(), quarters())
                          })

                  output$speciesHLValueBox <- renderValueBox({
                          num_specs <- length(unique(HL()$SpecCode))
                          num_specs <- as.numeric(num_specs)
                          valueBox(num_specs,
                                   "species recorded",
                                   icon = icon("fish"))
                  })
                  output$HLspeciesrecords <- renderTable({
                          AphiaID <- fread(file = "AphiaIDs.csv", sep = ",")
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


                CA <- reactive({
                        fread(file = "df_CA.csv", sep = ",")
                        # icesDatras::getCAdata(survey(),years(), quarters())
                        })

                output$speciesCAValueBox <- renderValueBox({
                        num_specs <- length(unique(CA()$SpecCode))
                        num_specs <- as.numeric(num_specs)
                        valueBox(num_specs,
                                "species recorded",
                                icon = icon("fish"))
                        })
                output$CAspeciesrecords <- renderTable({
                        AphiaID <- fread(file = "AphiaIDs.csv", sep = ",")
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

        #         #some issue with scales = free for plotly
                output$CAoutplot1 <- renderPlot({
                        AphiaID <- fread(file = "AphiaIDs.csv", sep = ",")
                        sps <- AphiaID[,4:5]
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
