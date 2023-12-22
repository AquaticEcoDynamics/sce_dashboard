#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny RPostgreSQL DBI
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  #sensorInfo <- scevo::sensorInfo
  
  databaseEnable <- as.logical(get_golem_config("enable", config = "database_connection"))
  print(paste0("databaseEnable: ", databaseEnable))
  
  wiskiEnable <- as.logical(get_golem_config("enable", config = "wiski_connection"))
  print(paste0("wiskiEnable: ", wiskiEnable))
  
  #rawwiski <- awss3Connect(filename = 'arms/wiski.csv')
  #rawwiski <- read.csv(file = 'www/wiski.csv', check.names = FALSE)

  
  
#### HOME PAGE ####

  
  # Generates blank web-map
  output$overviewMap <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      # leaflet::addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}',
      #                   options = tileOptions(opacity = 0.2, filter = "grayscale(100%)")) %>%
      leaflet::addTiles('https://tile.jawg.io/jawg-light/{z}/{x}/{y}{r}.png?access-token=Tzxu1wgoSHOUXGu4wCc3hLy31E4PPxVuanc4OX3BG7YjwM4ymwkqKZauiYosYn4J')%>%
      leaflet::setView(115.8613, -31.9523, 11)
  })
  
  methydro_data<-read.csv("www/Met_Hydro_sites.csv")
  Catchment_data<-read.csv("www/Catchment_monitoring_sites2_withCatchName.csv")
  Estuary_data<- read.csv("www/SCESTUARY - Copy.csv")
  Oxy_data<-read.csv("www/Oxy_sites.csv")
  flow_data <- methydro_data %>%
    dplyr::filter(group %in% 'flow')
  flowMapStations <- data.frame(
    name = flow_data$Site,
    colour = '#66c2a5',
    agency = flow_data$agency,
    lat = flow_data$lat,
    lon = flow_data$lon,
    URL = '',
    id = flow_data$stationID,
    fullname = flow_data$full_name,
    group = "Hydrology - Flow"
  )
  tide_data <- methydro_data %>%
    dplyr::filter(group %in% 'tide')
  tideMapStations <- data.frame(
    name = tide_data$Site,
    colour = '#66c2a5',
    agency = tide_data$agency,
    lat = tide_data$lat,
    lon = tide_data$lon,
    URL = '',
    id = tide_data$stationID,
    fullname = tide_data$full_name,
    group = "Hydrology - Tide"
  )
  weather_data <- methydro_data %>%
    dplyr::filter(group %in% 'met')
  weatherMapStations <- data.frame(
    name = weather_data$Site,
    colour = '#ffd92f',
    agency = weather_data$agency,
    lat = weather_data$lat,
    lon = weather_data$lon,
    URL = '',
    id = weather_data$stationID,
    fullname = weather_data$full_name,
    group = "Weather"
  )
  moor_data <- methydro_data %>%
    dplyr::filter(group %in% 'mooring')
  wqmoorMapStations <- data.frame(
    name = moor_data$Site,
    colour = "#cc99cc",
    agency = moor_data$agency,
    lat = moor_data$lat,
    lon = moor_data$lon,
    URL = '',
    id = moor_data$stationID,
    fullname = moor_data$full_name,
    group = "Water quality - Mooring"
  )
  wqcatchMapStations <- data.frame(
    name = Catchment_data$Site_Code,
    colour = "#a6d854",
    #source = "unknown",
    agency = "DBCA",
    lat = Catchment_data$Latitude,
    lon = Catchment_data$Longitude,
    URL = Catchment_data$URL,
    id = Catchment_data$AWRC_No,
    fullname = Catchment_data$SiteName_and_Suburb,
    #catchname = Catchment_data$CATCH_NAME
    group = "Water quality - Catchment"
  )
  wqestMapStations <- data.frame(
    name = Estuary_data$Project.Site.Reference,
    colour = "#8da0cb",
    #source = "unknown",
    agency = "DBCA",
    lat = Estuary_data$Latitude,
    lon = Estuary_data$Longitude,
    URL = Estuary_data$URL,
    id = Estuary_data$Site.Ref,
    fullname = Estuary_data$Site.full.Name,
    group = "Water quality - Estuary"
  )
  wqDOMapStations <- data.frame(
    name = Oxy_data$Project.Site.Reference,
    colour = "#fc8d62",
    #source = "unknown",
    agency = "DBCA",
    lat = Oxy_data$Latitude,
    lon = Oxy_data$Longitude,
    URL = '',
    id = '',
    fullname = Oxy_data$Site.Name,
    group = "Oxygenation plant"
  )
  
  allMapStations <- rbind(flowMapStations,
                          tideMapStations,
                          weatherMapStations,
                          wqmoorMapStations,
                          wqcatchMapStations,
                          wqestMapStations,
                          wqDOMapStations)


  sensorMapMarkers_overview(
    mapID = "overviewMap",
    Data = allMapStations)

  
  images <- list.files("www/Images/",full.names = T)
  output$homeimages <- renderSlickR({
    slickR(images)+
      settings(dots = TRUE, autoplay = TRUE)
  })
  
  #output$homeText <- renderPrint({ "add some text :)" })
  homemsg <- "www/homeTextImg.png"
  output$homeTextImg <- renderImage({
    list(src = homemsg,
         alt = "This is alternate text"
         # width = 300,  
         #height = 250
         ) 
  }, deleteFile = FALSE)
  
  
#### WEATHER #### 
  
  modWeatherEnable <- as.logical(get_golem_config("enable", config = "mod_weather"))
  print(paste0("modWeatherEnable: ", modWeatherEnable))
  
  # Fetch weather station data from config to map
  # weatherMapStations <- data.frame(
  #   name = configList(get_golem_config("name", config = "mod_weather_temp")),
  #   colour = configList(get_golem_config("colour", config = "mod_weather_temp")),
  #   source = configList(get_golem_config("source", config = "mod_weather_temp")),
  #   lat = configList(get_golem_config("lat", config = "mod_weather_temp")),
  #   lon = configList(get_golem_config("lon", config = "mod_weather_temp"))
  # )
  methydro_data<-read.csv("www/Met_Hydro_sites.csv")
  weather_data <- methydro_data %>%
    dplyr::filter(group %in% 'met')
  
  weatherMapStations <- data.frame(
      name = weather_data$Site,
      colour = '#ffd92f',
      agency = weather_data$agency,
      lat = weather_data$lat,
      lon = weather_data$lon,
      id = weather_data$stationID,
      fullname = weather_data$full_name
    )
    
  # Generates blank weather tab web-map
  output$weatherMap <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>%
      leaflet::setView(115.8613, -31.9523, 10)
  })
  #outputOptions(output, "weatherMap", suspendWhenHidden = FALSE)

  
  observe({
    input$navbar
    input$weatherTabset
    if(input$navbar=="Weather" && input$weatherTabset=="Weather"){
      sensorMapMarkers_methydro(
        mapID = "weatherMap",
        subGroupData = weatherMapStations
      )
      # leaflet::leafletProxy(
      #   mapId = 'weatherMap'
      # ) %>% 
      #   leaflet::clearMarkers() %>% 
      #   leaflet::addCircleMarkers(
      #     lng = as.numeric(weatherMapStations[["lon"]]), 
      #     lat = as.numeric(weatherMapStations[["lat"]]), 
      #     color = "white", 
      #     radius = 5, 
      #     weight = 2, 
      #     fillColor = weatherMapStations[["colour"]], 
      #     opacity = 1, 
      #     fillOpacity = 1,
      #     popup = paste0("<b>Station Name: </b>",weatherMapStations[["name"]], 
      #                    "<br><b>Station ID: </b>", weatherMapStations[["id"]],
      #                    "<br><b>Agency: </b>", weatherMapStations[["agency"]])
      #   )
    }
  })
  
  
##### WEATHER - TEMPERATURE (OLD) ####
  
  # modWeatherEnable <- as.logical(get_golem_config("enable", config = "mod_weather"))
  # modWeatherTempEnable <- as.logical(get_golem_config("enable", config = "mod_weather_temp"))
  # modWeatherTempName <- configList(get_golem_config("name", config = "mod_weather_temp"))
  # modWeatherTempCode <- configList(get_golem_config("sensor_code", config = "mod_weather_temp"))
  # modWeatherTempColour <- configList(get_golem_config("colour", config = "mod_weather_temp"))
  
  #print(paste0("modWeatherTempEnable: ", modWeatherTempEnable))
 
  # if(isTRUE(modWeatherTempEnable))
  #   {
  #     insertTab(
  #       inputId = "weatherTabset",
  #       select = TRUE,
  #       tabPanel(
  #         title = "Temperature",
  #         tags$summary(HTML("Select sites and date range:")),
  #         checkboxGroupInput(
  #           inputId = "weatherTempSiteCheckBox",
  #           label = NULL,
  #           inline = TRUE,
  #           choiceNames = modWeatherTempName,
  #           choiceValues =  modWeatherTempCode
  #         ),
  #         fluidRow(
  #           column(
  #             8,
  #             dateRangeInput(
  #               inputId = "weatherTempDateRange",
  #               label = NULL,
  #               start = Sys.Date()-7,
  #               end = Sys.Date()
  #             )
  #           ),
  #           column(
  #             4,
  #             actionButton(
  #               inputId = "weatherTempFetchData",
  #               label = "Plot"
  #             )
  #           )
  #         ),
  #         plotOutput("weatherTempPlot", height = "400px"),
  #         uiOutput("weatherTempDateSliderUI")
  #       )
  #     )
  #     
  # } else {
  #     print('no tab')
  #     #return(NULL)
  #   }
  # 
  # output$weatherTempDateSliderUI <- renderUI({
  #   sliderInput(
  #     inputId = "weatherTempDateSlider",
  #     "Filter dates:",
  #     min = as.Date(input$weatherTempDateRange[1]),
  #     max = as.Date(input$weatherTempDateRange[2]),
  #     value = c(
  #       as.Date(input$weatherTempDateRange[1]),
  #       as.Date(input$weatherTempDateRange[2])
  #     ),
  #     timeFormat="%Y-%m-%d",
  #     width = '95%',
  #     animate = animationOptions(1000)
  #   )
  # })
  # 
  # 
  # observeEvent(input$weatherTempFetchData,{ 
  #   #browser()
  #   if(isTRUE(databaseEnable))
  #   {
  #   weatherTempData <- databaseConnect(sensorCodes = input$weatherTempSiteCheckBox)
  #   plotData_y = "st_value_1"
  #   }else{
  #     weatherSensorCodes = input$weatherTempSiteCheckBox
  #     #filedir <- paste0("arms/",weatherSensorCodes,".csv")
  #     weatherTempData  <- awss3Connect_sensorcode(sensorCodes = weatherSensorCodes)
  #     #weatherTempData$datetime <- as.POSIXct(weatherTempData$Date,format="%Y-%m-%d %H:%M:%S") # arms is Y first
  #     plotData_y = "Data"
  #   # weatherTempData <- read.csv("www/FlowGaugesEllenBrookDS.csv")
  #   # weatherTempData$datetime <- as.POSIXct(weatherTempData$datetime,format="%d/%m/%Y")
  #   }
  #  
  #   # Get line plot colours for selected sensors
  #   weatherTempDataColours <- sensorColours(
  #     allCodes = modWeatherTempCode,
  #     allColours = modWeatherTempColour,
  #     selectedCodes = input$weatherTempSiteCheckBox
  #   )
  #   # Generate line graph from fetched data and display between slider dates
  #   output$weatherTempPlot <- renderPlot({
  #     weatherTempData <- dplyr::filter(
  #       weatherTempData,
  #       datetime >= as.POSIXct(input$weatherTempDateSlider[1]),
  #       datetime <= as.POSIXct(input$weatherTempDateSlider[2])
  #     )
  #     # Plot the graph    
  #     plotLine_s3(
  #       plotData = weatherTempData,
  #       plotDataX = "datetime",
  #       plotDataY = plotData_y,
  #       #plotDataGroup = "st_sensor_code",
  #       plotLabelX = "Date",
  #       plotLabelY = "Temperature (°C)",
  #       plotDataColours = weatherTempDataColours
  #     )
  #   })
  #   
  # })  
  
  
 ##### WEATHER - TEMPERATURE ####
  
  
  modWeatherTempEnable <- as.logical(get_golem_config("enable", config = "mod_weather_temp"))
  print(paste0("modWeatherTempEnable: ", modWeatherTempEnable))
  
  output$weathertestDateSlider <- renderUI({
    plotSlider(
      inputID = "weathertestDateSlider",
      minDate = input$weathertestDateFrom,
      maxDate = input$weathertestDateTo
    )
  })
  
  shinyjs::hide("weathertestDateSliderBox")
  
      ###get sensor Code weather
      selectedWeathersensorID <- reactive({
        req(isolate(input$weathertestSites))
        req(isolate(input$weathertestVariable))
        
        selectedWeathersites <- input$weathertestSites
        
        tmp_met <- sensorslist_hydromet %>%  
          dplyr::filter(Site %in% selectedWeathersites & s_graph_value %in% input$weathertestVariable) %>% 
          dplyr::select(s_table_name)
        
        tmp_met <- tmp_met[['s_table_name']]  #convert dataframe to vector
        tmp_met
        
      })
      
      observe({
      selectedWeathersites <- input$weathertestSites
      selectedWeatherdf <- weatherMapStations[weatherMapStations$name %in% selectedWeathersites, ]
      sensorMapMarkerHighlight_methydro (mapID = "weatherMap", 
                                         subGroupData = weatherMapStations, 
                                         selectedData = selectedWeatherdf, 
                                         hlcolour = "yellow")
      })
      
      
      observeEvent(input$weathertestFetchData,{
        if(isTRUE(databaseEnable))
        {
        weathertestData <- databaseConnect(sensorCodes = selectedWeathersensorID())
          plotData_y = "st_value_1_new"
          # weathertestData[,plotData_y][weathertestData[,plotData_y] < -10] <- NA #remove faulty data
        }else{
          
          #print(selectedWeathersensorID())
          
          weathertestData <- awss3Connect_sensorcode(sensorCodes = selectedWeathersensorID())
          plotData_y = "Data"
        }
        
        
        if (nrow(weathertestData) > 0) {
          weathertestData[,plotData_y][weathertestData[,plotData_y] < -10] <- NA

        # Generate line graph from fetched data and diplay between slider dates
        output$weathertestPlot <- plotly::renderPlotly({
          #output$weathertestPlot <- renderPlot({
          
          weathertestData <- dplyr::filter(
            weathertestData,
            datetime >= as.POSIXct(input$weathertestDateSlider[1]),
            datetime <= as.POSIXct(input$weathertestDateSlider[2])
          )
          
          #print(weathertestData)
          
          # Plot the graph
          library(plyr)
          
          weathertestData <- join(weathertestData,sensorslist_hydromet,by="s_table_name")
          weathertestData$Site <- as.factor(weathertestData$Site)
          
          plotLineMulti(
            plotData = weathertestData,
            plotDataX = "datetime",
            plotDataY = plotData_y,
            plotDataGroup = "Site",
            plotLabelX = "Date",
            plotLabelY = paste0(isolate(input$weathertestVariable))
          )
          

    })
        
        }else{
            # if no data print an error message
            no_data_df <- data.frame(label = "data unavailable: this may be caused by invalid site/variable combination")
            errorplot <- ggplot2::ggplot(no_data_df, aes(x = 1, y = 1, label = label)) +
              geom_text(size = 6) +
              theme_void()

            output$weathertestPlot <- plotly::renderPlotly({errorplot})
          
        }
        
    
  })
      
 ##### WEATHER - S3 ####
      
      output$weatherDateSlider <- renderUI({
        plotSlider(
          inputID = "weatherDateSlider",
          minDate = input$weatherDateFrom,
          maxDate = input$weatherDateTo
        )
      })
      
      shinyjs::hide("weatherDateSliderBox")
      
      observeEvent(input$weatherFetchData,{
        #browser()
        #if(isTRUE(databaseEnable))
        #{
        filedir <- paste0("data-warehouse/bom/idy/",input$weatherSites,"_", input$weatherVariable,"_DATA.csv")
        #weatherData <- awss3Connect(filename = 'data-warehouse/bom/idy/9021_Air_Temperature_DATA.csv')
        weatherData <- awss3Connect(filename = filedir)
        weatherData$datetime <- as.POSIXct(weatherData$Date,format="%Y-%m-%d %H:%M:%S")
        #}else{
        #  weatherTempData <- read.csv("www/FlowGaugesEllenBrookDS.csv")
        #  weatherTempData$datetime <- as.POSIXct(weatherTempData$datetime,format="%d/%m/%Y")
        #}
        
        # # Get line plot colours for selected sensors
        # weatherTempDataColours <- sensorColours(
        #   allCodes = modWeatherTempCode,
        #   allColours = modWeatherTempColour,
        #   selectedCodes = input$weatherTempSiteCheckBox
        # )
        # Generate line graph from fetched data and display between slider dates
        output$weatherPlot <- plotly::renderPlotly({
          weatherData <- dplyr::filter(
            weatherData,
            datetime >= as.POSIXct(input$weatherDateSlider[1]),
            datetime <= as.POSIXct(input$weatherDateSlider[2])
          )
          # Plot the graph
          
          # plotLine_s3(          #commented out by Matt as not working with docker?
          #   plotData = weatherData,
          #   plotDataX = "datetime",
          #   plotDataY = "Data",
          #   #plotDataGroup = "st_sensor_code",
          #   plotLabelX = "Date",
          #   plotLabelY = input$weatherVariable,
          #   plotDataColours = "#a6d854"
          # )
          
          plot <- ggplot2::ggplot(
            data = weatherData,
            mapping = ggplot2::aes(
              x = base::as.POSIXct(weatherData[, "datetime"]),
              y = weatherData[,"Data"],
              #      colour = base::as.character(plotData[,plotDataGroup])
            )
          ) +
            ggplot2::geom_line() 
          plot
        })
        
      })
   
  
 ##### WEATHER - ABOUT ####
      
      output$weatherAbout <- renderPrint({
        "Weather data were sourced from Bureau of Meteorology (BOM) and Department of Primary Industries and Regional Development (DPIRD) and are not quality-checked."
      })   
      
#### HYDRO #### 
      
  modHydroEnable <- as.logical(get_golem_config("enable", config = "mod_hydro"))
  print(paste0("modHydroEnable: ", modHydroEnable))
  
  # Fetch hydro station data from config to map
  # hydroMapStations <- data.frame(
  #   name = configList(get_golem_config("name", config = "mod_hydro_flow")),
  #   colour = configList(get_golem_config("colour", config = "mod_hydro_flow")),
  #   source = configList(get_golem_config("source", config = "mod_hydro_flow")),
  #   lat = configList(get_golem_config("lat", config = "mod_hydro_flow")),
  #   lon = configList(get_golem_config("lon", config = "mod_hydro_flow"))
  # )
      
  methydro_data<-read.csv("www/Met_Hydro_sites.csv")
  flow_data <- methydro_data %>%
        dplyr::filter(group %in% 'flow')
      
  flowMapStations <- data.frame(
        name = flow_data$Site,
        colour = '#66c2a5',
        agency = flow_data$agency,
        lat = flow_data$lat,
        lon = flow_data$lon,
        id = flow_data$stationID,
        fullname = flow_data$full_name
      )
      
  tide_data <- methydro_data %>%
        dplyr::filter(group %in% 'tide')
      
  tideMapStations <- data.frame(
        name = tide_data$Site,
        colour = '#66c2a5',
        agency = tide_data$agency,
        lat = tide_data$lat,
        lon = tide_data$lon,
        id = tide_data$stationID,
        fullname = tide_data$full_name
      )
      
  #Generates blank hydro tab web-map
  output$hydroMap <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>%
      leaflet::setView(115.8613, -31.9523, 10)
  })
  #outputOptions(output, "weatherMap", suspendWhenHidden = FALSE)
  
  observe({
    
    input$navbar
    input$hydroTabset
    if(input$navbar=="Hydrology" && input$hydroTabset=="Flow"){
      sensorMapMarkers_methydro(
        mapID = "hydroMap",
        subGroupData = flowMapStations
      )
      # leaflet::leafletProxy(
      #   mapId = 'hydroMap'
      # ) %>%
      #   leaflet::clearMarkers() %>%
      #   leaflet::addCircleMarkers(
      #     lng = as.numeric(flowMapStations[["lon"]]),
      #     lat = as.numeric(flowMapStations[["lat"]]),
      #     color = "white",
      #     radius = 7,
      #     weight = 2,
      #     fillColor = flowMapStations[["colour"]],
      #     opacity = 1,
      #     fillOpacity = 1,
      #     popup = paste0("<b>Station Name: </b>",flowMapStations[["name"]], 
      #                    "<br><b>Station ID: </b>", flowMapStations[["id"]],
      #                    "<br><b>Agency: </b>", flowMapStations[["agency"]])
      #   )
    }
  })
 
  # # Filter sensorInfo data to only hydro sensors
  # hydroSensorInfo <- sensorInfo[sensorInfo[["group"]]=="hydro",]
  # observeEvent(input$hydroTabset,{
  #   switch(
  #     input$hydroTabset,
  #     "Flow" = sensorMapMarkers(
  #       mapID = "hydroMap",
  #       data = hydroSensorInfo,
  #       subGroup = input$hydroTabset
  #       ),
  #     "Tide" = sensorMapMarkers(
  #       mapID = "hydroMap",
  #       data = hydroSensorInfo,
  #       subGroup = input$hydroTabset
  #     )
  #   )
  # })
  
 
  observeEvent(input$hydroTabset,{
    switch(
      input$hydroTabset,
      "Flow" = sensorMapMarkers_methydro(
        mapID = "hydroMap",
        subGroupData = flowMapStations
      ),
      "Tide" = sensorMapMarkers_methydro(
        mapID = "hydroMap",
        subGroupData = tideMapStations
      )
    )
  })
  
##### HYDRO - FLOW (OLD) ####
  
  # modHydroEnable <- as.logical(get_golem_config("enable", config = "mod_hydro"))
  # modHydroFlowEnable <- as.logical(get_golem_config("enable", config = "mod_hydro_flow"))
  # modHydroFlowName <- configList(get_golem_config("name", config = "mod_hydro_flow"))
  # modHydroFlowCode <- configList(get_golem_config("sensor_code", config = "mod_hydro_flow"))
  # modHydroFlowColour <- configList(get_golem_config("colour", config = "mod_hydro_flow"))
  # 
  # print(paste0("modHydroFlowEnable: ", modHydroFlowEnable))
  # 
  # if(isTRUE(modHydroFlowEnable))
  # {
  #   insertTab(
  #     inputId = "flowTabset",
  #     select = TRUE,
  #     tabPanel(
  #       title = "Flow",
  #       tags$summary(HTML("Select sites and date range:")),
  #       checkboxGroupInput(
  #         inputId = "hydroFlowSiteCheckBox",
  #         label = NULL,
  #         inline = TRUE,
  #         choiceNames = modHydroFlowName,
  #         choiceValues =  modHydroFlowCode
  #       ),
  #       fluidRow(
  #         column(
  #           8,
  #           dateRangeInput(
  #             inputId = "hydroFlowDateRange",
  #             label = NULL,
  #             start = Sys.Date()-7,
  #             end = Sys.Date()
  #           )
  #         ),
  #         column(
  #           4,
  #           actionButton(
  #             inputId = "hydroFlowFetchData",
  #             label = "Plot"
  #           )
  #         )
  #       ),
  #       plotOutput("hydroFlowPlot", height = "400px"),
  #       uiOutput("hydroFlowDateSliderUI")
  #     )
  #   )
  # 
  #   
  # } else {
  #   print('no tab')
  #   #return(NULL)
  # }
  # 
  # #Update slider from calendar date inputs
  # output$hydroFlowDateSlider <- renderUI({
  #   plotSlider(
  #     inputID = "hydroFlowDateSlider",
  #     minDate = input$hydroFlowDateRange[1],
  #     maxDate = input$hydroFlowDateRange[2]
  #   )
  # })
  # 
  # shinyjs::hide("hydroFlowDateSliderBox")
  # 
  # # On button click, fetch sensor data from SCEVO and graph
  # observeEvent(input$hydroFlowFetchData,{
  #   shinyjs::show("hydroFlowDateSliderBox", anim = TRUE, animType = "fade")
  #   if(isTRUE(databaseEnable))
  #   {
  #   hydroFlowData <- databaseConnect(sensorCodes = input$hydroFlowSiteCheckBox)
  #   }else{
  #   hydroFlowData <- read.csv("www/FlowGaugesEllenBrookDS.csv")
  #   hydroFlowData$datetime <- as.POSIXct(hydroFlowData$datetime,format="%d/%m/%Y")
  #   }
  # 
  #   # Get line plot colours for selected sensors
  #   hydroFlowDataColours <- sensorColours(
  #     allCodes = modHydroFlowCode,
  #     allColours = modHydroFlowColour,
  #     selectedCodes = input$hydroFlowSiteCheckBox
  #   )
  # 
  #   # Generate line graph from fetched data and display between slider dates
  #   output$hydroFlowPlot <- renderPlot({
  #     hydroFlowData <- dplyr::filter(
  #       hydroFlowData,
  #       datetime >= as.POSIXct(input$hydroFlowDateSlider[1]),
  #       datetime <= as.POSIXct(input$hydroFlowDateSlider[2])
  # 
  #     )
  # 
  #   # Plot the graph
  #   plotLine(
  #     plotData = hydroFlowData,
  #     plotDataX = "datetime",
  #     plotDataY = "st_value_1_new",
  #     plotDataGroup = "st_sensor_code",
  #     plotLabelX = "Date",
  #     plotLabelY = "Flow (m3/s)",
  #     plotDataColours = hydroFlowDataColours
  #   )
  #   })
  # })

  
##### HYDRO - FLOW ####
  
  modHydroFlowEnable <- as.logical(get_golem_config("enable", config = "mod_hydro_flow"))
  print(paste0("modHydroFlowEnable: ", modHydroFlowEnable))
  
  output$hydroFlowtestDateSlider <- renderUI({
    plotSlider(
      inputID = "hydroFlowtestDateSlider",
      minDate = input$hydroFlowtestDateFrom,
      maxDate = input$hydroFlowtestDateTo
    )
  })
  
  shinyjs::hide("hydroFlowtestDateSliderBox")
  
  ###get sensor Code 
  selectedFlowsensorID <- reactive({
    req(isolate(input$hydroFlowtestSites))
    req(isolate(input$hydroFlowtestVariable))
    
    selectedFlowsites <- input$hydroFlowtestSites
    
    tmp_flow <- sensorslist_hydromet %>%  
      dplyr::filter(Site %in% selectedFlowsites & s_graph_value %in% input$hydroFlowtestVariable) %>% 
      dplyr::select(s_table_name)
    
    tmp_flow <- tmp_flow[['s_table_name']]  #convert dataframe to vector
    tmp_flow
    
  })
  
  observe({
  selectedFlowsites <- input$hydroFlowtestSites
  selectedFlowdf <- flowMapStations[flowMapStations$name %in% selectedFlowsites, ]
  sensorMapMarkerHighlight_methydro (mapID = "hydroMap", 
                                     subGroupData = flowMapStations, 
                                     selectedData = selectedFlowdf, 
                                     hlcolour = "yellow")
  })
  
  observeEvent(input$hydroFlowtestFetchData,{
    if(isTRUE(databaseEnable))
    {
      hydroFlowtestData <- databaseConnect(sensorCodes = selectedFlowsensorID())
      plotData_y = "st_value_1_new"
    }else{
      hydroFlowtestData <- awss3Connect_sensorcode(sensorCodes = selectedFlowsensorID())
      plotData_y = "Data"
    }
    
    # Generate line graph from fetched data and diplay between slider dates
    output$hydroFlowtestPlot <- plotly::renderPlotly({
      #output$hydroFlowtestPlot <- renderPlot({
      
      hydroFlowtestData <- dplyr::filter(
        hydroFlowtestData,
        datetime >= as.POSIXct(input$hydroFlowtestDateSlider[1]),
        datetime <= as.POSIXct(input$hydroFlowtestDateSlider[2])
      )
      
      # Plot the graph
      library(plyr)
      
      hydroFlowtestData <- join(hydroFlowtestData,sensorslist_hydromet,by="s_table_name")
      hydroFlowtestData$Site <- as.factor(hydroFlowtestData$Site)
      
      plotLineMulti(
        plotData = hydroFlowtestData,
        plotDataX = "datetime",
        plotDataY = plotData_y,
        plotDataGroup = "Site",
        plotLabelX = "Date",
        plotLabelY = paste0(isolate(input$hydroFlowtestVariable))
      )
      
    })
    
  })
  
##### HYDRO - TIDE ####  
  
  modHydroTideEnable <- as.logical(get_golem_config("enable", config = "mod_hydro_tide"))
  print(paste0("modHydroTideEnable: ", modHydroTideEnable))
  
  output$hydroTidetestDateSlider <- renderUI({
    plotSlider(
      inputID = "hydroTidetestDateSlider",
      minDate = input$hydroTidetestDateFrom,
      maxDate = input$hydroTidetestDateTo
    )
  })
  
  shinyjs::hide("hydroTidetestDateSliderBox")
  
  ###get sensor Code weather
  selectedTidesensorID <- reactive({
    req(isolate(input$hydroTidetestSites))
    req(isolate(input$hydroTidetestVariable))
    
    selectedTidesites <- input$hydroTidetestSites
    
    tmp_tide <- sensorslist_hydromet %>%  
      dplyr::filter(Site %in% selectedTidesites & s_graph_value %in% input$hydroTidetestVariable) %>% 
      dplyr::select(s_table_name)
    
    tmp_tide <- tmp_tide[['s_table_name']]  #convert dataframe to vector
    tmp_tide
    
  })
  
  observe({
  selectedTidesites <- input$hydroTidetestSites
  selectedTidedf <- tideMapStations[tideMapStations$name %in% selectedTidesites, ]
  sensorMapMarkerHighlight_methydro (mapID = "hydroMap", 
                                     subGroupData = tideMapStations, 
                                     selectedData = selectedTidedf, 
                                     hlcolour = "yellow")
  })
  
  observeEvent(input$hydroTidetestFetchData,{
    if(isTRUE(databaseEnable))
    {
      hydroTidetestData <- databaseConnect(sensorCodes = selectedTidesensorID())
      plotData_y = "st_value_1_new"
    }else{
      hydroTidetestData <- awss3Connect_sensorcode(sensorCodes = selectedTidesensorID())
      plotData_y = "Data"
    }
    
    # Generate line graph from fetched data and diplay between slider dates
    output$hydroTidetestPlot <- plotly::renderPlotly({
      #output$hydroTidetestPlot <- renderPlot({
      
      hydroTidetestData <- dplyr::filter(
        hydroTidetestData,
        datetime >= as.POSIXct(input$hydroTidetestDateSlider[1]),
        datetime <= as.POSIXct(input$hydroTidetestDateSlider[2])
      )
      
      # Plot the graph
      library(plyr)
      
      hydroTidetestData <- join(hydroTidetestData,sensorslist_hydromet,by="s_table_name")
      hydroTidetestData$Site <- as.factor(hydroTidetestData$Site)
      
      plotLineMulti(
        plotData = hydroTidetestData,
        plotDataX = "datetime",
        plotDataY = plotData_y,
        plotDataGroup = "Site",
        plotLabelX = "Date",
        plotLabelY = paste0(isolate(input$hydroTidetestVariable))
      )
      
    })
    
  })
  
##### HYDRO - ABOUT ####
  
  output$hydroAbout <- renderPrint({
    "Flow data were sourced from the Water Information Reporting (WIR) program of Department of Water and Environmental Regulation (DWER) and are not quality-checked. 
     Tide data were sourced from Department of Transport (DOT) and Bureau of Meteorology (BOM) and are not quality-checked."
  }) 
  
##### HYDRO - TIDE (OLD) ####
  
  # modHydroEnable <- as.logical(get_golem_config("enable", config = "mod_hydro"))
  # modHydroTideEnable <- as.logical(get_golem_config("enable", config = "mod_hydro_tide"))
  # modHydroTideName <- configList(get_golem_config("name", config = "mod_hydro_tide"))
  # modHydroTideCode <- configList(get_golem_config("sensor_code", config = "mod_hydro_tide"))
  # modHydroTideColour <- configList(get_golem_config("colour", config = "mod_hydro_tide"))
  # 
  # print(paste0("modHydroTideEnable: ", modHydroTideEnable))
  # 
  # if(isTRUE(modHydroTideEnable))
  # {
  #   insertTab(
  #     inputId = "tideTabset",
  #     select = TRUE,
  #     tabPanel(
  #       title = "Tide",
  #       tags$summary(HTML("Select sites and date range:")),
  #       checkboxGroupInput(
  #         inputId = "hydroTideSiteCheckBox",
  #         label = NULL,
  #         inline = TRUE,
  #         choiceNames = modHydroTideName,
  #         choiceValues =  modHydroTideCode
  #       ),
  #       fluidRow(
  #         column(
  #           8,
  #           dateRangeInput(
  #             inputId = "hydroTideDateRange",
  #             label = NULL,
  #             start = Sys.Date()-7,
  #             end = Sys.Date()
  #           )
  #         ),
  #         column(
  #           4,
  #           actionButton(
  #             inputId = "hydroTideFetchData",
  #             label = "Plot"
  #           )
  #         )
  #       ),
  #       plotOutput("hydroTidePlot", height = "400px"),
  #       uiOutput("hydroTideDateSliderUI")
  #     )
  #   )
  #   
  #   # Update slider from calendar date inputs
  #   output$hydroTideDateSlider <- renderUI({
  #     plotSlider(
  #       inputID = "hydroTideDateSlider",
  #       minDate = input$hydroTideDateRange[1],
  #       maxDate = input$hydroTideDateRange[2]
  #     )
  #   })
  #   shinyjs::hide("hydroTideDateSliderBox")
  #   
  #   # On button click, fetch sensor data from SCEVO and graph
  #   observeEvent(input$hydroTideFetchData,{
  #     shinyjs::show("hydroTideDateSliderBox", anim = TRUE, animType = "fade")
  #     if(isTRUE(databaseEnable))
  #     {
  #     hydroTideData <- databaseConnect(sensorCodes = input$hydroTideSiteCheckBox)
  #     }else{
  #     hydroTideData <- read.csv("www/FlowGaugesEllenBrookDS.csv")
  #     hydroTideData$datetime <- as.POSIXct(hydroTideData$datetime,format="%d/%m/%Y")
  #     }
  #     # library(arrow)
  #     # hydroTideData <- data.frame(read_parquet("www/FFFBH01_Tidal_Height_DATA.parquet"))
  #     # colnames(hydroTideData)<-c("datetime","Depth","st_value_1","QC")
  #     # hydroTideData$datetime <- as.POSIXct(hydroTideData$datetime,format="%d-%m-%Y %H:%M:%S")
  #     # hydroTideData$st_sensor_code <-"1087"
  #     
  #     #print(head(hydroTideData))
  #     
  #     # Get line plot colours for selected sensors
  #     hydroTideDataColours <- sensorColours(
  #       allCodes = modHydroTideCode,
  #       allColours = modHydroTideColour,
  #       selectedCodes = input$hydroTideSiteCheckBox
  #     )
  #     
  #     # Generate line graph from fetched data and display between slider dates
  #     output$hydroTidePlot <- renderPlot({
  #       hydroTideData <- dplyr::filter(
  #         hydroTideData,
  #         datetime >= as.POSIXct(input$hydroTideDateSlider[1]),
  #         datetime <= as.POSIXct(input$hydroTideDateSlider[2])
  #       )
  #       
  #       # Plot the graph
  #       plotLine(
  #         plotData = hydroTideData,
  #         plotDataX = "datetime",
  #         plotDataY = "st_value_1",
  #         plotDataGroup = "st_sensor_code",
  #         plotLabelX = "Date",
  #         plotLabelY = "Tide (mAHD)",
  #         plotDataColours = hydroTideDataColours
  #       )
  #     })
  #   })
  #   
  # } 
  # else {
  #   print('no tab')
  #   #return(NULL)
  # }

  # # Update slider from calendar date inputs
  # output$hydroTideDateSlider <- renderUI({
  #   plotSlider(
  #     inputID = "hydroTideDateSlider",
  #     minDate = input$hydroTideDateFrom,
  #     maxDate = input$hydroTideDateTo
  #   )
  # })
  # 
  # # On button click, fetch sensor data from SCEVO and graph
  # observeEvent(input$hydroTideFetchData,{
  #   print(input$hydroTideSiteCheckBox)
  #   #hydroTideData <- databaseConnect(sensorCodes = input$hydroTideSiteCheckBox)
  #   hydroTideData <- read.csv("C:/Users/00101765/AED Dropbox/Sherry Zhai/SCEVO/dashboard - Copy/FlowGaugesEllenBrookDS.csv")
  #   hydroTideData$datetime <- as.POSIXct(hydroTideData$datetime,format="%d/%m/%Y")
  #   print(head(hydroTideData))
  #   
  #   # Get line plot colours for selected sensors
  #     hydroTideDataColours <- sensorColours(
  #       allCodes = modHydroTideCode,
  #       allColours = modHydroTideColour,
  #       selectedCodes = input$hydroTideSiteCheckBox
  #     )
  #   
  #   # Generate line graph from fetched data and display between slider dates
  #   output$hydroTidePlot <- renderPlot({
  #     hydroTideData <- dplyr::filter(
  #       hydroTideData,
  #       datetime >= as.POSIXct(input$hydroTideDateSlider[1]),
  #       datetime <= as.POSIXct(input$hydroTideDateSlider[2])
  #     )
  # 
  #     # Plot the graph
  #     plotLine(
  #       plotData = hydroTideData,
  #       plotDataX = "datetime",
  #       plotDataY = "st_value_1",
  #       plotDataGroup = "st_sensor_code",
  #       plotLabelX = "Date",
  #       plotLabelY = "Tide (mAHD)",
  #       plotDataColours = hydroTideDataColours
  #     )
  #   })
  # })

  
#### WATER QUALITY####
  
  modWqEnable <- as.logical(get_golem_config("enable", config = "mod_wq"))
  modWqCatchEnable <- as.logical(get_golem_config("enable", config = "mod_wq_catch"))
  # modWqCatchName <- configList(get_golem_config("name", config = "mod_wq_catch"))
  # modWqCatchCode <- configList(get_golem_config("sensor_code", config = "mod_wq_catch"))
  # modWqCatchColour <- configList(get_golem_config("colour", config = "mod_wq_catch"))
  modWqEstEnable <- as.logical(get_golem_config("enable", config = "mod_wq_est"))
  # modWqEstName <- configList(get_golem_config("name", config = "mod_wq_est"))
  # modWqEstCode <- configList(get_golem_config("sensor_code", config = "mod_wq_est"))
  # modWqEstColour <- configList(get_golem_config("colour", config = "mod_wq_est"))
  
  print(paste0("modWqEnable: ", modWqEnable))
  print(paste0("modWqCatchEnable: ", modWqCatchEnable))
  print(paste0("modWqEstEnable: ", modWqEstEnable))
  
  #Fetch wq data from config to map
  # wqcatchMapStations <- data.frame(
  #   name = configList(get_golem_config("name", config = "mod_wq_catch")),
  #   colour = configList(get_golem_config("colour", config = "mod_wq_catch")),
  #   source = configList(get_golem_config("source", config = "mod_wq_catch")),
  #   lat = configList(get_golem_config("lat", config = "mod_wq_catch")),
  #   lon = configList(get_golem_config("lon", config = "mod_wq_catch")),
  #     URL = "URL",
  #     id = "AWRC_No",
  #     fullname = "SiteName_and_Suburb"
  # )
  # wqestMapStations <- data.frame(
  #   name = configList(get_golem_config("name", config = "mod_wq_catch")),
  #   colour = configList(get_golem_config("colour", config = "mod_wq_catch")),
  #   source = configList(get_golem_config("source", config = "mod_wq_catch")),
  #   lat = configList(get_golem_config("lat", config = "mod_wq_catch")),
  #   lon = configList(get_golem_config("lon", config = "mod_wq_catch")),
  #   URL = "URL",
  #   id = "AWRC_No",
  #   fullname = "SiteName_and_Suburb"
  # )
  
  Catchment_data<-read.csv("www/Catchment_monitoring_sites2_withCatchName.csv")
  Estuary_data<- read.csv("www/SCESTUARY - Copy.csv")
  wqcatchMapStations <- data.frame(
    name = Catchment_data$Site_Code,
    colour = "#a6d854",
    source = "unknown",
    lat = Catchment_data$Latitude,
    lon = Catchment_data$Longitude,
    URL = Catchment_data$URL,
    id = Catchment_data$AWRC_No,
    fullname = Catchment_data$SiteName_and_Suburb,
    catchname = Catchment_data$CATCH_NAME
  )
  wqestMapStations <- data.frame(
    name = Estuary_data$Project.Site.Reference,
    colour = "#8da0cb",
    source = "unknown",
    lat = Estuary_data$Latitude,
    lon = Estuary_data$Longitude,
    URL = Estuary_data$URL,
    id = Estuary_data$Site.Ref,
    fullname = Estuary_data$Site.full.Name
  )
  
  methydro_data<-read.csv("www/Met_Hydro_sites.csv")
  moor_data <- methydro_data %>%
    dplyr::filter(group %in% 'mooring')
  
  wqmoorMapStations <- data.frame(
    name = moor_data$Site,
    colour = '#cc99cc',
    agency = moor_data$agency,
    lat = moor_data$lat,
    lon = moor_data$lon,
    id = moor_data$stationID,
    fullname = moor_data$full_name
  )
  
# Generates blank WQ tab web-map
  # output$wqMap <- leaflet::renderLeaflet({
  #   leaflet::leaflet() %>%
  #     leaflet::addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>%
  #     leaflet::setView(115.8613, -31.9523, 9) 
  # })
  output$wqMap <- leaflet::renderLeaflet({
    webMap()
  })
  
  observe({
    input$navbar
    input$wqTabset
    if(input$navbar=="Water quality" && input$wqTabset=="Catchment"){
      sensorMapMarkers_wq(
        mapID = "wqMap",
        subGroupData = wqcatchMapStations
      )
    }
  })
  
  observeEvent(input$wqTabset,{
    switch(
      input$wqTabset,
      "Catchment" = sensorMapMarkers_wq(
        mapID = "wqMap",
        subGroupData = wqcatchMapStations
      ),
      "Catchment Summary" = sensorMapMarkers_wq(
        mapID = "wqMap",
        subGroupData = wqcatchMapStations
      ),
      "Estuary" = sensorMapMarkers_wq(
        mapID = "wqMap",
        subGroupData = wqestMapStations
      ),
      "Estuary Summary" = sensorMapMarkers_wq(
        mapID = "wqMap",
        subGroupData = wqestMapStations
      ),
      "Profiles" = sensorMapMarkers_wq(
        mapID = "wqMap",
        subGroupData = wqestMapStations
      ),
      "Profiles - historic" = sensorMapMarkers_wq(
        mapID = "wqMap",
        subGroupData = wqestMapStations
      ),
      "Mooring" = sensorMapMarkers_methydro(
        mapID = "wqMap",
        subGroupData = wqmoorMapStations
      )
      
    )
  })
  # observe({
  #   input$navbar
  #   input$wqTabset
  #   if(input$navbar=="Water quality" && input$wqTabset=="Overview"){
  #     leaflet::leafletProxy(
  #       mapId = 'wqMap'
  #     ) %>%
  #       leaflet::clearMarkers() %>%
  #       leaflet::addCircleMarkers(
  #         lng = as.numeric(wqcatchMapStations[["lon"]]),
  #         lat = as.numeric(wqcatchMapStations[["lat"]]),
  #         color = "white",
  #         radius = 5,
  #         weight = 2,
  #         fillColor = wqcatchMapStations[["colour"]],
  #         opacity = 1,
  #         fillOpacity = 1,
  #         # popup = paste0("<b>Station Name: </b>",wqcatchMapStations[["name"]],
  #         #                "<br><b>Agency: </b>", wqcatchMapStations[["source"]]))
  #       popup = paste0("<b>", paste0("<img src=' ",wqcatchMapStations[["URL"]]," ' width='100px' height='100px'>"),"</b></br>",
  #                      paste0("<b>Name: </b>", wqcatchMapStations[["fullname"]] ),"</b></br>",
  #                      paste0("<b>Site code: </b>", wqcatchMapStations[["name"]] ),"</b></br>",
  #                      paste0("<b>Site. Ref: </b>", wqcatchMapStations[["id"]]),
  #                      "</b></br>"))%>% 
  #       leaflet::addCircleMarkers(
  #         lng = as.numeric(wqestMapStations[["lon"]]),
  #         lat = as.numeric(wqestMapStations[["lat"]]),
  #         color = "white",
  #         radius = 5,
  #         weight = 2,
  #         fillColor = wqestMapStations[["colour"]],
  #         opacity = 1,
  #         fillOpacity = 1,
  #         # popup = paste0("<b>Station Name: </b>",wqestMapStations[["name"]],
  #         #                "<br><b>Agency: </b>", wqestMapStations[["source"]]))
  #         popup = paste0("<b>", paste0("<img src=' ",wqestMapStations[["URL"]]," ' width='100px' height='100px'>"),"</b></br>",
  #                        paste0("<b>Name: </b>", wqestMapStations[["fullname"]] ),"</b></br>",
  #                        paste0("<b>Site code: </b>", wqestMapStations[["name"]] ),"</b></br>",
  #                        paste0("<b>Site. Ref: </b>", wqestMapStations[["id"]]),
  #                        "</b></br>"))%>% 
  #       
  #       leaflet::addLayersControl(baseGroups = c("Ecological Managment Zones","Subcatchments", "Elevation","All off"),
  #                                 #overlayGroups = c("Estuary sites","Catchment sites"),
  #                                 position="topright",
  #                                 options = leaflet::layersControlOptions(collapsed = FALSE))%>%
  #       addLegendCustom(colors, labels, sizes,shapes,borders=NA,title)
  #   }else{
  #     if(input$navbar=="Water quality" && input$wqTabset=="Catchment"){
  #       leaflet::leafletProxy(
  #         mapId = 'wqMap'
  #       ) %>% 
  #         leaflet::clearMarkers() %>% 
  #         leaflet::addCircleMarkers(
  #           lng = as.numeric(wqcatchMapStations[["lon"]]), 
  #           lat = as.numeric(wqcatchMapStations[["lat"]]), 
  #           color = "white", 
  #           radius = 5, 
  #           weight = 2, 
  #           fillColor = wqcatchMapStations[["colour"]], 
  #           opacity = 1, 
  #           fillOpacity = 1,
  #           # popup = paste0("<b>Station Name: </b>",wqcatchMapStations[["name"]], 
  #           #                "<br><b>Agency: </b>", wqcatchMapStations[["source"]]))
  #           popup = paste0("<b>", paste0("<img src=' ",wqcatchMapStations[["URL"]]," ' width='100px' height='100px'>"),"</b></br>",
  #             paste0("<b>Name: </b>", wqcatchMapStations[["fullname"]] ),"</b></br>",
  #             paste0("<b>Site code: </b>", wqcatchMapStations[["name"]] ),"</b></br>",
  #             paste0("<b>Site. Ref: </b>", wqcatchMapStations[["id"]]),
  #             "</b></br>"))%>% 
  #         leaflet::addLayersControl(baseGroups = c("Ecological Managment Zones","Subcatchments", "Elevation","All off"), 
  #                                   #overlayGroups = c("Estuary sites","Catchment sites"),position="topright",
  #                                   options = leaflet::layersControlOptions(collapsed = FALSE))
  #     }else{
  #       if(input$navbar=="Water quality" && input$wqTabset=="Estuary"){
  #       leaflet::leafletProxy(
  #         mapId = 'wqMap'
  #       ) %>% 
  #         leaflet::clearMarkers() %>% 
  #         leaflet::addCircleMarkers(
  #           lng = as.numeric(wqestMapStations[["lon"]]), 
  #           lat = as.numeric(wqestMapStations[["lat"]]), 
  #           color = "white", 
  #           radius = 5, 
  #           weight = 2, 
  #           fillColor = wqestMapStations[["colour"]], 
  #           opacity = 1, 
  #           fillOpacity = 1,
  #           # popup = paste0("<b>Station Name: </b>",wqestMapStations[["name"]], 
  #           #                "<br><b>Agency: </b>", wqestMapStations[["source"]]))
  #           popup = paste0("<b>", paste0("<img src=' ",wqestMapStations[["URL"]]," ' width='100px' height='100px'>"),"</b></br>",
  #                      paste0("<b>Name: </b>", wqestMapStations[["fullname"]] ),"</b></br>",
  #                      paste0("<b>Site code: </b>", wqestMapStations[["name"]] ),"</b></br>",
  #                      paste0("<b>Site. Ref: </b>", wqestMapStations[["id"]]),
  #                      "</b></br>"))%>% 
  #         leaflet::addLayersControl(baseGroups = c("Ecological Managment Zones","Subcatchments", "Elevation","All off"), 
  #                                   #overlayGroups = c("Estuary sites","Catchment sites"),position="topright",
  #                                   options = leaflet::layersControlOptions(collapsed = FALSE))
  #       }else{
  #         leaflet::leafletProxy(
  #           mapId = 'wqMap'
  #         ) %>% 
  #           leaflet::addPolylines(data = DATP, stroke = T, weight = 4,opacity = 1,
  #                                 color= c("orange","yellow"), 
  #                                 group = "Profile", label = ~Name)%>%
  #           leaflet::addLayersControl(baseGroups = c("Ecological Managment Zones","Subcatchments","Elevation","Profile","All off"), 
  #                                   #overlayGroups = c("Estuary sites","Catchment sites"),position="topright",
  #                                   options = leaflet::layersControlOptions(collapsed = FALSE))
  #         #%>%
  #           # leaflet::addLegend("bottomright", pal = profpal, values = ~Name,
  #           #           title = "profile line",
  #           #           opacity = 1
  #           #)
  #     }
  #     }
  #   }
  # })
  
  #not working
  # observeEvent(input$wqTabset,{
  #   switch(
  #     input$wqTabset,
  #     "Overview" = sensorMapMarkers(
  #       mapID = "hydroMap",
  #       data = hydroSensorInfo,
  #       subGroup = input$hydroTabset
  #     ),
  #     "Catchment" = sensorMapMarkers(
  #       mapID = "hydroMap",
  #       data = hydroSensorInfo,
  #       subGroup = input$hydroTabset
  #     ),
  #     "Estuary" = sensorMapMarkers(
  #       mapID = "hydroMap",
  #       data = hydroSensorInfo,
  #       subGroup = input$hydroTabset
  #     )
  #   )
  # })

    
##### WQ - CATCHMENT ####
  
  # if(isTRUE(modWqCatchEnable))
  # {
  #   insertTab(
  #     inputId = "catchTabset",
  #     select = TRUE,
  #     tabPanel(
  #       title = "Catchment",
  #       tags$summary(HTML("Select sites and date range:")),
  #       checkboxGroupInput(
  #         inputId = "wqCatchSiteCheckBox",
  #         label = NULL,
  #         inline = TRUE,
  #         choiceNames = modWqCatchName,
  #         choiceValues =  modWqCatchCode
  #       ),
  #       fluidRow(
  #         column(
  #           8,
  #           dateRangeInput(
  #             inputId = "wqCatchDateRange",
  #             label = NULL,
  #             start = Sys.Date()-7,
  #             end = Sys.Date()
  #           )
  #         ),
  #         column(
  #           4,
  #           actionButton(
  #             inputId = "wqCatchFetchData",
  #             label = "Plot"
  #           )
  #         )
  #       ),
  #       plotOutput("wqCatchPlot", height = "400px"),
  #       uiOutput("wqCatchDateSliderUI")
  #     )
  #   )
  # } else {
  #   print('no tab')
  # }

  # Update slider from calendar date inputs
  # output$wqCatchDateSlider <- renderUI({
  #   plotSlider(
  #     inputID = "wqCatchDateSlider",
  #     minDate = input$wqCatchDateRange[1],
  #     maxDate = input$wqCatchDateRange[2]
  #   )
  # })
  output$wqCatchDateSlider <- renderUI({
    plotSlider(
      inputID = "wqCatchDateSlider",
      minDate = input$wqCatchDateFrom,
      maxDate = input$wqCatchDateTo
    )
  })

  shinyjs::hide("wqCatchDateSliderBox")


  #this part doesn't work
  
  highlightCatch <- function(Catch_shp, Site_Code) {
    # Check if a site point was clicked (assuming site_data contains unique identifiers)
    clicked_site <- Catchment_data[Catchment_data$Site_Code == Site_Code, ]
    
    # Find the suburb ID associated with the clicked site
    catch_id <- clicked_site$CATCH_NAME
    
    # Highlight the corresponding suburb in the suburb_data
    Catch_shp$highlighted <- ifelse(Catch_shp$CATCH_NAME == catch_id, TRUE, FALSE)
    
    return(Catch_shp) 
  }
  
  observeEvent(input$map_click, {
    clicked_site <- input$map_click
    print(input$map_click)
    clicked_site_id <- clicked_site$Site_Code
    Catch_shp <- highlightCatch(Catchbound, clicked_site_id)
    # Update the map to reflect the changes
    leafletProxy("wqMap") %>%
      clearShapes() %>%
      addPolygons(data = Catch_shp, color = "yellow", fillOpacity = 0.5)
  })
  
  
  
  
  # ###get sensor Code Catchment (AS version)
  # selectedCatchsensorID <- reactive({
  #   req(isolate(input$wqCatchSites))
  #   req(isolate(input$wqCatchvariable))
  #     
  #     tmp<-sensorslist %>%  ### this is a Rda object stored in #the package, call it within the code I use it in! in #the top comment section
  #     dplyr::filter(Site %in% input$wqCatchSites & s_graph_value%in%input$wqCatchvariable)%>% ## filter the data based on the user selection of both site and variable
  #     dplyr::select(s_table_name)
  #   
  #   tmp
  #   
  # })
  # 
  
  ###get sensor Code Catchment
  selectedCatchsensorID <- reactive({
    req(isolate(input$wqCatchSites))
    req(isolate(input$wqCatchvariable))

    selectedCatchsites <- input$wqCatchSites

    tmp <- sensorslist %>%  ### this is a Rda object stored in #the package, call it within the code I use it in! in #the top comment section
           dplyr::filter(Site %in% selectedCatchsites & s_graph_value %in% input$wqCatchvariable) %>% ## filter the data based on the user selection of both site and variable
           dplyr::select(s_table_name)

    tmp <- tmp[['s_table_name']]  #convert dataframe to vector
    tmp
    
  })
    
  # Highlight selected sites on the map
  observe({
    selectedCatchsites <- input$wqCatchSites
    selectedCatchdf <- wqcatchMapStations[wqcatchMapStations$name %in% selectedCatchsites, ]
    sensorMapMarkerHighlight_wq(mapID = "wqMap", 
                                subGroupData = wqcatchMapStations, 
                                selectedData = selectedCatchdf, 
                                hlcolour = "yellow")
    # leafletProxy("wqMap") %>%
    #   clearMarkers() %>%
    #   leaflet::addCircleMarkers(
    #     lng = as.numeric(wqcatchMapStations[["lon"]]), 
    #     lat = as.numeric(wqcatchMapStations[["lat"]]), 
    #     color = "white", 
    #     radius = 5, 
    #     weight = 2, 
    #     fillColor = wqcatchMapStations[["colour"]], 
    #     opacity = 1, 
    #     fillOpacity = 1,
    #     # popup = paste0("<b>Station Name: </b>",wqcatchMapStations[["name"]],
    #     #                "<br><b>Agency: </b>", wqcatchMapStations[["source"]]))
    #     popup = paste0("<b>", paste0("<img src=' ",wqcatchMapStations[["URL"]]," ' width='100px' height='100px'>"),"</b></br>",
    #                    paste0("<b>Name: </b>", wqcatchMapStations[["fullname"]] ),"</b></br>",
    #                    paste0("<b>Site code: </b>", wqcatchMapStations[["name"]] ),"</b></br>",
    #                    paste0("<b>Site. Ref: </b>", wqcatchMapStations[["id"]]),
    #                    "</b></br>"))%>%
    #   # leaflet::addLayersControl(baseGroups = c("Ecological Managment Zones","Elevation","All off"), 
    #   #                           #overlayGroups = c("Estuary sites","Catchment sites"),position="topright",
    #   #                           options = leaflet::layersControlOptions(collapsed = FALSE)
    #   #) %>%
    # sensorMapMarkers_wq(
    #   mapID = "wqMap",
    #   subGroupData = wqcatchMapStations
    # ) %>%
      # 
      # addCircleMarkers(
      #   data = selectedCatchdf,
      #   lng = as.numeric(selectedCatchdf[["lon"]]),
      #   lat = as.numeric(selectedCatchdf[["lat"]]),
      #   #label = ~name,
      #   radius = 5,
      #   stroke = TRUE,
      #   color = "yellow",
      #   weight = 10,
      #   fillColor = selectedCatchdf[["colour"]],
      #   fillOpacity = 1,
      #   popup = paste0("<b>", paste0("<img src=' ",selectedCatchdf[["URL"]]," ' width='100px' height='100px'>"),"</b></br>",
      #                  paste0("<b>Name: </b>", selectedCatchdf[["fullname"]] ),"</b></br>",
      #                  paste0("<b>Site code: </b>", selectedCatchdf[["name"]] ),"</b></br>",
      #                  paste0("<b>Site. Ref: </b>", selectedCatchdf[["id"]]),
      #                  "</b></br>")
      # )
  })

  # On button click, fetch sensor data from SCEVO and graph for catchment - AS version
  
  observeEvent(input$wqCatchFetchData,{
    if(isTRUE(databaseEnable))
    {
      
      #print(selectedCatchsensorID())
      
    wqCatchData <- databaseConnect(sensorCodes = selectedCatchsensorID())
    plotData_y = "st_value_1_new"
    }else{
    # wqCatchData <- read.csv("www/FlowGaugesEllenBrookDS.csv")
    # wqCatchData$datetime <- as.POSIXct(wqCatchData$datetime,format="%d/%m/%Y")
    wqCatchData <- awss3Connect_sensorcode(sensorCodes = selectedCatchsensorID())
    plotData_y = "Data"
    }
    
    # Generate line graph from fetched data and diplay between slider dates
     output$wqCatchPlot <- plotly::renderPlotly({
    #output$wqCatchPlot <- renderPlot({
    
      wqCatchData <- dplyr::filter(
        wqCatchData,
        datetime >= as.POSIXct(input$wqCatchDateSlider[1]),
        datetime <= as.POSIXct(input$wqCatchDateSlider[2])
      )

      # Plot the graph
      library(plyr)
      
      wqCatchData <- join(wqCatchData,sensorslist,by="s_table_name")
      wqCatchData$Site <- as.factor(wqCatchData$Site)
      #wqCatchData$st_sensor_code <- as.factor(wqCatchData$st_sensor_code)
      plotPointMulti(
        plotData = wqCatchData,
        plotDataX = "datetime",
        #plotDataY = "st_value_1_new",
        plotDataY = plotData_y,
        #plotDataGroup = "st_sensor_code",
        plotDataGroup = "Site",
        plotLabelX = "Date",
        plotLabelY = paste0(isolate(input$wqCatchvariable))
        #plotDataColours = "#a6d854"
      )
      
    })
  })
   

    

  # # On button click, fetch sensor data from SCEVO and graph Gile's framework
  # observeEvent(input$wqCatchFetchData,{
  #   shinyjs::show("wqCatchDateSliderBox", anim = TRUE, animType = "fade")
  #   #hydroFlowData <- databaseConnect(sensorCodes = input$hydroFlowSiteCheckBox)
  #   wqCatchData <- read.csv("C:/Users/00101765/AED Dropbox/Sherry Zhai/SCEVO/dashboard - Copy/FlowGaugesEllenBrookDS.csv")
  #   wqCatchData$datetime <- as.POSIXct(wqCatchData$datetime,format="%d/%m/%Y")
  # 
  #   # Get line plot colours for selected sensors
  #   wqCatchDataColours <- sensorColours(
  #     allCodes = modWqCatchCode,
  #     allColours = modWqCatchColour,
  #     selectedCodes = input$wqCatchSiteCheckBox
  #   )
  # 
  #   # Generate line graph from fetched data and display between slider dates
  #   output$wqCatchPlot <- renderPlot({
  #     wqCatchData <- dplyr::filter(
  #       wqCatchData,
  #       datetime >= as.POSIXct(input$wqCatchDateSlider[1]),
  #       datetime <= as.POSIXct(input$wqCatchDateSlider[2])
  # 
  #     )
  # 
  #     # Plot the graph
  #     plotLine(
  #       plotData = wqCatchData,
  #       plotDataX = "datetime",
  #       plotDataY = "st_value_1",
  #       plotDataGroup = "st_sensor_code",
  #       plotLabelX = "Date",
  #       plotLabelY = "Unit",
  #       plotDataColours = wqCatchDataColours
  #     )
  #   })
  # })


##### WQ - CATCHEMNT SUMMARY ####
  observeEvent(input$wqCatchSumFetchData,{
    #Rawdata <- read.csv(file = 'www/DBCA_data_export/DBCA_data_export_2023-07-19_1615.csv', check.names = FALSE)
    #Rawdata <- awss3Connect(filename = 'data-warehouse/dbca/wiski/DBCA_data_export_2023-07-19_1615.csv')
    
      #output$catchsumPlot <- plotly::renderPlotly({
      output$catchsumPlot <- renderPlot({
      #plotly::ggplotly(
       plotCatchSum(
        plotDataSite = input$select_site_catch,
        plotDataYear = input$select_year_catch,
        plotDataVar = input$select_vars_catch
      #)
      )
    })
  })
##### WQ - ESTUARY ####
  
  # if(isTRUE(modWqEstEnable))
  # {
  #   insertTab(
  #     inputId = "estTabset",
  #     select = TRUE,
  #     tabPanel(
  #       title = "Estuary",
  #       tags$summary(HTML("Select sites and date range:")),
  #       checkboxGroupInput(
  #         inputId = "wqEstSiteCheckBox",
  #         label = NULL,
  #         inline = TRUE,
  #         choiceNames = modWqEstName,
  #         choiceValues =  modWqEstCode
  #       ),
  #       fluidRow(
  #         column(
  #           8,
  #           dateRangeInput(
  #             inputId = "wqEstDateRange",
  #             label = NULL,
  #             start = Sys.Date()-7,
  #             end = Sys.Date()
  #           )
  #         ),
  #         column(
  #           4,
  #           actionButton(
  #             inputId = "wqEstFetchData",
  #             label = "Plot"
  #           )
  #         )
  #       ),
  #       plotOutput("wqEstPlot", height = "400px"),
  #       uiOutput("wqEstDateSliderUI")
  #     )
  #   )
  # } else {
  #   print('no tab')
  # }
  
  output$wqEstDateSlider <- renderUI({
    plotSlider(
      inputID = "wqEstDateSlider",
      minDate = input$wqEstDateFrom,
      maxDate = input$wqEstDateTo
    )
  })
  
  shinyjs::hide("wqEstDateSliderBox")

  ###get sensor Code Estuary surface  (AS version)
  selectedEstsensorID_s <- reactive({
    req(isolate(input$wqEstSites))
    req(isolate(input$wqEstvariable))

    ESTsite<-paste0(input$wqEstSites,"_s")

    tmp1<-sensorslist_est %>%  ### this is a Rda object stored in #the package, call it within the code I use it in! in #the top comment section
      dplyr::filter(Site %in% ESTsite & s_graph_value%in%input$wqEstvariable)%>% ## filter the data based on the user selection of both site and variable
      dplyr::select(s_table_name)

    tmp1

  })

  ###get sensor Code Estuary bottom
  selectedEstsensorID_b <- reactive({
    req(isolate(input$wqEstSites))
    req(isolate(input$wqEstvariable))

    ESTsite2<-paste0(input$wqEstSites,"_b")

    tmp2<-sensorslist_est %>%  ### this is a Rda object stored in #the package, call it within the code I use it in! in #the top comment section
      dplyr::filter(Site %in% ESTsite2 & s_graph_value%in%input$wqEstvariable)%>% ## filter the data based on the user selection of both site and variable
      dplyr::select(s_table_name)

    tmp2

  })
  
  # Highlight selected sites on the map
  observe({
    selectedEstsites <- input$wqEstSites
    selectedEstdf <- wqestMapStations[wqestMapStations$name %in% selectedEstsites, ]
    sensorMapMarkerHighlight_wq(mapID = "wqMap", 
                                subGroupData = wqestMapStations, 
                                selectedData = selectedEstdf, 
                                hlcolour = "yellow")
    # leafletProxy("wqMap") %>%
    #   clearMarkers() %>%
    #   leaflet::addCircleMarkers(
    #     lng = as.numeric(wqestMapStations[["lon"]]), 
    #     lat = as.numeric(wqestMapStations[["lat"]]), 
    #     color = "white", 
    #     radius = 5, 
    #     weight = 2, 
    #     fillColor = wqestMapStations[["colour"]], 
    #     opacity = 1, 
    #     fillOpacity = 1,
    #     # popup = paste0("<b>Station Name: </b>",wqcatchMapStations[["name"]],
    #     #                "<br><b>Agency: </b>", wqcatchMapStations[["source"]]))
    #     popup = paste0("<b>", paste0("<img src=' ",wqestMapStations[["URL"]]," ' width='100px' height='100px'>"),"</b></br>",
    #                    paste0("<b>Name: </b>", wqestMapStations[["fullname"]] ),"</b></br>",
    #                    paste0("<b>Site code: </b>", wqestMapStations[["name"]] ),"</b></br>",
    #                    paste0("<b>Site. Ref: </b>", wqestMapStations[["id"]]),
    #                    "</b></br>"))%>%
    #   # leaflet::addLayersControl(baseGroups = c("Ecological Managment Zones","Elevation","All off"), 
    #   #                           #overlayGroups = c("Estuary sites","Catchment sites"),position="topright",
    #   #                           options = leaflet::layersControlOptions(collapsed = FALSE)
    #   #) %>%
    #   addCircleMarkers(
    #     data = selectedEstdf,
    #     lng = as.numeric(selectedEstdf[["lon"]]),
    #     lat = as.numeric(selectedEstdf[["lat"]]),
    #     #label = ~name,
    #     radius = 5,
    #     stroke = TRUE,
    #     color = "yellow",
    #     weight = 10,
    #     fillColor = selectedEstdf[["colour"]],
    #     fillOpacity = 1,
    #     popup = paste0("<b>", paste0("<img src=' ",selectedEstdf[["URL"]]," ' width='100px' height='100px'>"),"</b></br>",
    #                    paste0("<b>Name: </b>", selectedEstdf[["fullname"]] ),"</b></br>",
    #                    paste0("<b>Site code: </b>", selectedEstdf[["name"]] ),"</b></br>",
    #                    paste0("<b>Site. Ref: </b>", selectedEstdf[["id"]]),
    #                    "</b></br>")
    #   )
  })
  
  
  # On button click, fetch sensor data from SCEVO and graph for catchment 
  
  observeEvent(input$wqEstFetchData,{
    if(isTRUE(databaseEnable))
    {
    wqEstData_s<- databaseConnect(sensorCodes = selectedEstsensorID_s())
    wqEstData_b<- databaseConnect(sensorCodes = selectedEstsensorID_b())
    wqEstData <- rbind(wqEstData_s,wqEstData_b)
    plotData_y = "st_value_1_new"
    }else{
      wqEstData_s<- awss3Connect_sensorcode(sensorCodes = selectedEstsensorID_s())
      wqEstData_b<- awss3Connect_sensorcode(sensorCodes = selectedEstsensorID_b())
      wqEstData <- rbind(wqEstData_s,wqEstData_b)
      plotData_y = "Data"
    # wqEstData_s <- read.csv("www/FlowGaugesEllenBrookDS.csv")
    # wqEstData_s$datetime <- as.POSIXct(wqEstData_s$datetime,format="%d/%m/%Y")
    # wqEstData_b <- read.csv("www/FlowGaugesEllenBrookDS.csv")
    # wqEstData_b$datetime <- as.POSIXct(wqEstData_b$datetime,format="%d/%m/%Y")
    # wqEstData <- rbind(wqEstData_s,wqEstData_b)
    }
    
    # Generate line graph from fetched data and diplay between slider dates
    output$wqEstPlot <- plotly::renderPlotly({
      wqEstData <- dplyr::filter(
        wqEstData,
        datetime >= as.POSIXct(input$wqEstDateSlider[1]),
        datetime <= as.POSIXct(input$wqEstDateSlider[2])
      )
      
      library(plyr)
      wqEstData <- join(wqEstData,sensorslist_est,by="s_table_name")
      wqEstData$Site <- as.factor(wqEstData$Site)
      
    plotPointMulti(
      plotData = wqEstData,
      plotDataX = "datetime",
      plotDataY = plotData_y,
      plotDataGroup = "Site",
      plotLabelX = "Date",
      plotLabelY = paste0(isolate(input$wqEstvariable))
      #plotDataColours = "#8da0cb"
    )
    })
    
    # output$wqEstPlot_s <- plotly::renderPlotly({
    #   wqEstData_s <- dplyr::filter(
    #     wqEstData_s,
    #     datetime >= as.POSIXct(input$wqEstDateSlider[1]),
    #     datetime <= as.POSIXct(input$wqEstDateSlider[2])
    #   )
    #   # Plot the graph
    #   plotPoint(
    #     plotData = wqEstData_s,
    #     plotDataX = "datetime",
    #     plotDataY = "st_value_1_new",
    #     plotDataGroup = "st_sensor_code",
    #     plotLabelX = "Date",
    #     plotLabelY = paste0(isolate(input$Estvariable)),
    #     plotDataColours = "#8da0cb"
    #   )
    # })
    #  
    # output$wqEstPlot_b <- plotly::renderPlotly({
    # wqEstData_b <- dplyr::filter(
    #   wqEstData_b,
    #   datetime >= as.POSIXct(input$wqEstDateSlider[1]),
    #   datetime <= as.POSIXct(input$wqEstDateSlider[2])
    # )
    # plotPoint(
    #   plotData = wqEstData_b,
    #   plotDataX = "datetime",
    #   plotDataY = "st_value_1_new",
    #   plotDataGroup = "st_sensor_code",
    #   plotLabelX = "Date",
    #   plotLabelY = paste0(isolate(input$Estvariable)),
    #   plotDataColours = "#8da0cb"
    # )
    # })
  })

        
##### WQ - ESTUARY SUMMARY ####    
 
  observeEvent(input$wqEstSumFetchData,{
    #dfimport <- read.csv(file = 'www/DBCA_data_export/DBCA_data_export_2023-07-19_1615.csv', check.names = FALSE)
    #dfimport <- awss3Connect(filename = 'data-warehouse/dbca/wiski/DBCA_data_export_2023-07-19_1615.csv')
    
    # Generate line graph from fetched data and diplay between slider dates
    #output$estsumPlot <- plotly::renderPlotly({
     output$estsumPlot <- renderPlot({
      plotEstSum(
        plotDataRegion = input$select_region,
        plotDataYear = input$select_year,
        plotDataVar = input$select_vars
      )
    })
   })
  
  
# ##### WQ - PROFILE ####
# 
#   profdir <- "www/Profile_plotting"
#   #regions <- c("Swan","Canning")
#   #subdir <- list.dirs(profdatdir, full.names = TRUE, recursive=FALSE)
# 
# 
#   #update week depending on region selected
#   observeEvent(input$select_region,{
#     selected_option <- input$select_region
#     choices_list <- list(
#       "Swan"    = list.dirs(file.path(profdir,"Swan"),
#                             full.names = FALSE, recursive=FALSE),
#       "Canning" = list.dirs(file.path(profdir,"Canning"),
#                             full.names = FALSE, recursive=FALSE)
#     )
#     updateSelectInput(session,"select_week",
#                       choices = choices_list[[selected_option]])
#   })
# 
#   #on button click, plot profiles. If plot already exists in folder, just render image
#   observeEvent(input$wqProfFetchData,{
#       profdat <- file.path(profdir,input$select_region, input$select_week)
#       if(!file.exists(file.path(profdat, 'plots',
#                                 paste0(tolower(input$select_region),'_',input$select_week,'_surfer.png')
#                                 ))){
#         library(rivRmon)
#         if(input$select_region=="Swan"){
#         profPlot <- swan_surfR_alt(profdat,
#                                    ovit = "green",
#                                    ocav = "green")
#         }else{
#           if(input$select_region=="Canning"){
#           profPlot <- canning_surfR(profdat,
#                                     obac = "green",
#                                     onic = "green",
#                                     SHELL = FALSE)
#           }
#         }
#       }
# 
#   # output$profPlot <- renderImage({
#   #     filename <- normalizePath(file.path(profdat,'plots',
#   #                                         paste0(tolower(input$select_region),'_',input$select_week,'_surfer.png')))
#   #                                             # Return a list containing the filename
#   #                                             list(src = filename,
#   #                                                  width = '1100px',
#   #                                                  height = '700px')
#   #   }, deleteFile = FALSE)
# 
#       library(slickR)
#   output$profPlot <- renderSlickR({
#     filename <- list.files("www/Profile_plotting/Swan/2023-07-03/plots/",pattern=".png",full.names = T
#                                                             )
#     slickR(filename)
#     })
#   })


  
##### WQ - PROFILE - WISKI DATA ####


  #update week depending on region selected
  observeEvent(input$select_month_profile,{
    selected_option <- input$select_region_profile
    swanwks <- rawwiski %>%
      dplyr::filter(`Collect Year` %in% input$select_year_profile &
                    `Collect Month` %in% input$select_month_profile &
                    `Program Site Ref` %in% swansites &
                    `Collection Method` %in% 'Insitu' &
                    `Data Category` %in% 'Instrument log') %>%
      dplyr::select(`Collect Date`)

    cannwks <- rawwiski %>%
      dplyr::filter(`Collect Year` %in% input$select_year_profile &
                      `Collect Month` %in% input$select_month_profile &
                      `Program Site Ref` %in% cannsites &
                      `Collection Method` %in% 'Insitu' &
                      `Data Category` %in% 'Instrument log') %>%
      dplyr::select(`Collect Date`)

    choices_list <- list(
      "Swan"    = unique(swanwks),
      "Canning" = unique(cannwks)
    )
    updateSelectInput(session,"select_week_profile",
                      choices = choices_list[[selected_option]])
  })

  observeEvent(input$wqProfFetchData,{
    if(input$select_region_profile =="Swan"){
      plots <- plotProfile_Swan(
        plotDataWeek = input$select_week_profile,
        StatusOvit = "white",
        StatusOcav = "white")
    }else{
      if(input$select_region_profile =="Canning"){
      plots <- plotProfile_Canning(                  
          plotDataWeek = input$select_week_profile,
          StatusObac = "white",
          StatusOnic = "white")
      }
    }
    
    
    # output$profPlotWiski1 <- plotly::renderPlotly({ggplotly(plots[[1]])})
    # output$profPlotWiski2 <- plotly::renderPlotly({ggplotly(plots[[2]])})
    # output$profPlotWiski3 <- plotly::renderPlotly({ggplotly(plots[[3]])})
    # output$profPlotWiski4 <- plotly::renderPlotly({ggplotly(plots[[4]])})
    if(input$select_region_profile =="Swan"){
    output$profPlotWiski1 <- renderPlot({plots[[1]]})
    output$profPlotWiski2 <- renderPlot({plots[[2]]})
    output$profPlotWiski3 <- renderPlot({plots[[3]]})
    output$profPlotWiski4 <- renderPlot({plots[[4]]})
    output$profPlotWiski1z <- renderPlot({plots[[5]]})
    output$profPlotWiski2z <- renderPlot({plots[[6]]})
    output$profPlotWiski3z <- renderPlot({plots[[7]]})
    output$profPlotWiski4z <- renderPlot({plots[[8]]})
    }else{
      if(input$select_region_profile =="Canning"){
        output$profPlotWiski1 <- renderPlot({plots[[1]]})
        output$profPlotWiski2 <- renderPlot({plots[[2]]})
        output$profPlotWiski3 <- renderPlot({plots[[3]]})
        output$profPlotWiski4 <- renderPlot({plots[[4]]})
      }
    }
  })


##### WQ - PROFILE - HISTORIC ####

  profdir <- "www/Profile_plotting/historic/"
  #profdir <- "data-warehouse/dbca/wiski/profileplots/"
  
  #update week depending on region selected
    observeEvent(input$select_region_profile_hist,{
      selected_option <- input$select_region_profile_hist
      choices_list <- list(
        "Swan"    = sort(list.dirs(file.path(profdir,"Swan"),
                              full.names = FALSE, recursive=FALSE),decreasing = T),
        "Canning" = sort(list.dirs(file.path(profdir,"Canning"),
                              full.names = FALSE, recursive=FALSE),decreasing = T)
      )
      updateSelectInput(session,"select_year_profile_hist",
                        choices = choices_list[[selected_option]])
      
      
    })

    #on button click, render stored image
    observeEvent(input$wqProfHistFetchData,{
      
      #invalidateLater(500, session)
      
        proffolder <- file.path(profdir,input$select_region_profile_hist, input$select_year_profile_hist)
        filenames <- list.files(proffolder,pattern=".png",full.names = T)
        
        #print(proffolder)
        
        # proffolder <- paste0(profdir,input$select_region_profile_hist,"/", input$select_year_profile_hist,"/")
        # filenames <- awss3Listfiles(prefix = proffolder)
        # 
  
    #library(slickR)
        # observe({
        #   # Re-execute this reactive expression after 1000 milliseconds
        #   invalidateLater(1000, session)
        #   
        #   # Do something each time this is invalidated.
        #   # The isolate() makes this observer _not_ get invalidated and re-executed
        #   # when input$n changes.
        #   print("test")
        # })    
        
        #insertUI("#placeholder","afterEnd",ui = slickROutput("profPlotHist"))
    
    output$profPlotHist <- renderSlickR({
      
      #print(filenames)
      
      slickR(filenames)+
        settings(dots = TRUE,
                 customPaging = htmlwidgets::JS("function(slick,index) {
                            return '<a>'+(index+1)+'</a>';
                       }"))
        
      })
    #shinyjs::show("profPlotHist")
    })
    
    # observeEvent(input$wqProfHistClearData, {
    #   #shinyjs::hide("profPlotHist")
    #   # output$profPlotHist <- renderUI({
    #   #   NULL
    #   removeUI("#profPlotHist")
    #   #})
    # })
    
##### WQ - MOORING ####  
  
  output$wqMooringDateSlider <- renderUI({
    plotSlider(
      inputID = "wqMooringDateSlider",
      minDate = input$wqMooringDateFrom,
      maxDate = input$wqMooringDateTo
    )
  })
  
  shinyjs::hide("wqMooringDateSliderBox")
  
  ###get sensor Code 
    
    selectedMooringsensorID <- reactive({
      req(isolate(input$wqMooringSites))
      req(isolate(input$wqMooringVariable))
      
      selectedMooringsites <- input$wqMooringSites
      
      tmp_moor <- sensorslist_hydromet %>%  
        dplyr::filter(Site %in% selectedMooringsites & s_graph_value %in% input$wqMooringVariable) %>% ## filter the data based on the user selection of both site and variable
        dplyr::select(s_table_name)
      
      tmp_moor <- tmp_moor[['s_table_name']]  #convert dataframe to vector
      tmp_moor
      
    })
    
    observe({
    selectedMooringsites <- input$wqMooringSites
    selectedMoordf <- wqmoorMapStations[wqmoorMapStations$name %in% selectedMooringsites, ]
    sensorMapMarkerHighlight_methydro (mapID = "wqMap", 
                                       subGroupData = wqmoorMapStations, 
                                       selectedData = selectedMoordf, 
                                       hlcolour = "yellow")
    })
    

    observeEvent(input$wqMooringFetchData,{
      if(isTRUE(databaseEnable))
      {
        wqMooringData <- databaseConnect(sensorCodes = selectedMooringsensorID())
        plotData_y = "st_value_1_new"
      }else{
        wqMooringData <- awss3Connect_sensorcode(sensorCodes = selectedMooringsensorID())
        plotData_y = "Data"
      }
      
      # Generate line graph from fetched data and diplay between slider dates
      output$wqMooringPlot <- plotly::renderPlotly({
        #output$wqMooringPlot <- renderPlot({
        
        wqMooringData <- dplyr::filter(
          wqMooringData,
          datetime >= as.POSIXct(input$wqMooringDateSlider[1]),
          datetime <= as.POSIXct(input$wqMooringDateSlider[2])
        )
        
        # Plot the graph
        library(plyr)
        
        wqMooringData <- join(wqMooringData,sensorslist_hydromet,by="s_table_name")
        wqMooringData$Site <- as.factor(wqMooringData$Site)
        #wqMooringData$st_sensor_code <- as.factor(wqMooringData$st_sensor_code)
        plotLineMulti(
          plotData = wqMooringData,
          plotDataX = "datetime",
          #plotDataY = "st_value_1_new",
          plotDataY = plotData_y,
          #plotDataGroup = "st_sensor_code",
          plotDataGroup = "Site",
          plotLabelX = "Date",
          plotLabelY = paste0(isolate(input$wqMooringVariable))
          #plotDataColours = "#a6d854"
        )
        
      })
    })

  
  # # On button click, fetch sensor data from SCEVO and graph
  # observeEvent(input$wqMooringFetchData,{ 
  #   
  #   shinyjs::show("wqMooringDateSliderBox", anim = TRUE, animType = "fade")
  #   if(isTRUE(databaseEnable))
  #   {  
  #   wqMooringData <- databaseConnect(sensorCodes = input$wqMooringSiteCheckBox)
  #   }else{
  #   wqMooringData  <- read.csv("www/FlowGaugesEllenBrookDS.csv")
  #   wqMooringData$datetime <- as.POSIXct(wqMooringData$datetime,format="%d/%m/%Y")
  #   }
  #   
  #   # Get line plot colours for selected sensors
  #   wqMooringDataColours <- activeSensorColours(
  #     checkBoxInputs = input$wqMooringSiteCheckBox,
  #     sensorInfo = wqSensorInfo
  #   )
  #  
  #   
  #   # Generate line graph from fetched data and display between slider dates
  #   output$wqMooringPlot <- renderPlot({
  #     
  #     wqMooringData <- dplyr::filter(
  #       wqMooringData,
  #       datetime >= as.POSIXct(input$wqMooringDateSlider[1]),
  #       datetime <= as.POSIXct(input$wqMooringDateSlider[2])
  #     )
  #     
  #     # Plot the graph    
  #     plotLine(
  #       plotData = wqMooringData,
  #       plotDataX = "datetime",
  #       plotDataY = "st_value_1_new",
  #       plotDataGroup = "st_sensor_code",
  #       plotLabelX = "Date",
  #       plotLabelY = "Dissolved Oxygen (mg/L)",
  #       plotDataColours = wqMooringDataColours
  #       #plotDataColours = "red"
  #     )
  #   })
  # })  
  # 
  
##### WQ - ABOUT ####
    
    output$wqAbout <- renderPrint({
      "Catchment, estuary and profile data were sourced from Department of Biodiversity, Conservation and Attractions (DBCA) and are quality-checked.
       Mooring data were sourced from The University of Western Australia (UWA) AquaticEcodynamics (AED) group and are not quality-checked."
    }) 
  
#### OXYGENATION PLANT ####
  
  #### OXYGENATION PLANT - Dissolved Oxygen ####
  
  Oxy_data<-read.csv("www/Oxy_sites.csv")
  
  wqDOMapStations <- data.frame(
    name = Oxy_data$Project.Site.Reference,
    colour = "#fc8d62",
    source = "unknown",
    lat = Oxy_data$Latitude,
    lon = Oxy_data$Longitude,
    #URL = Oxy_data$URL,
    #id = Oxy_data$AWRC_No,
    fullname = Oxy_data$Site.Name
  )
  
  # Generates blank waterquality tab web-map
  output$oxyplantMap <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>%
      leaflet::setView(115.8613, -31.9523, 10) %>% 
      leaflet::addCircleMarkers(
        lng = as.numeric(wqDOMapStations[["lon"]]),
        lat = as.numeric(wqDOMapStations[["lat"]]),
        color = "white", 
        radius = 5, 
        weight = 2, 
        fillColor = wqDOMapStations[["colour"]], 
        opacity = 1, 
        fillOpacity = 1,
        popup = paste0(
                       paste0("<b>Name: </b>", wqDOMapStations[["fullname"]] ),"</b></br>",
                       # paste0("<b>Site code: </b>", wqDOMapStations[["name"]] ),"</b></br>",
                       # paste0("<b>Site. Ref: </b>", wqDOMapStations[["id"]]),
                       "</b></br>"))
      
  })
  
  # Update slider from calendar date inputs
  output$wqDODateSlider <- renderUI({
    plotSlider(
      inputID = "wqDODateSlider",
      minDate = input$wqDODateFrom,
      maxDate = input$wqDODateTo
    )
  })
  
  shinyjs::hide("wqDODateSliderBox")
  
  selectedDOsensorID <- reactive({
    req(isolate(input$wqDOSites))
    req(isolate(input$wqDOvariable))
    
    selectedDOsites <- input$wqDOSites
    
    tmp_oxy <- sensorslist_oxy %>%  
      dplyr::filter(Site %in% selectedDOsites & s_graph_value %in% input$wqDOvariable) %>% ## filter the data based on the user selection of both site and variable
      dplyr::select(s_table_name)
    
    tmp_oxy <- tmp_oxy[['s_table_name']]  #convert dataframe to vector
    tmp_oxy
    
  })
  
  # Highlight selected sites on the map
  
  observe({
    selectedDOsites <- input$wqDOSites
    selectedDOdf <- wqDOMapStations[wqDOMapStations$name %in% selectedDOsites, ]
    leafletProxy("oxyplantMap") %>%
      clearMarkers() %>%
      leaflet::addCircleMarkers(
        lng = as.numeric(wqDOMapStations[["lon"]]),
        lat = as.numeric(wqDOMapStations[["lat"]]),
        color = "white",
        radius = 5,
        weight = 2,
        fillColor = wqDOMapStations[["colour"]],
        opacity = 1,
        fillOpacity = 1,
        popup = paste0(
          paste0("<b>Name: </b>", wqDOMapStations[["fullname"]] ),"</b></br>",
          # paste0("<b>Site code: </b>", wqDOMapStations[["name"]] ),"</b></br>",
          # paste0("<b>Site. Ref: </b>", wqDOMapStations[["id"]]),
          "</b></br>"))%>%

      addCircleMarkers(
        data = selectedDOdf,
        lng = as.numeric(selectedDOdf[["lon"]]),
        lat = as.numeric(selectedDOdf[["lat"]]),
        #label = ~name,
        radius = 5,
        stroke = TRUE,
        color = "yellow",
        weight = 10,
        fillColor = selectedDOdf[["colour"]],
        fillOpacity = 1,
        popup = paste0(
          paste0("<b>Name: </b>", wqDOMapStations[["fullname"]] ),"</b></br>",
          # paste0("<b>Site code: </b>", wqDOMapStations[["name"]] ),"</b></br>",
          # paste0("<b>Site. Ref: </b>", wqDOMapStations[["id"]]),
          "</b></br>")
      )
  })
  
  # On button click, fetch sensor data and graph 
  
  observeEvent(input$wqDOFetchData,{
    if(isTRUE(databaseEnable))
    {
      
      #print(selectedCatchsensorID())
      
      wqDOData <- databaseConnect(sensorCodes = selectedDOsensorID())
      plotData_y = "st_value_1_new"
    }else{
      # wqDOData <- read.csv("www/FlowGaugesEllenBrookDS.csv") #dummy
      # wqDOData$datetime <- as.POSIXct(wqDOData$datetime,format="%d/%m/%Y")
      # wqDOData$s_table_name <- 'sensor_repository_81653' #dummy 
      wqDOData <- awss3Connect_sensorcode(sensorCodes = selectedDOsensorID())
      plotData_y = "Data"
    }
    
    # Generate line graph from fetched data and diplay between slider dates
    output$wqDOPlot <- plotly::renderPlotly({
      #output$wqDOPlot <- renderPlot({
      
      wqDOData <- dplyr::filter(
        wqDOData,
        datetime >= as.POSIXct(input$wqDODateSlider[1]),
        datetime <= as.POSIXct(input$wqDODateSlider[2])
        # datetime >= as.POSIXct(input$wqDODateFrom),
        # datetime <= as.POSIXct(input$wqDODateTo)
      )
      
      # Plot the graph
      library(plyr)
      
      wqDOData <- join(wqDOData,sensorslist_oxy,by="s_table_name")
      wqDOData$Site <- as.factor(wqDOData$Site)
      #wqDOData$st_sensor_code <- as.factor(wqDOData$st_sensor_code)
      plotLineMulti(
        plotData = wqDOData,
        plotDataX = "datetime",
        #plotDataY = "st_value_1_new",
        plotDataY = plotData_y,
        #plotDataGroup = "st_sensor_code",
        plotDataGroup = "Site",
        plotLabelX = "Date",
        plotLabelY = paste0(isolate(input$wqDOvariable))
        #plotDataColours = "#a6d854"
      )
    }
  )
  })

  
  ##### OXYGENATION PLANT - ABOUT ####
  
  output$oxyAbout <- renderPrint({
    "Oxygenation plant data were sourced from Department of Biodiversity, Conservation and Attractions (DBCA) and are not quality-checked."
  }) 
#### OXYGENATION PLANT (OLD) #### 
  # #### OXYGENATION PLANT - Oxy concentration 
  # 
  # # Filter sensorInfo data to only water quality sensors
  # wqSensorInfo <- sensorInfo[sensorInfo[["group"]]=="wq",]
  # wqsubGroupData <- wqSensorInfo[wqSensorInfo[["subGroup"]]=="DO",]
  # wqsubGroupData <- wqsubGroupData[1:2,]
  # # Generates blank waterquality tab web-map
  # output$oxyplantMap <- leaflet::renderLeaflet({
  #   leaflet::leaflet() %>%
  #     leaflet::addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>%
  #     leaflet::setView(115.8613, -31.9523, 9) %>% 
  #     leaflet::addCircleMarkers(
  #       lng = wqsubGroupData[["lon"]], 
  #       lat = wqsubGroupData[["lat"]], 
  #       color = "white", 
  #       radius = 7, 
  #       weight = 2, 
  #       fillColor = wqsubGroupData[["colour"]], 
  #       opacity = 1, 
  #       fillOpacity = 1,
  #       popup = paste0("<b>Station Name: </b>",wqsubGroupData[["label"]], "<br><b>Agency: </b>", wqsubGroupData[["agency"]])
  #     )
  # })
  # 
  # # Update slider from calendar date inputs
  # output$wqDODateSlider <- renderUI({
  #   plotSlider(
  #     inputID = "wqDODateSlider",
  #     minDate = input$wqDODateRange[1],
  #     maxDate = input$wqDODateRange[2]
  #   )
  # })
  # 
  # shinyjs::hide("wqDODateSliderBox")
  # 
  # # On button click, fetch sensor data from SCEVO and graph
  # observeEvent(input$wqDOFetchData,{ 
  #   
  #   shinyjs::show("wqDODateSliderBox", anim = TRUE, animType = "fade")
  #   if(isTRUE(databaseEnable))
  #   {
  #   wqDOData <- databaseConnect(sensorCodes = input$wqDOSiteCheckBox) 
  #   }else{
  #   wqDOData  <- read.csv("www/FlowGaugesEllenBrookDS.csv")
  #   wqDOData$datetime <- as.POSIXct(wqDOData$datetime,format="%d/%m/%Y")
  #   }
  #   
  #   # Get line plot colours for selected sensors
  #   wqDODataColours <- activeSensorColours(
  #     checkBoxInputs = input$wqDOSiteCheckBox,
  #     sensorInfo = wqSensorInfo
  #   )
  #   
  #   # Generate line graph from fetched data and display between slider dates
  #   output$wqDOPlot <- renderPlot({
  #     
  #     wqDOData <- dplyr::filter(
  #       wqDOData,
  #       datetime >= as.POSIXct(input$wqDODateSlider[1]),
  #       datetime <= as.POSIXct(input$wqDODateSlider[2])
  #     )
  #     
  #     # Plot the graph    
  #     plotLine(
  #       plotData = wqDOData,
  #       plotDataX = "datetime",
  #       plotDataY = "st_value_1",
  #       plotDataGroup = "st_sensor_code",
  #       plotLabelX = "Date",
  #       plotLabelY = "Dissolved Oxygen (mg/L)",
  #       plotDataColours = wqDODataColours
  #     )
  #   })
  # })
  # 
  # #### OXYGENATION PLANT - Oxy saturation ####
  # 
  # # Update slider from calendar date inputs
  # output$wqDOSatDateSlider <- renderUI({
  #   plotSlider(
  #     inputID = "wqDOSatDateSlider",
  #     minDate = input$wqDOSatDateRange[1],
  #     maxDate = input$wqDOSatDateRange[2]
  #   )
  # })
  # 
  # shinyjs::hide("wqDOSatDateSliderBox")
  # 
  # # On button click, fetch sensor data from SCEVO and graph
  # observeEvent(input$wqDOSatFetchData,{ 
  #   
  #   shinyjs::show("wqDOSatDateSliderBox", anim = TRUE, animType = "fade")
  #   if(isTRUE(databaseEnable))
  #   {
  #   wqDOSatData <- databaseConnect(sensorCodes = input$wqDOSatSiteCheckBox) 
  #   }else{
  #   wqDOSatData  <- read.csv("www/FlowGaugesEllenBrookDS.csv")
  #   wqDOSatData$datetime <- as.POSIXct(wqDOSatData$datetime,format="%d/%m/%Y")
  #   }
  #   
  #   # Get line plot colours for selected sensors
  #   wqDOSatDataColours <- activeSensorColours(
  #     checkBoxInputs = input$wqDOSatSiteCheckBox,
  #     sensorInfo = wqSensorInfo
  #   )
  #   
  #   # Generate line graph from fetched data and display between slider dates
  #   output$wqDOSatPlot <- renderPlot({
  #     
  #     wqDOSatData <- dplyr::filter(
  #       wqDOSatData,
  #       datetime >= as.POSIXct(input$wqDOSatDateSlider[1]),
  #       datetime <= as.POSIXct(input$wqDOSatDateSlider[2])
  #     )
  #     
  #     # Plot the graph    
  #     plotLine(
  #       plotData = wqDOSatData,
  #       plotDataX = "datetime",
  #       plotDataY = "st_value_1",
  #       plotDataGroup = "st_sensor_code",
  #       plotLabelX = "Date",
  #       plotLabelY = "DO Saturation (%)",
  #       plotDataColours = wqDOSatDataColours
  #     )
  #   })
  # })
  # 
#### PHYTOPLANKTON ####
  
  modPhytoEnable <- as.logical(get_golem_config("enable", config = "mod_phyto"))
  
  print(paste0("modPhytoEnable: ", modPhytoEnable))
  
  #load EMZ map layer
  FS<- rgdal::readOGR("www/new_all.shp",layer = "new_all", GDAL1_integer64_policy = TRUE)
  PRO <- sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
  DAT6 <- sp::spTransform(FS,PRO)  ### this sets the shapefile to have the same coordinates as the map
  factpal <- leaflet::colorFactor('viridis', DAT6$EMZ,reverse = FALSE) 
  
  legend_labels <- c("ALERT", "High (>10 ug/L)", "Medium (4-10 ug/L)", "Low (<4 ug/L)", "Unknown")
  legend_colors <- c("darkred", "red", "orange", "lightblue", "gray")
  
  
  # Generates blank WQ tab web-map
  output$phytoMap <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>%
      leaflet::setView(115.8613, -31.9523, 12) %>%
    leaflet::addPolygons(data = DAT6, fill = FALSE, stroke = TRUE, color= ~factpal(EMZ), #fillOpacity = .8, 
                         group = "Ecological Managment Zones", label = ~River) %>%
    leaflet::addLegend(
      position = "topright",
      colors = legend_colors,
      labels = legend_labels,
      title = "Microalgae Activity")
  })
  
  algaekey <- data.frame(
    Level = c("Low","Medium","High","Alert"),
    Description = c("Low levels of microalgae with no likely visible impact",
                    "Possibility of discolouration and/or scum formation",
                    "High probability of discolouration and/or scum formation",
                    "Presence of species potentially harmful to aquatic life, OR human health requiring public advice"),
    `Chlorophyll a` = c("< 4 ug/L", "4 - 10 ug/L", "> 10 ug/L", "N/A")
  )
  
  output$keyTable <- renderTable({
    algaekey
  })
 
  # set up site df's in correct upriver order
  algaesites <- c("BLA", "ARM", "HEA", "NAR", "NIL", "STJ", "MAY", "RON", "KIN",
                  "SUC", "WMP", "MSB",  #swan
                  "SCB2", "SAL", "SHELL", "RIV", "CASMID", "KEN", "BAC", "NIC", "ELL") #canning
  # 
  # Estuary_data<- read.csv("www/SCESTUARY - Copy.csv")
  # algae_data <-  Estuary_data %>%
  #   dplyr::filter(Project.Site.Reference %in% algaesites)
  #   
  # wqalgaeMapStations <- data.frame(
  #   name = algae_data$Project.Site.Reference,
  #   #colour = "#8da0cb",
  #   agency = "DBCA",
  #   lat = algae_data$Latitude,
  #   lon = algae_data$Longitude,
  #   URL = algae_data$URL,
  #   id = algae_data$Site.Ref,
  #   fullname = algae_data$Site.full.Name
  # )
  # 

  algaedf <- rawwiski %>%
    dplyr::filter(`Program Site Ref` %in% algaesites &
                    `Collection Method` %in% 'Integrated over depth'&
                    `Data Category` %in% 'Laboratory results')%>%
    dplyr::select(`Program Site Ref`, `Collect Date`,`Collect Month`,`Collect Year`, `Chlorophyll a (by vol) (mg/L)`)
  algaedf$chla_ugL <- algaedf$`Chlorophyll a (by vol) (mg/L)`*1000
  
  
  
  #update week depending on year/month selected
  observeEvent(input$select_month_algae,{
    #selected_option <- input$select_year_algae
    algaewks <- algaedf %>%
      dplyr::filter(`Collect Year` %in% input$select_year_algae &
                      `Collect Month` %in% input$select_month_algae)%>%
      dplyr::select(`Collect Date`)
    
    #algaewks <- sort(unique(algaewks))

    updateSelectInput(session,"select_week_algae",
                      choices = unique(algaewks))
  })
  
  
  observeEvent(input$AlgaeFetchData,{
  microalgaeMapMarkers(mapID = 'phytoMap',
                  plotDataWeek = input$select_week_algae)
  })

##### PHYTOPLANKTON - ABOUT ####
  
  output$phytoAbout <- renderPrint({
    "Microalgae data were sourced from Department of Biodiversity, Conservation and Attractions (DBCA) and are quality-checked."
  }) 
    
  
#### HABITAT ####
  
  modHabEnable <- as.logical(get_golem_config("enable", config = "mod_hab"))
  # modHabName <- configList(get_golem_config("name", config = "mod_hab"))
  # modHabCode <- configList(get_golem_config("sensor_code", config = "mod_hab"))
  # modHabColour <- configList(get_golem_config("colour", config = "mod_hab"))
  
  print(paste0("modHabEnable: ", modHabEnable))
  
  # Generates blank WQ tab web-map
  output$habMap <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>%
      leaflet::setView(115.8613, -31.9523, 9) 
  })
  
#   ######################## EXPERIMENTAL ####################################
# 
#   # Experimenting with generating completely new tabs from the `mod_init`
#   # config in golem-config.yml
#   # e.g. a user can define a custom module other than predefined ones like
#   # 'hydro' or 'weather'
# 
#   modInit <- data.frame(
#     name = configList(get_golem_config("name", config = "mod_init")),
#     id = configList(get_golem_config("id", config = "mod_init")),
#     icon = configList(get_golem_config("icon", config = "mod_init"))
#   )
# 
#   for(i in 1:NROW(modInit)){
#     insertTab(
#       inputId = 'navbar',
#       tabPanel(
#         title = modInit[i,"name"],
#         icon = shiny::icon(modInit[i,"icon"]),
#         fluidRow(
#           column(
#             4,
# 
#           )
#         )
#       )
#     )
# 
#     mod_leafletMap_server(modInit[i,"name"])
#   }
# 
 }


  
  
  
  
  
  
  
  
  
  
  
  