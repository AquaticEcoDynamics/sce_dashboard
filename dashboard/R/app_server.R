#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny RPostgreSQL DBI
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  #sensorInfo <- base::readRDS("inst/extdata/sensorInfo.csv")
  #sensorInfo <- base::load("inst/extdata/sensorInfo.rda")
  
  sensorInfo <- scevo::sensorInfo
  
  databaseEnable <- as.logical(get_golem_config("enable", config = "database_connection"))
  print(paste0("databaseEnable: ", databaseEnable))
  
  
#### WEATHER #### 
  
  # Fetch weather station data from config to map
  weatherMapStations <- data.frame(
    name = configList(get_golem_config("name", config = "mod_weather_temp")),
    colour = configList(get_golem_config("colour", config = "mod_weather_temp")),
    source = configList(get_golem_config("source", config = "mod_weather_temp")),
    lat = configList(get_golem_config("lat", config = "mod_weather_temp")),
    lon = configList(get_golem_config("lon", config = "mod_weather_temp"))
  )
  # Generates blank weather tab web-map
  output$weatherMap <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>%
      leaflet::setView(115.8613, -31.9523, 11)
  })
  #outputOptions(output, "weatherMap", suspendWhenHidden = FALSE)

  
  observe({
    input$navbar
    input$weatherTabset
    if(input$navbar=="Weather" && input$weatherTabset=="Temperature"){
      leaflet::leafletProxy(
        mapId = 'weatherMap'
      ) %>% 
        leaflet::clearMarkers() %>% 
        leaflet::addCircleMarkers(
          lng = as.numeric(weatherMapStations[["lon"]]), 
          lat = as.numeric(weatherMapStations[["lat"]]), 
          color = "white", 
          radius = 5, 
          weight = 2, 
          fillColor = weatherMapStations[["colour"]], 
          opacity = 1, 
          fillOpacity = 1,
          popup = paste0("<b>Station Name: </b>",weatherMapStations[["name"]], "<br><b>Agency: </b>", weatherMapStations[["source"]])
        )
    }
  })
  
  
##### WEATHER - TEMPERATURE ####
  
  modWeatherEnable <- as.logical(get_golem_config("enable", config = "mod_weather"))
  modWeatherTempEnable <- as.logical(get_golem_config("enable", config = "mod_weather_temp"))
  modWeatherTempName <- configList(get_golem_config("name", config = "mod_weather_temp"))
  modWeatherTempCode <- configList(get_golem_config("sensor_code", config = "mod_weather_temp"))
  modWeatherTempColour <- configList(get_golem_config("colour", config = "mod_weather_temp"))
  
  print(paste0("modWeatherTempEnable: ", modWeatherTempEnable))
 
  if(isTRUE(modWeatherTempEnable))
    {
      insertTab(
        inputId = "weatherTabset",
        select = TRUE,
        tabPanel(
          title = "Temperature",
          tags$summary(HTML("Select sites and date range:")),
          checkboxGroupInput(
            inputId = "weatherTempSiteCheckBox",
            label = NULL,
            inline = TRUE,
            choiceNames = modWeatherTempName,
            choiceValues =  modWeatherTempCode
          ),
          fluidRow(
            column(
              8,
              dateRangeInput(
                inputId = "weatherTempDateRange",
                label = NULL,
                start = Sys.Date()-2000,
                end = Sys.Date()
              )
            ),
            column(
              4,
              actionButton(
                inputId = "weatherTempFetchData",
                label = "Plot"
              )
            )
          ),
          plotOutput("weatherTempPlot", height = "400px"),
          uiOutput("weatherTempDateSliderUI")
        )
      )
      
  } else {
      print('no tab')
      #return(NULL)
    }
  
  output$weatherTempDateSliderUI <- renderUI({
    sliderInput(
      inputId = "weatherTempDateSlider",
      "Filter dates:",
      min = as.Date(input$weatherTempDateRange[1]),
      max = as.Date(input$weatherTempDateRange[2]),
      value = c(
        as.Date(input$weatherTempDateRange[1]),
        as.Date(input$weatherTempDateRange[2])
      ),
      timeFormat="%Y-%m-%d",
      width = '95%',
      animate = animationOptions(1000)
    )
  })
  
  
  observeEvent(input$weatherTempFetchData,{ 
    #browser()
    if(isTRUE(databaseEnable))
    {
    weatherTempData <- databaseConnect(sensorCodes = input$weatherTempSiteCheckBox)
    }else{
    weatherTempData <- read.csv("www/FlowGaugesEllenBrookDS.csv")
    weatherTempData$datetime <- as.POSIXct(weatherTempData$datetime,format="%d/%m/%Y")
    }
   
    # Get line plot colours for selected sensors
    weatherTempDataColours <- sensorColours(
      allCodes = modWeatherTempCode,
      allColours = modWeatherTempColour,
      selectedCodes = input$weatherTempSiteCheckBox
    )
    # Generate line graph from fetched data and display between slider dates
    output$weatherTempPlot <- renderPlot({
      weatherTempData <- dplyr::filter(
        weatherTempData,
        datetime >= as.POSIXct(input$weatherTempDateSlider[1]),
        datetime <= as.POSIXct(input$weatherTempDateSlider[2])
      )
      # Plot the graph    
      plotLine(
        plotData = weatherTempData,
        plotDataX = "datetime",
        plotDataY = "st_value_1",
        plotDataGroup = "st_sensor_code",
        plotLabelX = "Date",
        plotLabelY = "Temperature (°C)",
        plotDataColours = weatherTempDataColours
      )
    })
    
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
      weatherData$datetime <- as.POSIXct(weatherData$Date,format="%d-%m-%Y %H:%M:%S")
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
      plotLine_s3(
        plotData = weatherData,
        plotDataX = "datetime",
        plotDataY = "Data",
        #plotDataGroup = "st_sensor_code",
        plotLabelX = "Date",
        plotLabelY = input$weatherVariable,
        plotDataColours = "#a6d854"
      )
    })

  })
  
  
#### HYDRO #### 
  
  # Fetch hydro station data from config to map
  hydroMapStations <- data.frame(
    name = configList(get_golem_config("name", config = "mod_hydro_flow")),
    colour = configList(get_golem_config("colour", config = "mod_hydro_flow")),
    source = configList(get_golem_config("source", config = "mod_hydro_flow")),
    lat = configList(get_golem_config("lat", config = "mod_hydro_flow")),
    lon = configList(get_golem_config("lon", config = "mod_hydro_flow"))
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
      leaflet::leafletProxy(
        mapId = 'hydroMap'
      ) %>%
        leaflet::clearMarkers() %>%
        leaflet::addCircleMarkers(
          lng = as.numeric(hydroMapStations[["lon"]]),
          lat = as.numeric(hydroMapStations[["lat"]]),
          color = "white",
          radius = 5,
          weight = 2,
          fillColor = hydroMapStations[["colour"]],
          opacity = 1,
          fillOpacity = 1,
          popup = paste0("<b>Station Name: </b>",hydroMapStations[["name"]], "<br><b>Agency: </b>", hydroMapStations[["source"]])
        )
    }
  })
  # Filter sensorInfo data to only hydro sensors
  hydroSensorInfo <- sensorInfo[sensorInfo[["group"]]=="hydro",]
  observeEvent(input$hydroTabset,{
    switch(
      input$hydroTabset,
      "Flow" = sensorMapMarkers(
        mapID = "hydroMap",
        data = hydroSensorInfo,
        subGroup = input$hydroTabset
        ),
      "Tide" = sensorMapMarkers(
        mapID = "hydroMap",
        data = hydroSensorInfo,
        subGroup = input$hydroTabset
      )
    )
  })
  
##### HYDRO - FLOW ####
  
  modHydroEnable <- as.logical(get_golem_config("enable", config = "mod_hydro"))
  modHydroFlowEnable <- as.logical(get_golem_config("enable", config = "mod_hydro_flow"))
  modHydroFlowName <- configList(get_golem_config("name", config = "mod_hydro_flow"))
  modHydroFlowCode <- configList(get_golem_config("sensor_code", config = "mod_hydro_flow"))
  modHydroFlowColour <- configList(get_golem_config("colour", config = "mod_hydro_flow"))
  
  print(paste0("modHydroFlowEnable: ", modHydroFlowEnable))
  
  if(isTRUE(modHydroFlowEnable))
  {
    insertTab(
      inputId = "flowTabset",
      select = TRUE,
      tabPanel(
        title = "Flow",
        tags$summary(HTML("Select sites and date range:")),
        checkboxGroupInput(
          inputId = "hydroFlowSiteCheckBox",
          label = NULL,
          inline = TRUE,
          choiceNames = modHydroFlowName,
          choiceValues =  modHydroFlowCode
        ),
        fluidRow(
          column(
            8,
            dateRangeInput(
              inputId = "hydroFlowDateRange",
              label = NULL,
              start = Sys.Date()-7,
              end = Sys.Date()
            )
          ),
          column(
            4,
            actionButton(
              inputId = "hydroFlowFetchData",
              label = "Plot"
            )
          )
        ),
        plotOutput("hydroFlowPlot", height = "400px"),
        uiOutput("hydroFlowDateSliderUI")
      )
    )
  
    
  } else {
    print('no tab')
    #return(NULL)
  }

  # Update slider from calendar date inputs
  output$hydroFlowDateSlider <- renderUI({
    plotSlider(
      inputID = "hydroFlowDateSlider",
      minDate = input$hydroFlowDateRange[1],
      maxDate = input$hydroFlowDateRange[2]
    )
  })

  shinyjs::hide("hydroFlowDateSliderBox")

  # On button click, fetch sensor data from SCEVO and graph
  observeEvent(input$hydroFlowFetchData,{
    shinyjs::show("hydroFlowDateSliderBox", anim = TRUE, animType = "fade")
    if(isTRUE(databaseEnable))
    {
    hydroFlowData <- databaseConnect(sensorCodes = input$hydroFlowSiteCheckBox)
    }else{
    hydroFlowData <- read.csv("www/FlowGaugesEllenBrookDS.csv")
    hydroFlowData$datetime <- as.POSIXct(hydroFlowData$datetime,format="%d/%m/%Y")
    }

    # Get line plot colours for selected sensors
    hydroFlowDataColours <- sensorColours(
      allCodes = modHydroFlowCode,
      allColours = modHydroFlowColour,
      selectedCodes = input$hydroFlowSiteCheckBox
    )

    # Generate line graph from fetched data and display between slider dates
    output$hydroFlowPlot <- renderPlot({
      hydroFlowData <- dplyr::filter(
        hydroFlowData,
        datetime >= as.POSIXct(input$hydroFlowDateSlider[1]),
        datetime <= as.POSIXct(input$hydroFlowDateSlider[2])

      )

    # Plot the graph
    plotLine(
      plotData = hydroFlowData,
      plotDataX = "datetime",
      plotDataY = "st_value_1",
      plotDataGroup = "st_sensor_code",
      plotLabelX = "Date",
      plotLabelY = "Flow (m3/s)",
      plotDataColours = hydroFlowDataColours
    )
    })
  })

  
##### HYDRO - TIDE ####
  
  modHydroEnable <- as.logical(get_golem_config("enable", config = "mod_hydro"))
  modHydroTideEnable <- as.logical(get_golem_config("enable", config = "mod_hydro_tide"))
  modHydroTideName <- configList(get_golem_config("name", config = "mod_hydro_tide"))
  modHydroTideCode <- configList(get_golem_config("sensor_code", config = "mod_hydro_tide"))
  modHydroTideColour <- configList(get_golem_config("colour", config = "mod_hydro_tide"))
  
  print(paste0("modHydroTideEnable: ", modHydroTideEnable))
  
  if(isTRUE(modHydroTideEnable))
  {
    insertTab(
      inputId = "tideTabset",
      select = TRUE,
      tabPanel(
        title = "Tide",
        tags$summary(HTML("Select sites and date range:")),
        checkboxGroupInput(
          inputId = "hydroTideSiteCheckBox",
          label = NULL,
          inline = TRUE,
          choiceNames = modHydroTideName,
          choiceValues =  modHydroTideCode
        ),
        fluidRow(
          column(
            8,
            dateRangeInput(
              inputId = "hydroTideDateRange",
              label = NULL,
              start = Sys.Date()-7,
              end = Sys.Date()
            )
          ),
          column(
            4,
            actionButton(
              inputId = "hydroTideFetchData",
              label = "Plot"
            )
          )
        ),
        plotOutput("hydroTidePlot", height = "400px"),
        uiOutput("hydroTideDateSliderUI")
      )
    )
    
    # Update slider from calendar date inputs
    output$hydroTideDateSlider <- renderUI({
      plotSlider(
        inputID = "hydroTideDateSlider",
        minDate = input$hydroTideDateRange[1],
        maxDate = input$hydroTideDateRange[2]
      )
    })
    shinyjs::hide("hydroTideDateSliderBox")
    
    # On button click, fetch sensor data from SCEVO and graph
    observeEvent(input$hydroTideFetchData,{
      shinyjs::show("hydroTideDateSliderBox", anim = TRUE, animType = "fade")
      if(isTRUE(databaseEnable))
      {
      hydroTideData <- databaseConnect(sensorCodes = input$hydroTideSiteCheckBox)
      }else{
      hydroTideData <- read.csv("www/FlowGaugesEllenBrookDS.csv")
      hydroTideData$datetime <- as.POSIXct(hydroTideData$datetime,format="%d/%m/%Y")
      }
      # library(arrow)
      # hydroTideData <- data.frame(read_parquet("www/FFFBH01_Tidal_Height_DATA.parquet"))
      # colnames(hydroTideData)<-c("datetime","Depth","st_value_1","QC")
      # hydroTideData$datetime <- as.POSIXct(hydroTideData$datetime,format="%d-%m-%Y %H:%M:%S")
      # hydroTideData$st_sensor_code <-"1087"
      
      #print(head(hydroTideData))
      
      # Get line plot colours for selected sensors
      hydroTideDataColours <- sensorColours(
        allCodes = modHydroTideCode,
        allColours = modHydroTideColour,
        selectedCodes = input$hydroTideSiteCheckBox
      )
      
      # Generate line graph from fetched data and display between slider dates
      output$hydroTidePlot <- renderPlot({
        hydroTideData <- dplyr::filter(
          hydroTideData,
          datetime >= as.POSIXct(input$hydroTideDateSlider[1]),
          datetime <= as.POSIXct(input$hydroTideDateSlider[2])
        )
        
        # Plot the graph
        plotLine(
          plotData = hydroTideData,
          plotDataX = "datetime",
          plotDataY = "st_value_1",
          plotDataGroup = "st_sensor_code",
          plotLabelX = "Date",
          plotLabelY = "Tide (mAHD)",
          plotDataColours = hydroTideDataColours
        )
      })
    })
    
  } 
  else {
    print('no tab')
    #return(NULL)
  }

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
  modWqCatchName <- configList(get_golem_config("name", config = "mod_wq_catch"))
  modWqCatchCode <- configList(get_golem_config("sensor_code", config = "mod_wq_catch"))
  modWqCatchColour <- configList(get_golem_config("colour", config = "mod_wq_catch"))
  modWqEstEnable <- as.logical(get_golem_config("enable", config = "mod_wq_est"))
  modWqEstName <- configList(get_golem_config("name", config = "mod_wq_est"))
  modWqEstCode <- configList(get_golem_config("sensor_code", config = "mod_wq_est"))
  modWqEstColour <- configList(get_golem_config("colour", config = "mod_wq_est"))
  
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
  
  Catchment_data<-read.csv("www/Catchment_monitoring_sites2 - Copy.csv")
  Estuary_data<- read.csv("www/SCESTUARY - Copy.csv")
  wqcatchMapStations <- data.frame(
    name = Catchment_data$Site_Code,
    colour = "#a6d854",
    source = "unknown",
    lat = Catchment_data$Latitude,
    lon = Catchment_data$Longitude,
    URL = Catchment_data$URL,
    id = Catchment_data$AWRC_No,
    fullname = Catchment_data$SiteName_and_Suburb
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
    if(input$navbar=="Water quality" && input$wqTabset=="Overview"){
      leaflet::leafletProxy(
        mapId = 'wqMap'
      ) %>%
        leaflet::clearMarkers() %>%
        leaflet::addCircleMarkers(
          lng = as.numeric(wqcatchMapStations[["lon"]]),
          lat = as.numeric(wqcatchMapStations[["lat"]]),
          color = "white",
          radius = 5,
          weight = 2,
          fillColor = wqcatchMapStations[["colour"]],
          opacity = 1,
          fillOpacity = 1,
          # popup = paste0("<b>Station Name: </b>",wqcatchMapStations[["name"]],
          #                "<br><b>Agency: </b>", wqcatchMapStations[["source"]]))
        popup = paste0("<b>", paste0("<img src=' ",wqcatchMapStations[["URL"]]," ' width='100px' height='100px'>"),"</b></br>",
                       paste0("<b>Name: </b>", wqcatchMapStations[["fullname"]] ),"</b></br>",
                       paste0("<b>Site code: </b>", wqcatchMapStations[["name"]] ),"</b></br>",
                       paste0("<b>Site. Ref: </b>", wqcatchMapStations[["id"]]),
                       "</b></br>"))%>% 
        leaflet::addCircleMarkers(
          lng = as.numeric(wqestMapStations[["lon"]]),
          lat = as.numeric(wqestMapStations[["lat"]]),
          color = "white",
          radius = 5,
          weight = 2,
          fillColor = wqestMapStations[["colour"]],
          opacity = 1,
          fillOpacity = 1,
          # popup = paste0("<b>Station Name: </b>",wqestMapStations[["name"]],
          #                "<br><b>Agency: </b>", wqestMapStations[["source"]]))
          popup = paste0("<b>", paste0("<img src=' ",wqestMapStations[["URL"]]," ' width='100px' height='100px'>"),"</b></br>",
                         paste0("<b>Name: </b>", wqestMapStations[["fullname"]] ),"</b></br>",
                         paste0("<b>Site code: </b>", wqestMapStations[["name"]] ),"</b></br>",
                         paste0("<b>Site. Ref: </b>", wqestMapStations[["id"]]),
                         "</b></br>"))%>% 
        
        leaflet::addLayersControl(baseGroups = c("Ecological Managment Zones","Elevation","All off"),
                                  #overlayGroups = c("Estuary sites","Catchment sites"),
                                  position="topright",
                                  options = leaflet::layersControlOptions(collapsed = FALSE))%>%
        addLegendCustom(colors, labels, sizes,shapes,borders=NA,title)
    }else{
      if(input$navbar=="Water quality" && input$wqTabset=="Catchment"){
        leaflet::leafletProxy(
          mapId = 'wqMap'
        ) %>% 
          leaflet::clearMarkers() %>% 
          leaflet::addCircleMarkers(
            lng = as.numeric(wqcatchMapStations[["lon"]]), 
            lat = as.numeric(wqcatchMapStations[["lat"]]), 
            color = "white", 
            radius = 5, 
            weight = 2, 
            fillColor = wqcatchMapStations[["colour"]], 
            opacity = 1, 
            fillOpacity = 1,
            # popup = paste0("<b>Station Name: </b>",wqcatchMapStations[["name"]], 
            #                "<br><b>Agency: </b>", wqcatchMapStations[["source"]]))
            popup = paste0("<b>", paste0("<img src=' ",wqcatchMapStations[["URL"]]," ' width='100px' height='100px'>"),"</b></br>",
              paste0("<b>Name: </b>", wqcatchMapStations[["fullname"]] ),"</b></br>",
              paste0("<b>Site code: </b>", wqcatchMapStations[["name"]] ),"</b></br>",
              paste0("<b>Site. Ref: </b>", wqcatchMapStations[["id"]]),
              "</b></br>"))%>% 
          leaflet::addLayersControl(baseGroups = c("Ecological Managment Zones","Elevation","All off"), 
                                    #overlayGroups = c("Estuary sites","Catchment sites"),position="topright",
                                    options = leaflet::layersControlOptions(collapsed = FALSE))
      }else{
        if(input$navbar=="Water quality" && input$wqTabset=="Estuary"){
        leaflet::leafletProxy(
          mapId = 'wqMap'
        ) %>% 
          leaflet::clearMarkers() %>% 
          leaflet::addCircleMarkers(
            lng = as.numeric(wqestMapStations[["lon"]]), 
            lat = as.numeric(wqestMapStations[["lat"]]), 
            color = "white", 
            radius = 5, 
            weight = 2, 
            fillColor = wqestMapStations[["colour"]], 
            opacity = 1, 
            fillOpacity = 1,
            # popup = paste0("<b>Station Name: </b>",wqestMapStations[["name"]], 
            #                "<br><b>Agency: </b>", wqestMapStations[["source"]]))
            popup = paste0("<b>", paste0("<img src=' ",wqestMapStations[["URL"]]," ' width='100px' height='100px'>"),"</b></br>",
                       paste0("<b>Name: </b>", wqestMapStations[["fullname"]] ),"</b></br>",
                       paste0("<b>Site code: </b>", wqestMapStations[["name"]] ),"</b></br>",
                       paste0("<b>Site. Ref: </b>", wqestMapStations[["id"]]),
                       "</b></br>"))%>% 
          leaflet::addLayersControl(baseGroups = c("Ecological Managment Zones","Elevation","All off"), 
                                    #overlayGroups = c("Estuary sites","Catchment sites"),position="topright",
                                    options = leaflet::layersControlOptions(collapsed = FALSE))
        }else{
          leaflet::leafletProxy(
            mapId = 'wqMap'
          ) %>% 
            leaflet::addPolylines(data = DATP, stroke = T, weight = 4,opacity = 1,
                                  color= c("orange","yellow"), 
                                  group = "Profile", label = ~Name)%>%
            leaflet::addLayersControl(baseGroups = c("Ecological Managment Zones","Elevation","Profile","All off"), 
                                    #overlayGroups = c("Estuary sites","Catchment sites"),position="topright",
                                    options = leaflet::layersControlOptions(collapsed = FALSE))
          #%>%
            # leaflet::addLegend("bottomright", pal = profpal, values = ~Name,
            #           title = "profile line",
            #           opacity = 1
            #)
      }
      }
    }
  })
  
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
  
  if(isTRUE(modWqCatchEnable))
  {
    insertTab(
      inputId = "catchTabset",
      select = TRUE,
      tabPanel(
        title = "Catchment",
        tags$summary(HTML("Select sites and date range:")),
        checkboxGroupInput(
          inputId = "wqCatchSiteCheckBox",
          label = NULL,
          inline = TRUE,
          choiceNames = modWqCatchName,
          choiceValues =  modWqCatchCode
        ),
        fluidRow(
          column(
            8,
            dateRangeInput(
              inputId = "wqCatchDateRange",
              label = NULL,
              start = Sys.Date()-7,
              end = Sys.Date()
            )
          ),
          column(
            4,
            actionButton(
              inputId = "wqCatchFetchData",
              label = "Plot"
            )
          )
        ),
        plotOutput("wqCatchPlot", height = "400px"),
        uiOutput("wqCatchDateSliderUI")
      )
    )
  } else {
    print('no tab')
  }

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

  
  ###get sensor Code Catchment (AS version)
  selectedCatchsensorID <- reactive({
    req(isolate(input$wqCatchSites))
    req(isolate(input$wqCatchvariable))
    
    tmp<-sensorslist %>%  ### this is a Rda object stored in #the package, call it within the code I use it in! in #the top comment section
      dplyr::filter(Site %in% input$wqCatchSites & s_graph_value%in%input$wqCatchvariable)%>% ## filter the data based on the user selection of both site and variable
      dplyr::select(s_table_name)
    
    tmp
    
  })
  

  # On button click, fetch sensor data from SCEVO and graph for catchment - AS version
  
  observeEvent(input$wqCatchFetchData,{
    if(isTRUE(databaseEnable))
    {
    wqCatchData <- databaseConnect(sensorCodes = selectedCatchsensorID())
    }else{
    wqCatchData <- read.csv("www/FlowGaugesEllenBrookDS.csv")
    wqCatchData$datetime <- as.POSIXct(wqCatchData$datetime,format="%d/%m/%Y")
    }
    
    # Generate line graph from fetched data and diplay between slider dates
     output$wqCatchPlot <- plotly::renderPlotly({
    
      wqCatchData <- dplyr::filter(
        wqCatchData,
        datetime >= as.POSIXct(input$wqCatchDateSlider[1]),
        datetime <= as.POSIXct(input$wqCatchDateSlider[2])
        # datetime >= as.POSIXct(input$wqCatchDateFrom),
        # datetime <= as.POSIXct(input$wqCatchDateTo)
      )

      # Plot the graph
      plotPoint(
        plotData = wqCatchData,
        plotDataX = "datetime",
        plotDataY = "st_value_1",
        plotDataGroup = "st_sensor_code",
        plotLabelX = "Date",
        plotLabelY = paste0(isolate(input$wqCatchvariable)),
        plotDataColours = "#a6d854"
      )
      
      ######### testing a diff plot
      # output$scatter <- plotly::renderPlotly({
      #   
      #   wqCatchData <- dplyr::filter(
      #     wqCatchData,
      #     # datetime >= as.POSIXct(input$wqCatchDateSlider[1]),
      #     # datetime <= as.POSIXct(input$wqCatchDateSlider[2])
      #     datetime >= as.POSIXct(input$wqCatchDateFrom),
      #     datetime <= as.POSIXct(input$wqCatchDateTo)
      #   )
      # })
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
    
    
    # Generate line graph from fetched data and diplay between slider dates
      output$catchsumPlot <- plotly::renderPlotly({
      #output$catchsumPlot <- renderPlot({
      plotly::ggplotly(
       plotCatchSum(
        plotDataSite = input$select_site_catch,
        plotDataYear = input$select_year_catch,
        plotDataVar = input$select_vars_catch
      )
      )
    })
  })
##### WQ - ESTUARY ####
  
  if(isTRUE(modWqEstEnable))
  {
    insertTab(
      inputId = "estTabset",
      select = TRUE,
      tabPanel(
        title = "Estuary",
        tags$summary(HTML("Select sites and date range:")),
        checkboxGroupInput(
          inputId = "wqEstSiteCheckBox",
          label = NULL,
          inline = TRUE,
          choiceNames = modWqEstName,
          choiceValues =  modWqEstCode
        ),
        fluidRow(
          column(
            8,
            dateRangeInput(
              inputId = "wqEstDateRange",
              label = NULL,
              start = Sys.Date()-7,
              end = Sys.Date()
            )
          ),
          column(
            4,
            actionButton(
              inputId = "wqEstFetchData",
              label = "Plot"
            )
          )
        ),
        plotOutput("wqEstPlot", height = "400px"),
        uiOutput("wqEstDateSliderUI")
      )
    )
  } else {
    print('no tab')
  }
  
  # Update slider from calendar date inputs
  # output$wqEstDateSlider <- renderUI({
  #   plotSlider(
  #     inputID = "wqEstDateSlider",
  #     minDate = input$wqEstDateRange[1],
  #     maxDate = input$wqEstDateRange[2]
  #   )
  # })
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
  
  # On button click, fetch sensor data from SCEVO and graph for catchment - AS version
  
  observeEvent(input$wqEstFetchData,{
    if(isTRUE(databaseEnable))
    {
    wqEstData_s<- databaseConnect(sensorCodes = selectedEstsensorID_s())
    wqEstData_b<- databaseConnect(sensorCodes = selectedEstsensorID_b())
    }else{
    wqEstData_s <- read.csv("www/FlowGaugesEllenBrookDS.csv")
    wqEstData_s$datetime <- as.POSIXct(wqEstData_s$datetime,format="%d/%m/%Y")
    wqEstData_b <- read.csv("www/FlowGaugesEllenBrookDS.csv")
    wqEstData_b$datetime <- as.POSIXct(wqEstData_b$datetime,format="%d/%m/%Y")
    }
    
    # Generate line graph from fetched data and diplay between slider dates
    output$wqEstPlot_s <- plotly::renderPlotly({
      wqEstData_s <- dplyr::filter(
        wqEstData_s,
        datetime >= as.POSIXct(input$wqEstDateSlider[1]),
        datetime <= as.POSIXct(input$wqEstDateSlider[2])
      )
      # Plot the graph
      plotPoint(
        plotData = wqEstData_s,
        plotDataX = "datetime",
        plotDataY = "st_value_1",
        plotDataGroup = "st_sensor_code",
        plotLabelX = "Date",
        plotLabelY = paste0(isolate(input$Estvariable)),
        plotDataColours = "#8da0cb"
      )
    })
     
    output$wqEstPlot_b <- plotly::renderPlotly({
    wqEstData_b <- dplyr::filter(
      wqEstData_b,
      datetime >= as.POSIXct(input$wqEstDateSlider[1]),
      datetime <= as.POSIXct(input$wqEstDateSlider[2])
    )
    plotPoint(
      plotData = wqEstData_b,
      plotDataX = "datetime",
      plotDataY = "st_value_1",
      plotDataGroup = "st_sensor_code",
      plotLabelX = "Date",
      plotLabelY = paste0(isolate(input$Estvariable)),
      plotDataColours = "#8da0cb"
    )
    })
  })

        
##### WQ - ESTUARY SUMMARY ####    
 
  observeEvent(input$wqEstSumFetchData,{
    #dfimport <- read.csv(file = 'www/DBCA_data_export/DBCA_data_export_2023-07-19_1615.csv', check.names = FALSE)
    #dfimport <- awss3Connect(filename = 'data-warehouse/dbca/wiski/DBCA_data_export_2023-07-19_1615.csv')
    
    # Generate line graph from fetched data and diplay between slider dates
    output$estsumPlot <- plotly::renderPlotly({

      plotEstSum(
        plotDataRegion = input$select_region,
        plotDataYear = input$select_year,
        plotDataVar = input$select_vars
      )
    })
   })
  
  
##### WQ - PROFILE ####
  
  profdir <- "www/Profile_plotting"
  #regions <- c("Swan","Canning")
  #subdir <- list.dirs(profdatdir, full.names = TRUE, recursive=FALSE)

  
  #update week depending on region selected
  observeEvent(input$select_region,{
    selected_option <- input$select_region
    choices_list <- list(
      "Swan"    = list.dirs(file.path(profdir,"Swan"),
                            full.names = FALSE, recursive=FALSE),
      "Canning" = list.dirs(file.path(profdir,"Canning"),
                            full.names = FALSE, recursive=FALSE)
    )
    updateSelectInput(session,"select_week", 
                      choices = choices_list[[selected_option]])
  })
  
  #on button click, plot profiles. If plot already exists in folder, just render image
  observeEvent(input$wqProfFetchData,{
      profdat <- file.path(profdir,input$select_region, input$select_week)
      if(!file.exists(file.path(profdat, 'plots',
                                paste0(tolower(input$select_region),'_',input$select_week,'_surfer.png')
                                ))){
        library(rivRmon)
        if(input$select_region=="Swan"){
        profPlot <- swan_surfR_alt(profdat, 
                                   ovit = "green", 
                                   ocav = "green")
        }else{
          if(input$select_region=="Canning"){
          profPlot <- canning_surfR(profdat, 
                                    obac = "green", 
                                    onic = "green",
                                    SHELL = FALSE) 
          }
        }
      }

  # output$profPlot <- renderImage({
  #     filename <- normalizePath(file.path(profdat,'plots',
  #                                         paste0(tolower(input$select_region),'_',input$select_week,'_surfer.png')))
  #                                             # Return a list containing the filename
  #                                             list(src = filename,
  #                                                  width = '1100px',
  #                                                  height = '700px')
  #   }, deleteFile = FALSE)
  
      library(slickR)
  output$profPlot <- renderSlickR({
    filename <- list.files("www/Profile_plotting/Swan/2023-07-03/plots/",pattern=".png",full.names = T
                                                            )
    slickR(filename)
    })
  })
  
##### WQ - MOORING ####  
  
  # Filter sensorInfo data to only water quality sensors
  wqSensorInfo <- sensorInfo[sensorInfo[["group"]]=="wq",]
  wqsubGroupData <- wqSensorInfo[wqSensorInfo[["subGroup"]]=="Mooring",]
  wqsubGroupData <- wqsubGroupData[1:2,]
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
  
  # Update slider from calendar date inputs
  output$wqMooringDateSlider <- renderUI({
    plotSlider(
      inputID = "wqMooringDateSlider",
      minDate = input$wqMooringDateRange[1],
      maxDate = input$wqMooringDateRange[2]
    )
  })
  
  shinyjs::hide("wqMooringDateSliderBox")
  
  # On button click, fetch sensor data from SCEVO and graph
  observeEvent(input$wqMooringFetchData,{ 
    
    shinyjs::show("wqMooringDateSliderBox", anim = TRUE, animType = "fade")
    if(isTRUE(databaseEnable))
    {  
    wqMooringData <- databaseConnect(sensorCodes = input$wqMooringSiteCheckBox)
    }else{
    wqMooringData  <- read.csv("www/FlowGaugesEllenBrookDS.csv")
    wqMooringData$datetime <- as.POSIXct(wqMooringData$datetime,format="%d/%m/%Y")
    }
    
    # Get line plot colours for selected sensors
    wqMooringDataColours <- activeSensorColours(
      checkBoxInputs = input$wqMooringSiteCheckBox,
      sensorInfo = wqSensorInfo
    )
   
    
    # Generate line graph from fetched data and display between slider dates
    output$wqMooringPlot <- renderPlot({
      
      wqMooringData <- dplyr::filter(
        wqMooringData,
        datetime >= as.POSIXct(input$wqMooringDateSlider[1]),
        datetime <= as.POSIXct(input$wqMooringDateSlider[2])
      )
      
      # Plot the graph    
      plotLine(
        plotData = wqMooringData,
        plotDataX = "datetime",
        plotDataY = "st_value_1",
        plotDataGroup = "st_sensor_code",
        plotLabelX = "Date",
        plotLabelY = "Dissolved Oxygen (mg/L)",
        plotDataColours = wqMooringDataColours
        #plotDataColours = "red"
      )
    })
  })  
  
  
  
#### OXYGENATION PLANT ####

  #### OXYGENATION PLANT - Oxy concentration ####
  
  # Filter sensorInfo data to only water quality sensors
  wqSensorInfo <- sensorInfo[sensorInfo[["group"]]=="wq",]
  wqsubGroupData <- wqSensorInfo[wqSensorInfo[["subGroup"]]=="DO",]
  wqsubGroupData <- wqsubGroupData[1:2,]
  # Generates blank waterquality tab web-map
  output$oxyplantMap <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>%
      leaflet::setView(115.8613, -31.9523, 9) %>% 
      leaflet::addCircleMarkers(
        lng = wqsubGroupData[["lon"]], 
        lat = wqsubGroupData[["lat"]], 
        color = "white", 
        radius = 7, 
        weight = 2, 
        fillColor = wqsubGroupData[["colour"]], 
        opacity = 1, 
        fillOpacity = 1,
        popup = paste0("<b>Station Name: </b>",wqsubGroupData[["label"]], "<br><b>Agency: </b>", wqsubGroupData[["agency"]])
      )
  })
  
  # Update slider from calendar date inputs
  output$wqDODateSlider <- renderUI({
    plotSlider(
      inputID = "wqDODateSlider",
      minDate = input$wqDODateRange[1],
      maxDate = input$wqDODateRange[2]
    )
  })
  
  shinyjs::hide("wqDODateSliderBox")
  
  # On button click, fetch sensor data from SCEVO and graph
  observeEvent(input$wqDOFetchData,{ 
    
    shinyjs::show("wqDODateSliderBox", anim = TRUE, animType = "fade")
    if(isTRUE(databaseEnable))
    {
    wqDOData <- databaseConnect(sensorCodes = input$wqDOSiteCheckBox) 
    }else{
    wqDOData  <- read.csv("www/FlowGaugesEllenBrookDS.csv")
    wqDOData$datetime <- as.POSIXct(wqDOData$datetime,format="%d/%m/%Y")
    }
    
    # Get line plot colours for selected sensors
    wqDODataColours <- activeSensorColours(
      checkBoxInputs = input$wqDOSiteCheckBox,
      sensorInfo = wqSensorInfo
    )
    
    # Generate line graph from fetched data and display between slider dates
    output$wqDOPlot <- renderPlot({
      
      wqDOData <- dplyr::filter(
        wqDOData,
        datetime >= as.POSIXct(input$wqDODateSlider[1]),
        datetime <= as.POSIXct(input$wqDODateSlider[2])
      )
      
      # Plot the graph    
      plotLine(
        plotData = wqDOData,
        plotDataX = "datetime",
        plotDataY = "st_value_1",
        plotDataGroup = "st_sensor_code",
        plotLabelX = "Date",
        plotLabelY = "Dissolved Oxygen (mg/L)",
        plotDataColours = wqDODataColours
      )
    })
  })
  
  #### OXYGENATION PLANT - Oxy saturation ####
  
  # Update slider from calendar date inputs
  output$wqDOSatDateSlider <- renderUI({
    plotSlider(
      inputID = "wqDOSatDateSlider",
      minDate = input$wqDOSatDateRange[1],
      maxDate = input$wqDOSatDateRange[2]
    )
  })
  
  shinyjs::hide("wqDOSatDateSliderBox")
  
  # On button click, fetch sensor data from SCEVO and graph
  observeEvent(input$wqDOSatFetchData,{ 
    
    shinyjs::show("wqDOSatDateSliderBox", anim = TRUE, animType = "fade")
    if(isTRUE(databaseEnable))
    {
    wqDOSatData <- databaseConnect(sensorCodes = input$wqDOSatSiteCheckBox) 
    }else{
    wqDOSatData  <- read.csv("www/FlowGaugesEllenBrookDS.csv")
    wqDOSatData$datetime <- as.POSIXct(wqDOSatData$datetime,format="%d/%m/%Y")
    }
    
    # Get line plot colours for selected sensors
    wqDOSatDataColours <- activeSensorColours(
      checkBoxInputs = input$wqDOSatSiteCheckBox,
      sensorInfo = wqSensorInfo
    )
    
    # Generate line graph from fetched data and display between slider dates
    output$wqDOSatPlot <- renderPlot({
      
      wqDOSatData <- dplyr::filter(
        wqDOSatData,
        datetime >= as.POSIXct(input$wqDOSatDateSlider[1]),
        datetime <= as.POSIXct(input$wqDOSatDateSlider[2])
      )
      
      # Plot the graph    
      plotLine(
        plotData = wqDOSatData,
        plotDataX = "datetime",
        plotDataY = "st_value_1",
        plotDataGroup = "st_sensor_code",
        plotLabelX = "Date",
        plotLabelY = "DO Saturation (%)",
        plotDataColours = wqDOSatDataColours
      )
    })
  })
  
#### PHYTOPLANKTON ####
  
  modPhytoEnable <- as.logical(get_golem_config("enable", config = "mod_phyto"))
  # modPhytoName <- configList(get_golem_config("name", config = "mod_phyto"))
  # modPhytoCode <- configList(get_golem_config("sensor_code", config = "mod_phyto"))
  # modPhytoColour <- configList(get_golem_config("colour", config = "mod_phyto"))
  
  print(paste0("modPhytoEnable: ", modPhytoEnable))
  
  # Generates blank WQ tab web-map
  output$phytoMap <- leaflet::renderLeaflet({
    leaflet::leaflet() %>%
      leaflet::addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>%
      leaflet::setView(115.8613, -31.9523, 9) 
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


  
  
  
  
  
  
  
  
  
  
  
  