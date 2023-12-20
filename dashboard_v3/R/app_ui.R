#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny leaflet 
#' @noRd
#' 
#' 


app_ui <- function(request) {
  modWeatherEnable <- as.logical(get_golem_config("enable", config = "mod_weather"))
  modWeatherTempEnable <- as.logical(get_golem_config("enable", config = "mod_weather_temp"))
  modWeatherS3Enable <- as.logical(get_golem_config("enable", config = "mod_weather_S3"))
  modHydroEnable <- as.logical(get_golem_config("enable", config = "mod_hydro"))
  modHydroFlowEnable <- as.logical(get_golem_config("enable", config = "mod_hydro_flow"))
  modHydroTideEnable <- as.logical(get_golem_config("enable", config = "mod_hydro_tide"))
  modWqEnable <- as.logical(get_golem_config("enable", config = "mod_wq"))
  modWqCatchEnable <- as.logical(get_golem_config("enable", config = "mod_wq_catch"))
  modWqEstEnable <- as.logical(get_golem_config("enable", config = "mod_wq_est"))
  modWqMooringEnable <- as.logical(get_golem_config("enable", config = "mod_wq_mooring"))
  modPhytoEnable <- as.logical(get_golem_config("enable", config = "mod_phyto"))
  modHabEnable <- as.logical(get_golem_config("enable", config = "mod_hab"))
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinyjs::useShinyjs(),  
    navbarPage(
      theme = bslib::bs_theme(bootswatch = "litera"),
      #title = "SCEVO",
      title = tags$script(HTML("var header = $('.navbar > .container-fluid');
      header.append('<div style=\"float:left\"><a href=\"https://www.dbca.wa.gov.au\"><img src=\"https://www.dropbox.com/s/4bz9621ggsvz6ib/DBCA_BCS_Logo_colour.jpg?dl=1\" alt=\"alt\" style=\"float:left;height:60px;padding-top:0px;padding-left:0px;padding-right:0px\"> </a></div>');
      console.log(header)")),
      # title = tags$script(HTML("var header = $('.navbar > .container-fluid');
      # header.append('<div style=\"float:left\"><a href=\"https://www.dbca.wa.gov.au\"><img src=\"https://www.dropbox.com/s/4bz9621ggsvz6ib/DBCA_BCS_Logo_colour.jpg\" alt=\"alt\" style=\"float:left;height:60px;padding-top:0px;padding-left:0px;padding-right:0px\"> </a></div>');
      # console.log(header)")),
      # title = tags$script(HTML("var header = $('.navbar > .container-fluid');
      # header.append('<div style=\"float:left\"><a href=\"https://www.dbca.wa.gov.au\"><img src=\"www/DBCA_BCS_Logo_colour.png\" alt=\"alt\" style=\"float:left;height:60px;padding-top:0px;padding-left:0px;padding-right:0px\"> </a></div>');
      # console.log(header)")),
      id = 'navbar',
      tabPanel(
        "Home",
        icon = icon("circle-info"),
        fluidRow(
          column(
            4,
            leaflet::leafletOutput("overviewMap", height = '850px')
          ),
          column(
            8,
            slickR::slickROutput(outputId="homeimages", height = "550px"),
            #verbatimTextOutput("homeText")
            imageOutput(outputId="homeTextImg",height = "300px") #, style = "display: block; margin-left: auto; margin-right: auto;")
          ))),
      if(isTRUE(modHydroEnable))
      {
      tabPanel(
        "Hydrology",
        icon = icon("water"),
        fluidRow(
          column(
            4,
            leaflet::leafletOutput("hydroMap", height = '850px')
          ),
          column(
            8,
            tabsetPanel(
              id = "hydroTabset",
              type = "tabs",
              if(isTRUE(modHydroFlowEnable))
              {
              tabPanel(
                  title = "Flow",
                  flowtimeseriesUI(namespace = "hydroFlowtest")
                )},
              # if(isTRUE(modHydroFlowEnable))
              # {
              # tabPanel(
              #   title = "Flow",
              #   #style = "overflow-y:scroll; max-height: 700px",
              #   #tags$details(
              #     tags$summary(HTML("Select sites and date range:")),
              #     sensorCheckBoxUI(
              #       namespace = "hydroFlow",
              #       sensorInfo = sensorInfo,
              #       group = "hydro",
              #       subGroup = "Flow"
              #     ),
              #     dateRangeUI(namespace = "hydroFlow"),
              #   #),
              #   plotOutput("hydroFlowPlot", height = "400px"),
              #   dateSliderUI(namespace = "hydroFlow")
              #   #timeseriesUI()
              # )
              #   },
              if(isTRUE(modHydroTideEnable))
              {
              tabPanel(
                  title = "Tide",
                  tidetimeseriesUI(namespace = "hydroTidetest")
              )},
              tabPanel(
                  title= "About",
                  verbatimTextOutput("hydroAbout")
              )
                
              # if(isTRUE(modHydroTideEnable))
              # {
              # tabPanel(
              #   title = "Tide",
              #   tags$summary(HTML("Select sites and date range:")), 
              #     sensorCheckBoxUI(
              #       namespace = "hydroTide",
              #       sensorInfo = sensorInfo, 
              #       group = "hydro", 
              #       subGroup = "Tide"
              #     ),
              #    dateRangeUI(namespace = "hydroTide"),
              #   plotOutput("hydroTidePlot", height = "400px"),
              #   dateSliderUI(namespace = "hydroTide")
              #   
              # )}
            )
          )
        )
      )},
      if(isTRUE(modWeatherEnable))
      {
      tabPanel(
        "Weather",
        icon = icon("cloud-sun"),
        fluidRow(
          column(
            4,
            leaflet::leafletOutput("weatherMap", height = '850px')
          ),
          column(
            8,
            tabsetPanel(
              id = "weatherTabset",
              type = "tabs",
              tabPanel(
                title = "Weather",
                weathertimeseriesUI_test(namespace = "weathertest")
              ),
              if(isTRUE(modWeatherS3Enable))
              {
              tabPanel(
                title = "WeatherS3_dev",
                weathertimeseriesUI_S3(namespace = "weather")
              )
                },
              tabPanel(
                title= "About",
                textOutput("weatherAbout")
              )
            )
          )
        )
      )},
      if(isTRUE(modWqEnable))
      {
      tabPanel(
        "Water quality",
        icon = icon("vial"),
        fluidRow(
          column(
            4,
            leaflet::leafletOutput("wqMap", height = '850px')
          ),
          column(
            8,
            tabsetPanel(
              id = "wqTabset",
              type = "tabs",
              # tabPanel(
              #   title = "Overview"
              # ),
              tabPanel(
                title = "Catchment",
               timeseriesUI_AS(namespace = "wqCatch"),
              ),
              tabPanel(
                title = "Catchment Summary",
                catchsummaryplotsUI(namespace = "wqCatchSum")
              ),
              tabPanel(
                title = "Estuary",
                timeseriesUI_AS_2(namespace = "wqEst"),
              ),
              tabPanel(
                title = "Estuary Summary",
                estsummaryplotsUI(namespace = "wqEstSum")
              ),
              tabPanel(
                title = "Profiles",
                profileplotswiskiUI(namespace = "wqProf")
                ),
              tabPanel(
                title = "Profiles - historic",
                profileplotshistUI(namespace = "wqProfHist")
              ),
              tabPanel(
                title= "Mooring",
                mooringtimeseriesUI(namespace = "wqMooring")
              ),
              tabPanel(
                title= "About",
                textOutput("wqAbout")
              )
              )
            )
              )
          )},
      tabPanel(
        "Oxygenation plant",
        icon = icon("fish"),
        fluidRow(
          column(
            4,
            leaflet::leafletOutput("oxyplantMap", height = '850px')
          ),
          column(
            8,
            tabsetPanel(
              id = "oxyTabset",
              type = "tabs",
              tabPanel(
                title = "Dissolved Oxygen",
                oxyplanttimeseriesUI(namespace = "wqDO")
              ),
              tabPanel(
                title= "About",
                textOutput("oxyAbout")
              )
              # tabPanel(
              #   title = "Dissolved Oxygen (concentration)",
              #   tags$summary(HTML("Select sites and date range:")),
              #   sensorCheckBoxUI(
              #   namespace = "wqDO",
              #   sensorInfo = sensorInfo, 
              #   group = "wq", 
              #   subGroup = "DO"
              #   ),
              #  dateRangeUI(namespace = "wqDO"),
              #  plotOutput("wqDOPlot", height = "400px"),
              #  dateSliderUI(namespace = "wqDO")
              # ),
              # tabPanel(
              #   title = "Dissolved Oxygen (saturation)",
              #   tags$summary(HTML("Select sites and date range:")),
              #   sensorCheckBoxUI(
              #     namespace = "wqDOSat",
              #     sensorInfo = sensorInfo, 
              #     group = "wq", 
              #     subGroup = "DOSaturation"
              #   ),
              #   dateRangeUI(namespace = "wqDOSat"),
              #   plotOutput("wqDOSatPlot", height = "400px"),
              #   dateSliderUI(namespace = "wqDOSat")
              # )
              
              )))),
      
      if(isTRUE(modPhytoEnable))
      {
      tabPanel(
        "Phytoplankton",
        icon = icon("bacteria"),
        fluidRow(
          # column(
          #   4,
          #   # leaflet::leafletOutput("phytoMap", height = '850px')
          #   tableOutput("keyTable")
          # ),
          column(
            12,
            tabsetPanel(
              id = "phytoTabset",
              type = "tabs",
              tabPanel(
                title = "Microalgae activity",
                microalgaeactivityUI(namespace = "Algae"),
                leaflet::leafletOutput("phytoMap", height = '700px'),
                tableOutput("keyTable")
              ),
              tabPanel(
                title= "About",
                textOutput("phytoAbout")
              )
            )
          )
        )
      )},
      if(isTRUE(modHabEnable))
      {
      tabPanel(
        "Habitat",
        icon = icon("seedling"),
        fluidRow(
          column(
            4,
            leaflet::leafletOutput("habMap", height = '850px')
          ),
          column(
            8,
            tabsetPanel(
              id = "habTabset",
              type = "tabs",
            )
          )
        )
      )}
        )
      )
    
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'secvo'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

