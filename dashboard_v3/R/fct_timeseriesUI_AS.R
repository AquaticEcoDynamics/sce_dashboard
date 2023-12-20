#' timeseriesUI
#'
#' @description Generates the common timeseries UI elements e.g. date inputs, slider. Requires @param namespace to individualise elements.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

#SensorCodes<-read.csv("C:/Users/AminaSaeed/Desktop/sensors_list.csv")
#Catchment_data<-read.csv("C:/Users/AminaSaeed/Desktop/Dhasboard folders - Test/Draft4/Draft4/inst/app/www/Catchment_monitoring_sites2.csv")
Catchment_data<-read.csv("www/Catchment_monitoring_sites2_withCatchName.csv")
#Estuary_data<- read.csv("www/SCESTUARY - Copy.csv")

Cacthment_sites<-Catchment_data$Site_Code
WQ_variables<-unique(sensorslist$s_graph_value) ## set up water quality variables

#### rename water quality variables and add additional column with units

#### for some variables convert units from ug/L to mg/L

#### Append original sign with variable


timeseriesUI_AS <- function(namespace){
  
  library(plotly)
  return(

    tags$div(
      wellPanel(fluidRow(
        column(3,selectInput(
          inputId =paste0(namespace,"Sites"), label = "Select Site(s):", choices=Cacthment_sites,multiple = TRUE),style="margin-top: 50px;"),
        column(3,selectInput(
          inputId =paste0(namespace,"variable"), label ="Select Variable:", choices = WQ_variables),style="margin-top: 50px;"),
        column(2,dateInput(
          inputId = paste0(namespace,"DateFrom"),label = "From:", value = Sys.Date()-7),style="margin-top: 50px; "),
        column(2,dateInput(inputId = paste0(namespace,"DateTo"), label = "To:", value = Sys.Date()),style="margin-top: 50px;"),

        column(2,actionButton( inputId = paste0(namespace,"FetchData"),
                               label = "Plot",style="margin-top: 83px; width: 100%")

                # surface unresponsive version (height= '250px',width='620' ) instead pf just width
               # box(title = h4("Bottom", style = 'font-size:20px;text-align:center;width:100%'),solidHeader = T,width=11)#,shinycssloaders::withSpinner(dygraphs::dygraphOutput("ribbonplot1",width ='100%' ))) # bottom
        ))
        ,
      fluidRow(
        column(
          12,
          uiOutput(
            outputId = paste0(namespace,"DateSlider")
          ))
        ),
      # fluidRow(
      #   column(
      #     12,
      #  # box(title = h4("Surface", style = 'font-size:20px;text-align:center; width:100%'),solidHeader = T,width=11 ,shinycssloaders::withSpinner(plotOutput("CatchPlot", height = "500px")))
      #  #box(title = h4("Surface", style = 'font-size:20px;text-align:center; width:100%'),solidHeader = T,width=11 ,shinycssloaders::withSpinner(plotly::plotlyOutput("scatter", height = "500px")))
      # 
      #   box(title = "",solidHeader = T,width=11 ,
      #       shinycssloaders::withSpinner(plotly::plotlyOutput("scatter", height = "500px"),
      # ))))

      fluidRow(
        conditionalPanel(
          condition = paste0("input.", namespace, "FetchData > 0"),
        column(
          12,
          shinycssloaders::withSpinner(plotly::plotlyOutput("wqCatchPlot", height = "500px"))
          #shinycssloaders::withSpinner(plotOutput("wqCatchPlot", height = "500px")),
      )))
      )

    )
  )
}
