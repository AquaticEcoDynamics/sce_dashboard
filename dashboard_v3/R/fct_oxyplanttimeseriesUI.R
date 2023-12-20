#' oxyplanttimeseriesUI
#'
#' @description Generates the common timeseries UI elements e.g. date inputs, slider. Requires @param namespace to individualise elements.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#load("data/sensorslist_oxy.rda")
Oxy_data<-read.csv("www/Oxy_sites.csv")

Oxy_sites<- Oxy_data$Project.Site.Reference
Oxy_variables<-unique(sensorslist_oxy$s_graph_value)

#### rename water quality variables and add additional column with units

#### for some variables convert units from ug/L to mg/L

#### Append original sign with variable


oxyplanttimeseriesUI <- function(namespace){
  
  library(plotly)
  return(
    
    tags$div(
      wellPanel(fluidRow(
        column(3,selectInput(
          inputId =paste0(namespace,"Sites"), label = "Select Site(s):", choices=Oxy_sites,multiple = TRUE),style="margin-top: 50px;"),
        column(3,selectInput(
          inputId =paste0(namespace,"variable"), label ="Select Variable:", choices = Oxy_variables),style="margin-top: 50px;"),
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
            shinycssloaders::withSpinner(plotly::plotlyOutput("wqDOPlot", height = "500px")),
            #shinycssloaders::withSpinner(plotOutput("wqCatchPlot", height = "500px")),
          )
          ))
      )
      
    )
  )
}
