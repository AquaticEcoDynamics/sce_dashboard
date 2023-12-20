#' estsummaryplotsUI
#'
#' @description Generates the estuary summary plots UI elements e.g. date inputs, region. Requires @param namespace to individualise elements.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


estsummaryplotsUI <- function(namespace){
  library(plotly)

  regions <- c("Swan","Canning")
  currentyear <- as.numeric(format(Sys.Date(), "%Y"))
  years <- sort(2010:currentyear,decreasing = T)
  vars <- c("TN", # Total N
                "NH4", # Dissolved Ammonia
                "TON", # Total oxidised nitrogen
                "DON", # Dissolved organic nitrogen
                "TP", # Total P
                "FRP", # Filterable reactive P
                "Si", # Reactive silica
                "DOC", # Dissolved organic carbon
                "TSS", # Total suspended solids
                "Alk", # Alkalinity
                "Chla", # Chl a
                "DO", # Dissolved oxygen
                "SpCond", # Sp conducdivity
                "TempC", # Temp
                "pH", # pH
                "Secchi") # Secchi

  return(
    tags$div(
      wellPanel(
        fluidRow(
          column(3,selectInput("select_region",
                               label = "Select region" ,
                               choices = regions, 
                               selected = NULL),
                               style="margin-top: 50px;"),
          column(3,selectInput("select_year",
                               label = "Select year" ,
                               choices = years,  
                               selected = NULL),
                               style="margin-top: 50px;"),
          column(3,selectInput("select_vars",
                               label = "Select variable" ,
                               choices = vars, 
                               selected = NULL),
                 style="margin-top: 50px;"),
          column(2,actionButton(inputId = "wqEstSumFetchData",
                                label = "Plot",
                                icon = icon("bar-chart"), 
                                style="margin-top: 83px; width: 100%"))
          ),
        fluidRow(
          conditionalPanel(
            condition = "input.wqEstSumFetchData > 0",
          #column(12,shinycssloaders::withSpinner(plotly::plotlyOutput("estsumPlot", height = "1000px"))
          column(12,shinycssloaders::withSpinner(plotOutput("estsumPlot", height = "1000px"))
          )
          )
        )
      )
    )
  )
}

