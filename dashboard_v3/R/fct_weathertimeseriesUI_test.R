#' weathertimeseriesUI_test
#'
#' @description Generates the weather plots UI elements e.g. date inputs, region. Requires @param namespace to individualise elements.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' 
#load("data/sensorslist_hydromet.rda")
methydro_data<-read.csv("www/Met_Hydro_sites.csv")

weathertimeseriesUI_test <- function(namespace){
  
  weathersites<- methydro_data %>%
    dplyr::filter(group %in% 'met') %>% 
    dplyr::select('Site')
  weathervars <- methydro_data %>%
    dplyr::filter(group %in% 'met') %>% 
    dplyr::select('s_graph_value')
  weathervars <- unique(weathervars$s_graph_value)
  
  return(
    tags$div(
      wellPanel(
        fluidRow(
          column(3,selectInput(inputId =paste0(namespace,"Sites"),
                               label = "Select Site(s):" ,
                               choices = weathersites, 
                               multiple = TRUE,
                               selected = NULL),
                 style="margin-top: 50px;"),
          column(3,selectInput(inputId =paste0(namespace,"Variable"), 
                               label ="Select Variable:",
                               choices = weathervars,
                               selected = NULL),
                 style="margin-top: 50px;"),
          column(2,dateInput(inputId = paste0(namespace,"DateFrom"),
                             label = "From:", 
                             value = Sys.Date()-7),
                 style="margin-top: 50px; "),
          column(2,dateInput(inputId = paste0(namespace,"DateTo"), 
                             label = "To:", 
                             value = Sys.Date()),
                 style="margin-top: 50px;"),
          column(2,actionButton(inputId = paste0(namespace,"FetchData"),
                                label = "Plot",
                                icon = icon("bar-chart"), 
                                style="margin-top: 83px; width: 100%"))
        ),
        fluidRow(
          column(12,uiOutput(outputId = paste0(namespace,"DateSlider")))
        ),
        #shinycssloaders::withSpinner(imageOutput(outputId="profPlot"))
        fluidRow(
          conditionalPanel(
            condition = paste0("input.", namespace, "FetchData > 0"),
          column(12,shinycssloaders::withSpinner(plotly::plotlyOutput(outputId="weathertestPlot", height = "500px")))
          )
        )
      )
    )
  )
}
