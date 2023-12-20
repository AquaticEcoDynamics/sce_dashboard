#' weathertimeseriesUI_S3
#'
#' @description Generates the weather plots UI elements e.g. date inputs, region. Requires @param namespace to individualise elements.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


weathertimeseriesUI_S3 <- function(namespace){
  
  # weathersites <- c("9021","9053")
  # weathervars <- c("Air_Temperature","Wind_Speed")
  dir <- 'data-warehouse/bom/idy'
  weathersites <- awss3Listfiles_sitevar(prefix = dir)$site
  weathervars <- awss3Listfiles_sitevar(prefix = dir)$variable
  
  return(
    tags$div(
      wellPanel(
        fluidRow(
          column(3,selectInput(inputId =paste0(namespace,"Sites"),
                               label = "Select Site" ,
                               choices = weathersites, 
                               selected = NULL),
                 style="margin-top: 50px;"),
          column(3,selectInput(inputId =paste0(namespace,"Variable"), 
                               label ="Select Variable:",
                               choices = weathervars,
                               #choices = NULL,  #these choices are given based on region selected (in app_server.R)
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
          column(12,shinycssloaders::withSpinner(plotly::plotlyOutput(outputId="weatherPlot", height = "500px")))
        )
      )
    )
  )
}
