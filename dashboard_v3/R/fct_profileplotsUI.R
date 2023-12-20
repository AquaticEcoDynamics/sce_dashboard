#' profileplotsUI
#'
#' @description Generates the profile plots UI elements e.g. date inputs, region. Requires @param namespace to individualise elements.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


profileplotsUI <- function(namespace){

  library(slickR)
  regions <- c("Swan","Canning")

  return(
    tags$div(
      wellPanel(
        fluidRow(
          column(3,selectInput("select_region",
                               label = "Select region" ,
                               choices = regions, 
                               selected = NULL),
                               style="margin-top: 50px;"),
          column(3,selectInput("select_week",
                               label = "Select week" ,
                               choices = NULL,  #these choices are given based on region selected (in app_server.R)
                               selected = NULL),
                               style="margin-top: 50px;"),
          column(2,actionButton(inputId = "wqProfFetchData",
                                label = "Plot",
                                icon = icon("bar-chart"), 
                                style="margin-top: 83px; width: 100%")),
          #shinycssloaders::withSpinner(imageOutput(outputId="profPlot"))
          shinycssloaders::withSpinner(slickROutput(outputId="profPlot"))
        )
      )
    )
  )
}

