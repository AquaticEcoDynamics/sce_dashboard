#' microalgaeactivityUI
#'
#' @description Generates the profile plots UI elements e.g. date inputs, region. Requires 
#' @param namespace to individualise elements.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' 
#rawwiski <- awss3Connect(filename = 'arms/wiski.csv')

wiskiEnable <- as.logical(get_golem_config("enable", config = "wiski_connection"))

if(isTRUE(wiskiEnable))
{
  rawwiski <- awss3Connect(filename = 'arms/wiski.csv')
}else{
  rawwiski <- read.csv(file = 'www/wiski.csv', check.names = FALSE)
}

rawwiski$`Collect Date` <- as.Date(rawwiski$`Collect Date`,format="%d/%m/%Y")

# set up site df's in correct upriver order
# algaesites <- c("BLA", "ARM", "HEA", "NAR", "NIL", "STJ", "MAY", "RON", "KIN", 
#              "SUC", "WMP", "MSB",  #swan
#              "SCB2", "SAL", "SHELL", "RIV", "CASMID", "KEN", "BAC", "NIC", "ELL") #canning


years <- sort(unique(rawwiski$`Collect Year`),decreasing = T)
months <- 1:12




microalgaeactivityUI <- function(namespace){
  
  return(
    tags$div(
      wellPanel(
        fluidRow(
          # column(3,selectInput("select_region_profile",
          #                      label = "Select region" ,
          #                      choices = regions, 
          #                      selected = NULL),
          #        style="margin-top: 50px;"),
          column(2,selectInput("select_year_algae",
                               label = "Select year" ,
                               choices = years,  
                               selected = NULL),
                 style="margin-top: 50px;"),
          column(2,selectInput("select_month_algae",
                               label = "Select month" ,
                               choices = months,  
                               selected = NULL),
                 style="margin-top: 50px;"),
          column(3,selectInput("select_week_algae",
                               label = "Select week" ,
                               choices = NULL,  #these choices are given based on year/month selected (in app_server.R)
                               selected = NULL),
                 style="margin-top: 50px;"),
          column(2,actionButton(inputId = "AlgaeFetchData",
                                label = "Map",
                                icon = icon("map-location-dot"), 
                                style="margin-top: 83px; width: 100%"))
        )
      )
    )
  )
}

