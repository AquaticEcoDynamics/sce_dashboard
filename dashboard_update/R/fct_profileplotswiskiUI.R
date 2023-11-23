#' profileplotswiskiUI
#'
#' @description Generates the profile plots UI elements e.g. date inputs, region. Requires 
#' @param namespace to individualise elements.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' 
rawwiski <- awss3Connect(filename = 'arms/wiski.csv')
rawwiski$`Collect Date` <- as.Date(rawwiski$`Collect Date`,format="%d/%m/%Y")
regions <- c("Swan","Canning")
years <- sort(unique(rawwiski$`Collect Year`),decreasing = T)
months <- 1:12

swansites <- c('ARM','BLA','FP1','FP7','FP22','CAV','HEA','NAR','NIL',
               'STJ','MAY','RON','KIN','SUC','MEAD','MULB','WMP',
               'REG','MSB','POL','SANDBR','KMO','JBC','VIT')
cannsites <- c('BAC','BACD300','BACD500','BACU300','CASMID','ELL','GRE','KEN',
              'KENU300','KS7','KS9','MACD50','MASD50','NIC','NIC-IN',
              'NICD200','PAC','PO2','RIV','SAL','SCB2')



profileplotswiskiUI <- function(namespace){
  
  #library(slickR)

  return(
    tags$div(
      wellPanel(
        fluidRow(
          column(3,selectInput("select_region_profile",
                               label = "Select region" ,
                               choices = regions, 
                               selected = NULL),
                 style="margin-top: 50px;"),
          column(2,selectInput("select_year_profile",
                               label = "Select year" ,
                               choices = years,  
                               selected = NULL),
                 style="margin-top: 50px;"),
          column(2,selectInput("select_month_profile",
                               label = "Select month" ,
                               choices = months,  
                               selected = NULL),
                 style="margin-top: 50px;"),
          column(3,selectInput("select_week_profile",
                               label = "Select week" ,
                               choices = NULL,  #these choices are given based on region selected (in app_server.R)
                               selected = NULL),
                 style="margin-top: 50px;"),
          column(2,actionButton(inputId = "wqProfFetchData",
                                label = "Plot",
                                icon = icon("bar-chart"), 
                                style="margin-top: 83px; width: 100%")),
          # shinycssloaders::withSpinner(plotly::plotlyOutput(outputId="profPlotWiski1",height = "300px")),
          # shinycssloaders::withSpinner(plotly::plotlyOutput(outputId="profPlotWiski2",height = "300px")),
          # shinycssloaders::withSpinner(plotly::plotlyOutput(outputId="profPlotWiski3",height = "300px")),
          # shinycssloaders::withSpinner(plotly::plotlyOutput(outputId="profPlotWiski4",height = "350px"))
          shinycssloaders::withSpinner(plotOutput(outputId="profPlotWiski1",height = "280px")),
          shinycssloaders::withSpinner(plotOutput(outputId="profPlotWiski2",height = "280px")),
          shinycssloaders::withSpinner(plotOutput(outputId="profPlotWiski3",height = "280px")),
          shinycssloaders::withSpinner(plotOutput(outputId="profPlotWiski4",height = "320px")),
          shinycssloaders::withSpinner(plotOutput(outputId="profPlotWiski1z",height = "280px")),
          shinycssloaders::withSpinner(plotOutput(outputId="profPlotWiski2z",height = "280px")),
          shinycssloaders::withSpinner(plotOutput(outputId="profPlotWiski3z",height = "280px")),
          shinycssloaders::withSpinner(plotOutput(outputId="profPlotWiski4z",height = "320px"))
        )
      )
    )
  )
}

