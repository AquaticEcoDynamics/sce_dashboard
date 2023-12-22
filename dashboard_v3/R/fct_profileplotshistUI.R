#' profileplotshistUI
#'
#' @description Generates the profile plots UI elements e.g. date inputs, region. Requires @param namespace to individualise elements.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
# rawwiski <- awss3Connect(filename = 'arms/wiski.csv')
# rawwiski$`Collect Date` <- as.Date(rawwiski$`Collect Date`,format="%d/%m/%Y")
#regions <- c("Swan","Canning")
#profdir <- "www/Profile_plotting/test/"

#subdir <- list.dirs(profdir, full.names = TRUE, recursive=FALSE)

#years <- sort(unique(rawwiski$`Collect Year`),decreasing = T)
# months <- 1:12
  


profileplotshistUI <- function(namespace){

  library(slickR)
  regions <- c("Swan","Canning")
  #currentyear <- as.numeric(format(Sys.Date(), "%Y"))
  #years <- sort(1999:currentyear,decreasing = T)
  

  return(
    tags$div(
      wellPanel(
        fluidRow(
          column(3,selectInput("select_region_profile_hist",
                               label = "Select region" ,
                               choices = regions, 
                               selected = NULL),
                 style="margin-top: 50px;"),
          column(2,selectInput("select_year_profile_hist",
                               label = "Select year" ,
                               #choices = years,
                               choices = NULL,  #these choices are given based on region selected (in app_server.R)  
                               selected = NULL),
                 style="margin-top: 50px;"),
          # column(2,selectInput("select_month_profile_hist",
          #                      label = "Select month" ,
          #                      choices = months,  
          #                      selected = NULL),
          #        style="margin-top: 50px;"),
          # column(3,selectInput("select_week_profile_hist",
          #                      label = "Select week" ,
          #                      choices = NULL,  #these choices are given based on region selected (in app_server.R)
          #                      selected = NULL),
          #        style="margin-top: 50px;"),
          column(2,actionButton(inputId = "wqProfHistFetchData",
                                label = "Plot",
                                icon = icon("bar-chart"), 
                                style="margin-top: 83px; width: 100%")),
          # column(2,actionButton(inputId = "wqProfHistClearData",
          #                       label = "Clear all",
          #                       icon = icon("xmark"), 
          #                       style="margin-top: 83px; width: 100%")),
          
          #shinycssloaders::withSpinner(imageOutput(outputId="profPlot"))
          fluidRow(
            column(12,
          conditionalPanel(
            condition = "input.wqProfHistFetchData > 0",
          shinycssloaders::withSpinner(slickROutput(outputId="profPlotHist",height = "940px" ))
                                                    #width = "500px"  #this crops the image instead of shrink it to size
          )
            )
          )
        )
      )
    )
  )
}

