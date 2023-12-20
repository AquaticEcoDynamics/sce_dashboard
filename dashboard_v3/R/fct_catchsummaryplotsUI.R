#' catchsummaryplotsUI
#'
#' @description Generates the estuary summary plots UI elements e.g. date inputs, region. Requires @param namespace to individualise elements.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
library(plyr)
library(dplyr)
library (tidyr)
library(openxlsx)
library(ggplot2)
library(lubridate)
library(EnvStats)

wiskiEnable <- as.logical(get_golem_config("enable", config = "wiski_connection"))

if(isTRUE(wiskiEnable))
{
  Rawdata <- awss3Connect(filename = 'arms/wiski.csv')
}else{
  Rawdata <- read.csv(file = 'www/wiski.csv', check.names = FALSE)
}

dat<-Rawdata%>%
  select(1,2,3,4,5,6,7,8,11,12,13,14,21,24,27,30,33,36,39,47,48,50,51,53,54,56,57,59,60,63,66,68,69,71,72,77,78,86,87)

dat <-dat %>% rename(#Site.Ref = `ï»¿Site Ref`,
  Site.Ref = `Site Ref`,
  Time = `Collect Time`,
  Sample.Date = `Collect Date`, 
  Month = `Collect Month`, 
  Year = `Collect Year`,
  Project = `Program Code`, 
  Site = `Program Site Ref`, 
  Sal.ppt = `Salinity (ppt)`, 
  Temp= `Temperature (deg C)`, 
  ODO.mgl=`O2-{DO conc} (mg/L)`,
  ODO.sat=`O2-{DO %sat} (%)`, 
  pH = `pH (no units)`,
  Turbidity.NTU= `Turbidity (NTU)`,
  TON.mgl= `N (sum sol ox) {NOx-N TON} (ug/L)`,
  NH3.mgl= `NH3-N/NH4-N (sol) (ug/L)`,
  DON.mgl= `N (sum sol org) {DON} (ug/L)`,
  TKN.mgl= `N (tot kjel) {TKN} (ug/L)`,
  NO2.mgl= `NO2-N (sol) (ug/L)`,
  NO3.mgl= `NO3-N (sol) (ug/L)`,
  TP.mgl = `P (tot) {TP pTP} (ug/L)`,
  FRP.mgl= `PO4-P (sol react) {SRP FRP} (ug/L)`,
  DOC.mgl= `C (sol org) {DOC DOC as NPOC} (ug/L)`,
  TSS.mgl= `Suspended Solids (Total) {TSS} (mg/L)`,
  TN.mgl = `N (tot) {TN pTN} (ug/L)`,
  SPCcondus.cm = `Cond @ 25 deg C (uS/cm)`, 
  COC = `Primary CoC Number`)


catchdat<-dat%>%
  filter(Project %in% c("SG-C-SWANCATCH","SG-C-SCCATCH", "SG-C-SCWQIP"))

Sites_catch <- sort(unique(catchdat$Site))
years_catch <- sort(unique(dat$Year),decreasing = T)
vars_catch <- c("Sal.ppt", 
          "Temp", 
          "pH",
          "ODO.mgl",
          "ODO.sat",
          # "Turbidity.NTU",
          "TON.mgl",
          "NH3.mgl",
          "DON.mgl",
          "TKN.mgl",
          # "NO2.mgl",
          # "NO3.mgl",
          "TP.mgl",
          "FRP.mgl",
          "DOC.mgl",
          "TSS.mgl",
          "TN.mgl",
          "SPCcondus.cm")

catchsummaryplotsUI <- function(namespace){
  #library(plotly)

  
  return(
    tags$div(
      wellPanel(
        fluidRow(
          column(3,selectInput("select_site_catch",
                               label = "Select site" ,
                               choices = Sites_catch, 
                               selected = NULL),
                 style="margin-top: 50px;"),
          column(3,selectInput("select_year_catch",
                               label = "Select year" ,
                               choices = years_catch,  
                               selected = NULL),
                 style="margin-top: 50px;"),
          column(3,selectInput("select_vars_catch",
                               label = "Select variable" ,
                               choices = vars_catch,  
                               selected = NULL),
                 style="margin-top: 50px;"),
          column(2,actionButton(inputId = "wqCatchSumFetchData",
                                label = "Plot",
                                icon = icon("bar-chart"), 
                                style="margin-top: 83px; width: 100%"))
        ),
        fluidRow(
          conditionalPanel(
            condition = "input.wqCatchSumFetchData > 0",
          #column(12,shinycssloaders::withSpinner(plotly::plotlyOutput("catchsumPlot"))
            column(12,shinycssloaders::withSpinner(plotOutput("catchsumPlot", height = "600px"))
            )
          )
        )
      )
    )
  )
}

