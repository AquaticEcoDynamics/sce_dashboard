#' microalgaeMapMarkers
#'
#' @description make microalgae map markers and colour by chla concentration.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' 
#rawwiski <- awss3Connect(filename = 'arms/wiski.csv')
#rawwiski <- read.csv(file = 'www/wiski.csv', check.names = FALSE)

wiskiEnable <- as.logical(get_golem_config("enable", config = "wiski_connection"))

if(isTRUE(wiskiEnable))
{
  rawwiski <- awss3Connect(filename = 'arms/wiski.csv')
}else{
  rawwiski <- read.csv(file = 'www/wiski.csv', check.names = FALSE)
}

rawwiski$`Collect Date` <- as.Date(rawwiski$`Collect Date`,format="%d/%m/%Y")
algaesites <- c("BLA", "ARM", "HEA", "NAR", "NIL", "STJ", "MAY", "RON", "KIN", 
                "SUC", "WMP", "MSB",  #swan
                "SCB2", "SAL", "SHELL", "RIV", "CASMID", "KEN", "BAC", "NIC", "ELL") #canning

algaedf <- rawwiski %>%
  dplyr::filter(`Program Site Ref` %in% algaesites &
                  `Collection Method` %in% 'Integrated over depth'&
                  `Data Category` %in% 'Laboratory results')%>%
  dplyr::select(`Program Site Ref`, `Collect Date`,`Collect Month`,`Collect Year`, `Chlorophyll a (by vol) (mg/L)`)
algaedf$chla_ugL <- algaedf$`Chlorophyll a (by vol) (mg/L)`*1000



Estuary_data <- read.csv("www/SCESTUARY - Copy.csv")
Estuary_data <- Estuary_data %>%
  rename('Program Site Ref' = Project.Site.Reference)

algaedf_join <- inner_join(algaedf,Estuary_data,by = 'Program Site Ref')


microalgaeMapMarkers <- function(mapID,plotDataWeek){
  
  selectedData <- algaedf_join %>%  
    dplyr::filter(`Collect Date` %in% as.Date(plotDataWeek,format="%Y-%m-%d"))%>%
    dplyr::mutate(category = case_when(
                  chla_ugL < 4 ~ "LOW",
                  chla_ugL <= 10 ~ "MEDIUM",
                  chla_ugL > 10 ~ "HIGH",
                  TRUE ~ "LOW"))
  
  getColor <- function(selectedData) {
    sapply(selectedData$chla_ugL, function(chla_ugL) {
      if(chla_ugL < 4) {
        "lightblue"
      } else if(chla_ugL <= 10) {  #there's no option 'yellow'....
        "orange"
      } else if(chla_ugL > 10) {
        "red"
      } else {
        "gray"
      } })
  }
  
  getIcon <- function(chla_ugL) {
    sapply(selectedData$chla_ugL, function(chla_ugL) {
    if (chla_ugL < 4) {
      'face-smile'
    } else if (chla_ugL <= 10) {
      'face-frown-open'
    } else if (chla_ugL > 10) {
      'face-dizzy'
    } else {
      'face-meh-blank'
    } })
  }
  
  
  icons <- awesomeIcons(
    #icon = 'ios-close',
    #icon = 'circle-dot',
    #icon = 'face-dizzy',
    icon = getIcon(selectedData),
    iconColor = 'black',
    #library = 'ion',
    library = 'fa',
    markerColor = getColor(selectedData)
  )
  
  # legend_labels <- c("ALERT", "High (>10 ug/L)", "Medium (4-10 ug/L)", "Low (<4 ug/L)", "Unknown")
  # legend_colors <- c("darkred", "red", "orange", "lightblue", "gray")
  

  leaflet::leafletProxy(
    mapId = mapID,
    data = selectedData
  ) %>% 
    leaflet::clearMarkers() %>% 
    leaflet::addAwesomeMarkers(
      lng = selectedData[["Longitude"]], 
      lat = selectedData[["Latitude"]], 
      icon = icons,
      label = selectedData[["category"]],
      #labelOptions = labelOptions(noHide = TRUE),
      popup = paste0("<b>", paste0("<img src=' ",selectedData[["URL"]]," ' width='100px' height='100px'>"),"</b></br>",
                     paste0("<b>Chlorophyll-a (ug/L): </b>", selectedData[["chla_ugL"]] ),"</b></br>",
                     paste0("<b>Name: </b>", selectedData[["Site.Name"]] ),"</b></br>",
                     paste0("<b>Site code: </b>", selectedData[["Program Site Ref"]] ),"</b></br>",
                     paste0("<b>Site. Ref: </b>", selectedData[["Site.Ref"]]),
                     "</b></br>")
    )
    
}