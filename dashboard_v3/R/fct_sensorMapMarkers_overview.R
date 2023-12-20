#' sensorMapMarkers_overview
#'
#' @description Based on the current tab panel, removes previous sensor markers from map and adds the current ones.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

#make legends
colors <- c('#66c2a5','#ffd92f',"#a6d854","#8da0cb",'#cc99cc',"#fc8d62")
labels <- c('Hydrology','Weather','Water quality - Catchment','Water quality - Estuary', 'Water quality - Mooring', 'Oxygenation plant')
sizes <- c(15,15,15,15,15,15)
shapes <- c("circle", "circle", "circle","circle", "circle", "circle")
borders <- c("#FFFFFF", "#FFFFFF","#FFFFFF", "#FFFFFF", "#FFFFFF","#FFFFFF")
title2 <- "Monitoring sites"

sensorMapMarkers_overview <- function(mapID, Data){
  
  
  leaflet::leafletProxy(
    mapId = mapID,
    data = Data
  ) %>% 
    leaflet::clearMarkers() %>% 
    leaflet::addCircleMarkers(
      lng = Data[["lon"]], 
      lat = Data[["lat"]], 
      color = "white", 
      radius = 5, 
      weight = 2, 
      fillColor = Data[["colour"]], 
      opacity = 1, 
      fillOpacity = 1,
      popup = paste0("<b>", paste0("<img src=' ",Data[["URL"]]," ' width='100px' height='100px'>"),"</b></br>",
                     #paste0("<b>Name: </b>", Data[["fullname"]] ),"</b></br>",
                     paste0("<b>Site name: </b>", Data[["name"]] ),"</b></br>",
                     paste0("<b>Site ID: </b>", Data[["id"]]),"</b></br>",
                     paste0("<b>Group: </b>", Data[["group"]]),"</b></br>",
                     paste0("<b>Agency: </b>", Data[["agency"]]),
                     "</b></br>")) %>%
    # leaflet::addLegend(
    #   #map = mapID,
    #   position = "topleft",  # Adjust position as needed
    #   colors = c('#66c2a5','#ffd92f','#7b1fa2',"#a6d854","#8da0cb","#fc8d62"),  # Use unique colors from the data
    #   labels = c('Hydrology','Weather','Water quality - mooring','Water quality - catchment','Water quality - estuary', 'Oxygenation plant') ,  # Use unique groups from the data
    #   title = "Monitoring sites"
    # )
    addLegendCustom(colors, labels, sizes,shapes,borders,title=title2)
  
}