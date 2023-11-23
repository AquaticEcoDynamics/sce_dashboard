#' sensorMapMarkers_overview
#'
#' @description Based on the current tab panel, removes previous sensor markers from map and adds the current ones.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

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
                     "</b></br>"))
    
}