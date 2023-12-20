#' sensorMapMarkerHighlight_wq
#'
#' @description highlight site on map when selected to plot
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

#turned layer control off for testing on docker

sensorMapMarkerHighlight_wq <- function(mapID, subGroupData, selectedData, hlcolour){
  
  leaflet::leafletProxy(
    mapId = mapID,
    data = subGroupData
  ) %>% 
    leaflet::clearMarkers() %>% 
    leaflet::addCircleMarkers(
      lng = subGroupData[["lon"]], 
      lat = subGroupData[["lat"]], 
      color = "white", 
      radius = 5, 
      weight = 2, 
      fillColor = subGroupData[["colour"]], 
      opacity = 1, 
      fillOpacity = 1,
      popup = paste0("<b>", paste0("<img src=' ",subGroupData[["URL"]]," ' width='100px' height='100px'>"),"</b></br>",
                     paste0("<b>Name: </b>", subGroupData[["fullname"]] ),"</b></br>",
                     paste0("<b>Site code: </b>", subGroupData[["name"]] ),"</b></br>",
                     paste0("<b>Site. Ref: </b>", subGroupData[["id"]]),
                     "</b></br>"))%>% 
    # leaflet::addLayersControl(baseGroups = c("Ecological Managment Zones","Subcatchments", "Profile", "Elevation", "All off"), 
    #                           #overlayGroups = c("Estuary sites","Catchment sites"),position="topright",
    #                           options = leaflet::layersControlOptions(collapsed = FALSE)) %>%

  leaflet::addCircleMarkers(
  data = selectedData,
  lng = as.numeric(selectedData[["lon"]]),
  lat = as.numeric(selectedData[["lat"]]),
  #label = ~name,
  radius = 5,
  stroke = TRUE,
  color = hlcolour,
  weight = 10,
  fillColor = selectedData[["colour"]],
  fillOpacity = 1,
  popup = paste0("<b>", paste0("<img src=' ",selectedData[["URL"]]," ' width='100px' height='100px'>"),"</b></br>",
                 paste0("<b>Name: </b>", selectedData[["fullname"]] ),"</b></br>",
                 paste0("<b>Site code: </b>", selectedData[["name"]] ),"</b></br>",
                 paste0("<b>Site. Ref: </b>", selectedData[["id"]]),
                 "</b></br>")
)
}