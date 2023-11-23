#' sensorMapMarkerHighlight_methydro
#'
#' @description highlight site on map when selected to plot
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


sensorMapMarkerHighlight_methydro <- function(mapID, subGroupData, selectedData, hlcolour){
  
  leaflet::leafletProxy(
    mapId = mapID,
    data = subGroupData
  ) %>% 
    leaflet::clearMarkers() %>% 
    leaflet::addCircleMarkers(
      lng = subGroupData[["lon"]], 
      lat = subGroupData[["lat"]], 
      color = "white", 
      radius = 7, 
      weight = 2, 
      fillColor = subGroupData[["colour"]], 
      opacity = 1, 
      fillOpacity = 1,
      popup = paste0("<b>Station Name: </b>",subGroupData[["name"]],
                     "<br><b>Station ID: </b>", subGroupData[["id"]],
                     "<br><b>Agency: </b>", subGroupData[["agency"]])
    ) %>%
    leaflet::addCircleMarkers(
      data = selectedData,
      lng = as.numeric(selectedData[["lon"]]),
      lat = as.numeric(selectedData[["lat"]]),
      #label = ~name,
      radius = 7,
      stroke = TRUE,
      color = hlcolour,
      weight = 10,
      fillColor = selectedData[["colour"]],
      fillOpacity = 1,
      popup = paste0("<b>Station Name: </b>",selectedData[["name"]],
                     "<br><b>Station ID: </b>", selectedData[["id"]],
                     "<br><b>Agency: </b>", selectedData[["agency"]])
    )
}