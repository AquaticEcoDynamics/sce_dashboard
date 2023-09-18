#' webMap 
#'
#' @description Generates an empty leaflet web-map centered on Perth.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

# webMap <- function(){
#   leaflet::leaflet() %>%
#     leaflet::addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>%
#     leaflet::setView(115.8613, -31.9523, 9) 
# }

FS<- rgdal::readOGR("www/new_all.shp",layer = "new_all", GDAL1_integer64_policy = TRUE)
profile<-rgdal::readOGR("www/Profile.shp",layer = "Profile", GDAL1_integer64_policy = TRUE)
#data <- read.csv("www/temp1-attributes.csv") ## each table with attributes
x<-"www/Raster_image_DEM_reproj.tif"
Bathy<-raster::raster(x, layer=1, values=TRUE)
bathypal <- leaflet::colorNumeric(
  #palette = colorRamp(c("#9E0142", "#D53E4F" ,"#F46D43",  "#FDAE61"   ,"#FEE08B", "#FFFFBF" ,"#ABDDA4" , "#66C2A5" ,"#3288BD"  )), # span of values in palette
  palette = "Blues",
  domain= c(Bathy@data@max,Bathy@data@min),#range of values
  na.color = "transparent",
  reverse = TRUE)
PRO <- sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')

DAT6 <- sp::spTransform(FS,PRO)  ### this sets the shapefile to have the same coordinates as the map
DATP <- sp::spTransform(profile,PRO)  ### this sets the shapefile to have the same coordinates as the map


factpal <- leaflet::colorFactor('viridis', DAT6$EMZ,reverse = FALSE) 
profpal <- leaflet::colorFactor('viridis', DATP$Name)
# bathypal <- leaflet::colorNumeric('viridis', Bathy,
#                     na.color = "transparent")


#Define HTML for the infobox


webMap <- function(){
  
  leaflet::leaflet() %>%
    leaflet::addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>%
    leaflet::setView(115.8613, -31.9523, 11)%>%
    # leaflet::addTiles(urlTemplate = "/mytiles/{z}/{x}/{y}.png",data=Bathy, group= "Estuary depth")%>%
   # leaflet::addRasterImage(Bathy, colors = bathypal, group= "Elevation")%>%
   # leaflet::addLegend(position = "topleft",pal = bathypal, values = c(Bathy@data@max,Bathy@data@min), title = "Elevation (m)")%>%
    # leaflet::addCircleMarkers(data = Estuary_data, lng= ~Longitude, lat=~Latitude,
    #                           radius = 3, color="white",group = "Estuary sites",
    #                           fillOpacity = 0.9, popup = ~paste("<b>", paste0("<img src=' ",Estuary_data$URL ," ' width='100px' height='100px'>"),"</b></br>",                                                                                                                                   ,actionLink("Water", "More on WQ", onclick = 'Shiny.onInputChange(\"Link_click\",  Math.random())')
    #                           ),label = ~Project.Site.Reference)%>%
    # leaflet::addCircleMarkers(data = Catchment_data, lng= ~Longitude, lat=~Latitude,
    #                           radius = 3, color="blue" ,group = "Catchment sites",
    #                           fillOpacity = 0.9,    popup = ~paste("<b>", paste0("<img src=' ",Catchment_data$URL," ' width='100px' height='100px'>"),"</b></br>",                                                                                                                                                                             ,actionLink("Water", "More on WQ", onclick = 'Shiny.onInputChange(\"Link_click\",  Math.random())')
    #                           ),label = ~Site_Code)%>%
    leaflet::addPolygons(data = DAT6, fill = TRUE, stroke = TRUE, color= ~factpal(EMZ), fillOpacity = .8, 
                       group = "Ecological Managment Zones", label = ~River)%>%
    # leaflet::addPolylines(data = DATP, stroke = T, 
    #                       color= ~profpal(Name), 
    #                       group = "Profile", label = ~Name)%>%
      # leaflet::addLayersControl(baseGroups = c("Ecological Managment Zones","Elevation","Profile","All off"), 
      #                           # overlayGroups = c("Estuary sites","Catchment sites"),
      #                           position="topright",
      #                           options = leaflet::layersControlOptions(collapsed = FALSE))%>%
    htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:left\">Sites</label>');
            $('.leaflet-control-layers-base').prepend('<label style=\"text-align:left\">Map layers</label>');
        }
    ")%>%
    

    
    ## https://github.com/rstudio/leaflet/issues/477
   
    leaflet::hideGroup(c("Estuary sites","Catchment sites","Ecological Managment Zones","Estuary depth","Habitat"))

}

