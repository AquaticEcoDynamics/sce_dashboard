#' webMap 
#'
#' @description Generates an empty leaflet web-map centered on Perth.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd



##testing for docker - turned all shp layers off 

FS<- rgdal::readOGR("www/new_all.shp",layer = "new_all", GDAL1_integer64_policy = TRUE)
profile<-rgdal::readOGR("www/Profile.shp",layer = "Profile", GDAL1_integer64_policy = TRUE)
Catchbound <- rgdal::readOGR("www/thirty_subcatchment_GDA2020_MGA50.shp",layer = "thirty_subcatchment_GDA2020_MGA50", GDAL1_integer64_policy = TRUE)
# #data <- read.csv("www/temp1-attributes.csv") ## each table with attributes
# x<-"www/Raster_image_DEM_reproj.tif"
# Bathy<-raster::raster(x, layer=1, values=TRUE)
# bathypal <- leaflet::colorNumeric(
#   #palette = colorRamp(c("#9E0142", "#D53E4F" ,"#F46D43",  "#FDAE61"   ,"#FEE08B", "#FFFFBF" ,"#ABDDA4" , "#66C2A5" ,"#3288BD"  )), # span of values in palette
#   palette = "Blues",
#   domain= c(Bathy@data@max,Bathy@data@min),#range of values
#   na.color = "transparent",
#   reverse = TRUE)
PRO <- sp::CRS('+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')
# 
DAT6 <- sp::spTransform(FS,PRO)  ### this sets the shapefile to have the same coordinates as the map
DATP <- sp::spTransform(profile,PRO)  ### this sets the shapefile to have the same coordinates as the map
DATCatchbound <- sp::spTransform(Catchbound,PRO) 
# 
# 
factpal <- leaflet::colorFactor('viridis', DAT6$EMZ,reverse = FALSE) 
profpal <-leaflet::colorFactor('viridis',DATP$Name)
catchpal <- leaflet::colorFactor('viridis',DATCatchbound$CATCH_NAME)
# # bathypal <- leaflet::colorNumeric('viridis', Bathy,
# #                     na.color = "transparent")
# 
# 
# #Define HTML for the infobox


webMap <- function(){
  
  leaflet::leaflet() %>%
    leaflet::addTiles(urlTemplate = 'https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}') %>%
    leaflet::setView(115.8613, -31.9523, 10) %>%
    # leaflet::addRasterImage(Bathy, colors = bathypal, group= "Elevation")%>%
    # leaflet::addLegend(position = "topleft",pal = bathypal, values = c(Bathy@data@max,Bathy@data@min), title = "Elevation (m)")%>%
    leaflet::addPolygons(data = DAT6, fill = TRUE, stroke = TRUE, color= ~factpal(EMZ), fillOpacity = .8,
                       group = "Ecological Managment Zones", label = ~River)%>%
    leaflet::addPolygons(data = DATCatchbound, fill = TRUE, stroke = TRUE, weight = 3, color= 'white' , fillOpacity = .3,
                         group = "Subcatchments", label = ~CATCH_NAME) %>%
    leaflet::addPolylines(data = DATP, stroke = T, weight = 4,opacity = 1,
                          color= c("orange","yellow"),
                          group = "Profile", label = ~Name)%>%
    # htmlwidgets::onRender("
    #     function() {
    #         $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:left\">Sites</label>');
    #         $('.leaflet-control-layers-base').prepend('<label style=\"text-align:left\">Map layers</label>');
    #     }
    # ") %>%
    # 
    # 
    # 
    # ## https://github.com/rstudio/leaflet/issues/477
    # 
    # leaflet::hideGroup(c("Estuary sites","Catchment sites","Ecological Managment Zones","Estuary depth","Habitat"))
    leaflet::addLayersControl(baseGroups = c("Ecological Managment Zones","Subcatchments", "Profile", "Elevation", "All off"),
                            #overlayGroups = c("Estuary sites","Catchment sites"),position="topright",
                            options = leaflet::layersControlOptions(collapsed = FALSE))


}

