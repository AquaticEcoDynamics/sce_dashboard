#' addLegendCustom
#'
#' @description adds custom legend for catchment and estuary data
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


# colors <- c("white", "blue","blank")
# sizes <- c(10, 10, 10)
# labels <- c("Estuary sites","Catchment sites", "algal bloom")
# borders <- c("white", "blue","red")
# shapes <- c("circle", "circle", "circle")
# title2<- "Key"

colors <- c("#8da0cb","#a6d854")
sizes <- c(10, 10)
labels <- c("Estuary sites","Catchment sites")
borders <- c("white", "white")
shapes <- c("circle", "circle")
title2<- "Sites"


addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, opacity = 1,title){

  make_shapes <- function(colors, sizes, borders, shapes) {
    shapes <- gsub("circle", "50%", shapes)
    #shapes <- gsub("square", "0%", shapes)
    paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
  }
  make_labels <- function(sizes, labels) {
    paste0("<div style='display: inline-block;height: ",
           sizes, "px;margin-top: 4px;line-height: ",
           sizes, "px;'>", labels, "</div>")
  }

  legend_colors <- make_shapes(colors, sizes, borders, shapes)
  legend_labels <- make_labels(sizes, labels)

  return(leaflet::addLegend(map, position = "topleft", colors = legend_colors, labels = legend_labels, opacity = opacity,title = title2))
}

