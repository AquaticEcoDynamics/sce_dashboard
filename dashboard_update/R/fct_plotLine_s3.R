#' plotLine_s3 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

plotLine_s3 <- function(plotData, plotDataX, plotDataY, plotDataColours, plotLabelX, plotLabelY){
  plot <- ggplot2::ggplot(
    data = plotData,
    mapping = ggplot2::aes(
      x = base::as.POSIXct(plotData[,plotDataX]),
      y = plotData[,plotDataY],
#      colour = base::as.character(plotData[,plotDataGroup])
    )
  ) +
    ggplot2::geom_line() +
    # ggplot2::scale_colour_manual(   #commented out by Matt as not working with docker?
    #   values = plotDataColours
    # ) +
    ggplot2::xlab(plotLabelX) +
    ggplot2::ylab(plotLabelY) +
    ggplot2::theme_light(
      base_size = 14
    ) +
    ggplot2::theme(
      legend.position = "none"
    )
  plot
}

