#' plotLineMulti
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

plotLineMulti <- function(plotData, plotDataX, plotDataY, plotDataGroup, plotLabelX, plotLabelY){
  
  
  plot <- ggplot2::ggplot(
    data = plotData,
    mapping = ggplot2::aes(
      x = base::as.POSIXct(plotData[,plotDataX]),
      y = plotData[,plotDataY],
      colour = base::as.character(plotData[,plotDataGroup])
    )
  ) +
    ggplot2::geom_line() +
    #ggplot2::scale_colour_manual() +
    ggplot2::xlab(plotLabelX) +
    ggplot2::ylab(plotLabelY) +
    ggplot2::theme_light(
      base_size = 14
    ) +
    ggplot2::theme(
      legend.position = "bottom"
    ) +
    ggplot2::scale_colour_discrete(name  ="Site")
  plot
}

