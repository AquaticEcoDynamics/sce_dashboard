#' plotPointMulti
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

plotPointMulti <- function(plotData, plotDataX, plotDataY, plotDataGroup, plotLabelX, plotLabelY){
  # if (all(is.na(plotData[,plotDataY]))) {
  #   no_data_df <- data.frame(label = "data not found")
  #   plot <- ggplot2::ggplot(no_data_df, aes(x = 1, y = 1, label = label)) +
  #     geom_text(size = 10) +
  #     theme_void()
  # } else { 
    plot <- ggplot2::ggplot(
     data = plotData,
     mapping = ggplot2::aes(
      x = base::as.POSIXct(plotData[,plotDataX]),
      y = plotData[,plotDataY],
      colour = base::as.character(plotData[,plotDataGroup])
     )
  ) +
    ggplot2::geom_point() +
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
  # }
  plot
  
}

