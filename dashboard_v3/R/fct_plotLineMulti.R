#' plotLineMulti
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

plotLineMulti <- function(plotData, plotDataX, plotDataY, plotDataGroup, plotLabelX, plotLabelY){
  
#  if (nrow(plotData) > 0) {
  
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
  
  # }else{
  #   # if no data print an error message
  #   no_data_df <- data.frame(label = "data unavailable: this may be caused by invalid site/variable combination")
  #   errorplot <- ggplot2::ggplot(no_data_df, aes(x = 1, y = 1, label = label)) +
  #     geom_text(size = 10) +
  #     theme_void()
  #   
  #   errorplot
  # }
  

}

