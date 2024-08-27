setSecondYAxis <- function(plot, 
                           rescaling,
                           titleFormat = NULL,
                           textFormat = NULL,
                           yAxisLabel = "Estimate",
                           yAxisTitleColor = NULL) {
  if (identical(rescaling, list(scale = 1, center = 0))) return(plot)
  
  scale <- rescaling$scale
  center <- rescaling$center
  
  # format equal to first axis:
  if (is.null(titleFormat)) titleFormat <- config()[["defaultIntervalTimePlotTitle"]]
  if (is.null(textFormat)) textFormat <- config()[["defaultIntervalTimePlotText"]]
  # custom format for second axis:
  if (is.null(yAxisTitleColor)) yAxisTitleColor <- config()[["defaultIntervalTimePlotTitle"]][["color"]]
  
  plot <- plot + 
    theme(axis.title.y.right = element_text(family = titleFormat[["fontFamily"]],
                                            size = titleFormat[["size"]],
                                            face = titleFormat[["fontType"]],
                                            color = yAxisTitleColor,
                                            hjust = 0.5),
          axis.text.y.right = element_text(family = textFormat[["fontFamily"]],
                                           size = textFormat[["size"]],
                                           face = textFormat[["fontType"]],
                                           color = textFormat[["color"]],
                                           hjust = 0.5)) +
    scale_y_continuous(
      # Features of the first axis
      # Add a second axis and specify its features
      sec.axis = sec_axis(~(.* scale) + center, name = yAxisLabel)
    )
  
  plot
}

rescaleSecondAxisData <- function(plotData, individual, rescaling) {
  if (is.null(individual) || individual == "") return(plotData)
  
  # get index for filter
  index <- plotData$individual == individual
  
  if (nrow(plotData[index, ]) == 0 ||
      identical(rescaling, list(scale = 1, center = 0))) return(plotData)
  
  # rescale data
  scale <- rescaling$scale
  center <- rescaling$center
  
  plotData[index, ]$median <- (plotData[index, ]$median  - center ) / scale
  plotData[index, ]$lower <- (plotData[index, ]$lower - center ) / scale
  plotData[index, ]$upper <- (plotData[index, ]$upper  - center ) / scale
  
  plotData
}

getRescaleParams <- function(oldLimits, newLimits = NULL, secAxis = FALSE) {
  if (length(newLimits) == 0 || !secAxis) return(list(scale = 1, center = 0))
  
  b <- seq(min(newLimits),max(newLimits), length.out = 100)
  a <- seq(min(oldLimits),max(oldLimits), length.out = 100)
  res <- lm(b~a)
  
  list(scale = res$coefficients[2],
       center = res$coefficients[1])
}

getSecondAxisTitle <- function(secAxisTitle, secAxisModel) {
  if (is.null(secAxisModel) || secAxisModel == "") return("")
  
  if (secAxisTitle == "") return(sprintf("%s Estimate", secAxisModel))
  
  return(secAxisTitle)
}
