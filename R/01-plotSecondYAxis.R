rescaleSecondAxisData <- function(plotData, individual, plotRanges) {
  # no rescaling if individual is not set
  if (length(individual) == 0 || individual == "")
    return(plotData)
  
  # get index for filter
  index <- plotData$individual == individual
  
  # no rescaling if individual is not found
  if (nrow(plotData[index, ]) == 0)
    return(plotData)
  
  # get rescaling factors
  rescaling <- calculateRescalingFactors(
    oldLimits = getScaleYLimits(plotData = plotData, scaleParams = plotRanges$yAxis),
    newLimits = getScaleYLimits(plotData = plotData, # (re-)scale to full data range
                                #plotData = plotData[index, ], # rescale to selected data range
                                scaleParams = plotRanges$yAxis2)
  )
  
  # rescale data for index
  scale <- rescaling$scale
  center <- rescaling$center
  
  plotData[index, ]$median <- (plotData[index, ]$median  - center) / scale
  plotData[index, ]$lower <- (plotData[index, ]$lower - center) / scale
  plotData[index, ]$upper <- (plotData[index, ]$upper  - center) / scale
  
  plotData
}

getScaleYLimits <- function(plotData, scaleParams) {
  if (scaleParams$fromData) {
    # use data based limits
    getYRange(plotData)
  } else {
    # use custom limits
    c(scaleParams$min, scaleParams$max)
  }
}

getSecAxisTitle <- function(modelName, customTitle) {
  # no second axis if modelName is not set
  if (is.null(modelName) || modelName == "") return(NULL)
  
  customTitle <- extractTitle(customTitle)
  if (is.null(customTitle) || customTitle == "") {
    modelName
  } else {
    customTitle
  }
}
