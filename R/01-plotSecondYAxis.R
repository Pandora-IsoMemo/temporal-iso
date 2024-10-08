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

getScaleYLimits <- function(plotData, scaleParams) {
  if (scaleParams$fromData) {
    # use data based limits
    getYRange(plotData) %>% unlist()
  } else {
    # use custom limits
    c(ymin = scaleParams$min, ymax = scaleParams$max)
  }
}

getRescaleParams <- function(oldLimits, newLimits = NULL, secAxis = FALSE) {
  if (length(newLimits) == 0 || !secAxis) return(list(scale = 1, center = 0))
  
  b <- seq(min(newLimits),max(newLimits), length.out = 100)
  a <- seq(min(oldLimits),max(oldLimits), length.out = 100)
  res <- lm(b~a)
  
  list(scale = res$coefficients[2],
       center = res$coefficients[1])
}

addSecAxisTitle <- function(paramList, modelName, customTitle) {
  customTitle <- extractTitle(customTitle)
  if (is.null(customTitle) || customTitle == "") {
    title <- modelName
  } else {
    title <- customTitle
  }
  
  c(title = title, paramList)
}
