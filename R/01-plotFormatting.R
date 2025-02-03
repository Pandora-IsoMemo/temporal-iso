getStyleForIndividuals <- function(pointStyleList, input) {
  # pointStyleList are reactive values -> lapply over the names and not(!) the list itself
  style <- lapply(names(pointStyleList), function(individual) {pointStyleList[[individual]][input]}) %>% 
    unlist()
  names(style) <- names(pointStyleList)
  
  style
}

# Get the default point format for models
# 
# @param pointStyleList list of point styles for each model
# @param modelNames names of the models
# @param isFullReset if TRUE, all point styles are reset to the default values
getDefaultPointFormatForModels <- function(pointStyleList, modelNames, isFullReset = FALSE) {
  # default colours
  defaultColours <- ggplot2::scale_color_discrete()$palette(n = length(modelNames))
  names(defaultColours) <- modelNames
  
  # setup lists with default values for style specs
  for (i in modelNames) {
    if (is.null(pointStyleList[[i]]) || isFullReset) {
      pointStyleList[[i]] <- config()[["defaultPointStyle"]][["dataPoints"]]
      # use default colour per model
      pointStyleList[[i]]["color"] <- defaultColours[i]
    }
  }
  
  return(pointStyleList)
}

getDefaultTextFormat <- function() {
  list(plotTitle  = config()[["defaultIntervalTimePlotTitle"]],
       xAxisTitle = config()[["defaultIntervalTimePlotTitle"]],
       yAxisTitle = config()[["defaultIntervalTimePlotTitle"]],
       yAxisTitle2 = config()[["defaultIntervalTimePlotTitle"]],
       xAxisText  = config()[["defaultIntervalTimePlotText"]],
       yAxisText  = config()[["defaultIntervalTimePlotText"]],
       yAxisText2  = config()[["defaultIntervalTimePlotText"]])
}
