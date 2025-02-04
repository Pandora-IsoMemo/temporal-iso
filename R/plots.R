#' Plot of credibility intervals for each time interval, plotted as timeseries
#' 
#' @description The function plots the credibility intervals for each
#' time interval and returns a ggplot object, which is further customizable.
#' 
#' @param object model of class \code{\link{TemporalIso}}
#' @param prop double between 0 and 1: length of credibility interval. The default value is 80 percent.
#' @param plotShifts boolean if shifts shall be marked or not. Defaults to False.
#' @param yLim numeric vector of length 2: range of y axis
#' @param xLim numeric vector of length 2: range of x axis
#' @param deriv character "1" for absolute values, "2" for first differences
#' @param color color of line and uncertainty region (RGB)
#' @param alphaL opacity line
#' @param alphaU opacity uncertainty region
#' @param sizeTextY size plot y label
#' @param xAxisLabel character label x-axis
#' @param yAxisLabel character label y-axis
#' @param sizeTextX size text x label
#' @param sizeAxisX size text x axis label
#' @param sizeAxisY size text y axis label
#' @param secAxis boolean secondary axis on right side?
#' @param extendLabels boolean if TRUE, extend the labels of the x-axis to the x-axis limits. 
#'  If FALSE, the range of the data defines the range of x-axis labels.
#' @param ... arguments handed to \code{\link{getShiftTime}}
#' @inheritParams shinyTools::formatPointsOfGGplot
#' 
#' @return a \link[ggplot2]{ggplot} object.
#' 
#' @export
plotTime <- function(object, prop = 0.8, plotShifts = FALSE,
                     yLim = c(0,1), xLim = c(0,1),
                     deriv = "1", 
                     color = NULL, alphaL = 0.9, alphaU = 0.1,
                     sizeTextY = 12, sizeTextX = 12, sizeAxisX = 12, sizeAxisY = 12, secAxis = FALSE,
                     xAxisLabel = "Time", yAxisLabel = "Estimate", extendLabels = FALSE,
                     pointStyle = config()[["defaultPointStyle"]],
                     ...){
  stopifnot(prop < 1)

  x <- getPlotData(object, prop = prop, deriv = deriv) %>%
    updateTime(object = object, deriv = deriv)
  
  x$individual <- "current"  
  pointStyleList <- list()
  pointStyleList[["current"]] <- config()[["defaultPointStyle"]][["dataPoints"]]
  pointStyleList[["current"]]["color"] <- color
  
  p <- basePlotTime(df = x,
                    xLim = xLim, yLim = yLim,
                    sizeTextX = sizeTextX, sizeTextY = sizeTextY,
                    sizeAxisX = sizeAxisX, sizeAxisY = sizeAxisY) %>%
    setDefaultTitles(prop, xAxisLabel, yAxisLabel)
  
  p <- p %>%
    drawLinesAndRibbon(pointStyleList = pointStyleList, alphaL = alphaL, alphaU = alphaU) %>%
    setXAxisLabels(xAxisData = getXAxisData(object = object),
                   extendLabels = extendLabels, 
                   xLim = xLim, 
                   deriv = deriv) %>%
    drawShiftLines(object = object, 
                   deriv = deriv,
                   plotShifts = plotShifts,
                   ...)
  
  p + theme(legend.position = "none")
}

basePlotTime <- function(df,
                         xLim = NULL, yLim = NULL, 
                         sizeTextX = 12, sizeTextY = 12, 
                         sizeAxisX = 12, sizeAxisY = 12) {
  if ("time" %in% colnames(df)) {
    p <- ggplot(df, aes(x = .data[["time"]]))
  } else {
    p <- ggplot(df)
  }
  
  p <- p + 
    theme(panel.grid.major.x = element_line(size = 0.1)) + # no error only in most recent version: element_line(linewidth = 0.1)
    theme(axis.title.x = element_text(size = sizeTextX),
          axis.text.x = element_text(size = sizeAxisX),
          axis.title.y = element_text(size = sizeTextY),
          axis.text.y = element_text(size = sizeAxisY))
  
  p <- p %>%
    setPlotLimits(xLim = xLim, yLim = yLim)
  
  p
}



drawLinesAndRibbon <- function(plot, pointStyleList, alphaL, alphaU, legend = NULL) {
  if (is.null(plot) || nrow(plot$data) == 0) return(plot)
  
  # draw lines "upper", "median", "lower" for each "individual"
  if (nrow(plot$data) > 1) {
    plot <- plot +
      geom_line(aes(y = .data[["median"]], colour = .data[["individual"]]), 
                alpha = alphaL) +
      geom_line(aes(y = .data[["lower"]], colour = .data[["individual"]]), 
                linewidth = 0.05, alpha = alphaL) + 
      geom_line(aes(y = .data[["upper"]], colour = .data[["individual"]]), 
                linewidth = 0.05, alpha = alphaL)
  } else {
    plot <- plot +
      geom_point(aes(y = .data[["median"]], colour = .data[["individual"]]),
                 alpha = alphaL) +
      geom_point(aes(y = .data[["lower"]], colour = .data[["individual"]]), 
                 size = 0.05, alpha = alphaL) +
      geom_point(aes(y = .data[["upper"]], colour = .data[["individual"]]), 
                 size = 0.05, alpha = alphaL)
  }
  
  # draw ribbon for each "individual"
  if (nrow(plot$data) > 1) {
    plot <- plot + 
      geom_ribbon(aes(ymin = .data[["lower"]], ymax = .data[["upper"]], 
                      fill = .data[["individual"]]), 
                  linetype = 2, alpha = alphaU)
  }
  
  # draw points "median" for each "individual"
  plot <- plot +
    geom_point(aes(y = .data[["median"]],
                   colour = .data[["individual"]],
                   shape = .data[["individual"]],
                   size = .data[["individual"]],
                   fill = .data[["individual"]]),
               alpha = alphaL)
  
  if (length(pointStyleList) == 0) return(plot)
  
  # set scales for each "individual" with default legend name
  lineColors <- getStyleForIndividuals(pointStyleList, input = "color")
  fillColors <- getStyleForIndividuals(pointStyleList, input = "color")
  pointShapes <- getStyleForIndividuals(pointStyleList, input = "symbol")
  pointSize <- getStyleForIndividuals(pointStyleList, input = "size")
  
  # default legend name for empty input
  if (!is.null(legend)) {
    legendName <- extractTitle(legend$layout$title[[1]], default = "individual")
    legendLabels <- sapply(names(legend$layout$labels), function(name) {
      extractTitle(legend$layout$labels[[name]], default = name)
    })
    
    plot <- plot %>% setLegendThemeOfGGplot(legend = legend)
  } else {
    legendName <- "individual"
    legendLabels <- names(lineColors)
    names(legendLabels) <- names(lineColors)
  }
  
  plot <- plot + 
    scale_colour_manual(name = legendName, labels = legendLabels, values = lineColors) +  # former colorL
    scale_fill_manual(name = legendName, labels = legendLabels, values = fillColors) +  # former colorU
    scale_shape_manual(name = legendName, labels = legendLabels, values = pointShapes) +
    scale_size_manual(name = legendName, labels = legendLabels, values = pointSize)
  
  plot
}

setDefaultTitles <- function(plot, prop, xAxisLabel = "Time", yAxisLabel = "Estimate") {
  stopifnot(prop < 1)
  
  plot +
    labs(title = paste0(prop * 100, "%-Credibility-Interval for isotopic values over time"),
         x = xAxisLabel, y = yAxisLabel)
}

drawShiftLines <- function(plot, object, deriv, plotShifts, ...) {
  # breaks within data range
  breaks <- getBreaks(time = object@time, deriv = deriv)
  
  if (plotShifts){
    index <- getShiftIndex(object, ...)
    plot <- plot + geom_vline(xintercept = breaks[which(index)] + 0.5, col = "darkgrey")
  }
  
  plot
}

removeEmptyModels <- function(plotDataList) {
  # keep names of all models
  allModelNames <- names(plotDataList)
  
  # remove elements with no rows
  plotDataList <- plotDataList[sapply(plotDataList, nrow) > 0]
  
  # empty of selected models
  emptyModels <- setdiff(allModelNames, names(plotDataList))
  
  # warning that no data is available for some selected models
  if (length(emptyModels) > 0) {
    warning(paste("No data available for model(s):", 
                  paste(emptyModels, collapse = ", "),
                  ". Model(s) not displayed in table 'Plot Data' or in 'Time Plot'."))
  }
  
  plotDataList
}

extractPlotDataDF <- function(plotDataList, models, credInt) {
  models <- intersect(models, names(plotDataList))
  
  if (length(models) == 0) return(data.frame())

  # filter for displayed models:
  res <- plotDataList[models] %>%
    bind_rows(.id = "individual") %>%
    # add column with 
    mutate(cred_interval = sprintf("%.0f%%", credInt * 100)) %>%
    group_by(.data$time) %>%
    # add id for each x value:
    mutate(id_time = cur_group_id()) %>% 
    ungroup() %>%
    group_by(.data$individual) %>%
    # add id for each individual:
    mutate(id_model = cur_group_id()) %>%
    # add an empty line after each individual (containing only NA values):
    do(add_na_row(.)) %>% 
    ungroup() %>%
    # select order of columns for display and export data
    select("id_model", "individual", "id_time", "time_lower", "time_upper", "time", 
           "cred_interval", "lower", "median", "upper", "sd") %>%
    # remove last line containing only NA values
    slice(1:(n() - 1))
  
  # drop levels of the individual column (important for legend content)
  res$individual <- res$individual %>%
    as.factor() %>%
    droplevels()
  
  res
}

#' Get Plot Data
#' 
#' Extracts data from model output object
#' 
#' @param time (numeric) time vector
#' @inheritParams plotTime
getPlotData <- function(object, prop = 0.8, time = NULL, deriv = "1"){
  dat <- rstan::extract(object)$interval

  if (is.null(dat)) return(data.frame())
  
  # apply user derivation
  if(deriv == "2"){
    if (ncol(dat) > 2) {
      dat <- t(apply(dat, 1, diff))
    } else {
      dat <- apply(dat, 1, diff) %>% as.matrix(ncol = 1)
    }
  }
  
  # extract quantiles
  lLim <- (1 - prop) / 2
  uLim <- 1 - lLim
  intervalQuantiles <- as.data.frame(
    t(apply(dat, 2, quantile, probs = c(lLim, 0.5, uLim)))
  )
  names(intervalQuantiles) <- c("lower", "median", "upper")
  
  # extract sd
  intervalSD <- data.frame(
    sd = apply(dat, 2, sd)
  )
  
  # combine data
  out <- bind_cols(intervalQuantiles, intervalSD)
  
  # add time column
  if (is.null(time)) time <- 1:ncol(dat)
  out$time <- time
  
  return(out)
}

#' Extract Plot Data
#' 
#' @param plotData (data.frame) plot data
#' @inheritParams plotTime
updateTime <- function(plotData, object, deriv = "1") {
  if (nrow(plotData) == 0) return(plotData)
  
  plotData$time <- adjustTimeColumn(objectTime = object@time, deriv = deriv)
  plotData$time_lower <- adjustTimeColumn(objectTime = object@timeLower, deriv = deriv)
  plotData$time_upper <- adjustTimeColumn(objectTime = object@timeUpper, deriv = deriv)
  
  plotData
}

#' Adjust Time Column
#' 
#' @param objectTime (numeric) vector, time element of object
#' @inheritParams plotTime
adjustTimeColumn <- function(objectTime, deriv){
  if(deriv == "1"){
    res <- as.numeric(objectTime)
  } else {
    res <- joinTimeForDerivation(as.numeric(objectTime))
  }
  
  res
}

#' Get Breaks for Derivation
#' 
#' Get the time for the x axis of the plot in case of showing the derivation
#' 
#' @param time (numeric) time points of x axis
joinTimeForDerivation <- function(time){
  as.numeric(time)[1:(length(time) - 1)] + 
    diff(as.numeric(time)) / 2
}

#' Get X-Axis Data
#' 
#' @inheritParams plotTime
#' @param oldXAxisData data.frame, output from previous run of this function
#' 
#' @export
getXAxisData <- function(object, oldXAxisData = data.frame()){
  if (is.null(object)) return(data.frame())
  
  xAxisData <- data.frame(time = object@time,
                          time_lower = object@timeLower,
                          time_upper = object@timeUpper)
  
  if (nrow(oldXAxisData) > 0) {
    xAxisData <- bind_rows(xAxisData, oldXAxisData) %>%
      distinct() %>%
      arrange(.data$time)
  }
  
  xAxisData
}

setPlotLimits <- function(plot, newData = NULL, xLim = NULL, yLim = NULL) {
  allData <- plot$data
  if(!is.null(newData)) allData <- bind_rows(plot$data, newData) %>% distinct()
  
  if (length(xLim) == 0) xLim <- getXRange(allData)
  if (length(yLim) == 0) yLim <- getYRange(allData)
  
  plot + coord_cartesian(ylim = yLim, xlim = xLim)
}

getUserLimits <- function(plotRanges) {
  if (is.null(plotRanges[["fromData"]]) || plotRanges[["fromData"]]) return(NULL)
  
  c(plotRanges[["min"]], plotRanges[["max"]])
}

getXRange <- function(dat) {
  if (nrow(dat) == 0) return(c(defaultInputsForUI()$xmin, defaultInputsForUI()$xmax))
  
  range(dat$time, na.rm = TRUE)
}

getYRange <- function(dat) {
  if (nrow(dat) == 0) return(c(defaultInputsForUI()$ymin, defaultInputsForUI()$ymax))
  
  ymin <- min(dat$lower, na.rm = TRUE)
  ymax <- max(dat$upper, na.rm = TRUE)
  rangeY <- ymax - ymin
  
  c(ymin - 0.1*rangeY, ymax + 0.1*rangeY)
}
