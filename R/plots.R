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
                     ...){
  stopifnot(prop < 1)

  x <- getPlotData(object, prop = prop, deriv = deriv) %>%
    updateTime(object = object, deriv = deriv)
  
  x$individual <- "current"
  pointStyleList <- list()
  pointStyleList[["current"]] <- config()[["defaultPointStyle"]][["dataPoints"]]
  pointStyleList[["current"]]["color"] <- colorL
  
  p <- basePlotTime(x = x,
                      xLim = xLim, yLim = yLim,
                      sizeTextX = sizeTextX, sizeTextY = sizeTextY,
                      sizeAxisX = sizeAxisX, sizeAxisY = sizeAxisY) %>%
      setTitles(prop, xAxisLabel, yAxisLabel)
  
  p <- p %>%
    drawLinesAndRibbon(pointStyleList = pointStyleList, alphaL = alphaL, alphaU = alphaU) %>%
    setXAxisLabels(xAxisData = getXAxisData(object = object),
                   extendLabels = extendLabels, 
                   xLim = xLim, 
                   deriv = deriv,
                   plotShifts = plotShifts,
                   ...)
  
  p + theme(legend.position = "none")
}

basePlotTime <- function(x,
                         xLim = NULL, yLim = NULL, 
                         sizeTextX = 12, sizeTextY = 12, 
                         sizeAxisX = 12, sizeAxisY = 12) {
  p <- ggplot(x, aes(x = .data[["time"]])) + 
    theme(panel.grid.major.x = element_line(linewidth = 0.1)) + 
    theme(axis.title.x = element_text(size = sizeTextX),
          axis.text.x = element_text(size = sizeAxisX),
          axis.title.y = element_text(size = sizeTextY),
          axis.text.y = element_text(size = sizeAxisY))
  
  p <- p %>%
    setPlotLimits(xLim = xLim, yLim = yLim)
  
  p
}

setPlotLimits <- function(plot, newData = NULL, xLim = NULL, yLim = NULL) {
  allData <- plot$data
  if(!is.null(newData)) allData <- bind_rows(plot$data, newData) %>% distinct()
  
  if (length(xLim) == 0) xLim <- getXRange(allData) %>% unlist()
  if (length(yLim) == 0) yLim <- getYRange(allData) %>% unlist()
  
  plot + coord_cartesian(ylim = yLim, xlim = xLim)
}

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
    theme(axis.title.y.right = element_text(family = "Arial",
                                            size = titleFormat[["size"]],
                                            face = titleFormat[["fontType"]],
                                            color = yAxisTitleColor,
                                            hjust = 0.5),
          axis.text.y.right = element_text(family = "Arial",
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

rescaleLayerData <- function(x, rescaling) {
  if (identical(rescaling, list(scale = 1, center = 0))) return(x)
  
  scale <- rescaling$scale
  center <- rescaling$center
  
  x$median <- (x$median  - center ) / scale
  x$lower <- (x$lower - center ) / scale
  x$upper <- (x$upper  - center ) / scale
  
  x
}

getRescaleParams <- function(oldLimits, newLimits = NULL, secAxis = FALSE) {
  if (length(newLimits) == 0 || !secAxis) return(list(scale = 1, center = 0))
  
  b <- seq(min(newLimits),max(newLimits), length.out = 100)
  a <- seq(min(oldLimits),max(oldLimits), length.out = 100)
  res <- lm(b~a)
  
  list(scale = res$coefficients[2],
       center = res$coefficients[1])
}

drawLinesAndRibbon <- function(plot, pointStyleList, alphaL, alphaU) {
  # draw lines "upper", "mdeian", "lower"
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
  
  # draw ribbon
  if (nrow(plot$data) > 1) {
    plot <- plot + 
      geom_ribbon(aes(ymin = .data[["lower"]], ymax = .data[["upper"]], 
                      fill = .data[["individual"]]), 
                  linetype = 2, alpha = alphaU)
  }
  
  # draw points "median"
  plot <- plot +
    geom_point(aes(y = .data[["median"]],
                   colour = .data[["individual"]],
                   shape = .data[["individual"]],
                   size = .data[["individual"]],
                   fill = .data[["individual"]]),
               alpha = alphaL)
  
  # set scales for each "individual"
  lineColors <- getStyleForIndividuals(pointStyleList, input = "color")
  fillColors <- getStyleForIndividuals(pointStyleList, input = "color")
  pointShapes <- getStyleForIndividuals(pointStyleList, input = "symbol")
  pointSize <- getStyleForIndividuals(pointStyleList, input = "size")
  
  plot + 
    scale_colour_manual(name = "individual", values = lineColors) +  # former colorL
    scale_fill_manual(name = "individual", values = fillColors)+  # former colorU
    scale_shape_manual(name = "individual", values = pointShapes) +
    scale_size_manual(name = "individual", values = pointSize)
}

getStyleForIndividuals <- function(pointStyleList, input) {
  # pointStyleList are reactive values -> lapply over the names and not(!) the list itself
  style <- lapply(names(pointStyleList), function(x) {pointStyleList[[x]][input]}) %>% 
    unlist()
  names(style) <- names(pointStyleList)
  
  style
}

setTitles <- function(plot, prop, xAxisLabel = "Time", yAxisLabel = "Estimate") {
  stopifnot(prop < 1)
  
  plot +
    labs(title = paste0(prop * 100, "%-Credibility-Interval for isotopic values over time"),
         x = xAxisLabel, y = yAxisLabel)
}

setXAxisLabels <- function(plot, xAxisData, extendLabels, xLim, deriv, plotShifts = FALSE, ...) {
  xPlotLim <- xLabelLim <- range(xAxisData)
  
  if (length(xLim) == 2) xPlotLim <- xLim
  
  if (extendLabels && length(xLim) == 2) {
    xLabelLim <- xLim
    xAxisData <- xAxisData %>%
      extendXAxis(xLabelLim = xLabelLim)
  }
  
  breaks <- getBreaks(time = xAxisData$time, deriv = deriv)
  labels <- getLabel(xAxisData = xAxisData, deriv = deriv)
  
  plot <- plot + 
    scale_x_continuous(breaks = breaks, labels = labels, limits = xPlotLim) 
  
  if (plotShifts){
    index <- getShiftIndex(object, ...)
    plot <- plot + geom_vline(xintercept = breaks[which(index)] + 0.5, col = "darkgrey")
  }
  
  plot
}

getLim <- function(plotRanges, axis = c("xAxis", "yAxis")) {
  axis <- match.arg(axis)
  
  if (plotRanges[[axis]][["fromData"]]) return(numeric(0))
  
  c(plotRanges[[axis]][["min"]], plotRanges[[axis]][["max"]])
}

extractPlotDataDF <- function(plotDataList, models, credInt) {
  # filter for displayed models:
  plotDataList[models] %>%
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
}

#' Get Plot Data
#' 
#' Extracts data from model output object
#' 
#' @param time (numeric) time vector
#' @inheritParams plotTime
getPlotData <- function(object, prop = 0.8, time = NULL, deriv = "1"){
  lLim <- (1 - prop) / 2
  uLim <- 1 - lLim
  dat <- rstan::extract(object)$interval

  if(deriv == "2"){
    if (ncol(dat) > 2) {
      dat <- t(apply(dat, 1, diff))
    } else {
      dat <- apply(dat, 1, diff) %>% as.matrix(ncol = 1)
    }
  }
  
  # extract quantiles
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

extractAllXAxisData <- function(extractedPlotDataList) {
  extractedPlotDataList %>%
    bind_rows() %>%
    select("time", "time_lower", "time_upper") %>% 
    distinct() %>%
    arrange(.data$time)
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

#' Extend X Axis
#' 
#' Add breaks and labels for x axis
#' 
#' @param xAxisData (data.frame) data.frame containing "time", "lower" and "upper" columns used for
#'  the x axis.
#' @param xLabelLim numeric vector of length 2: range of labels of x axis
extendXAxis <- function(xAxisData, xLabelLim) {
  if (min(xLabelLim) < min(xAxisData[["time_lower"]])) {
    # add new row at the beginning
    newFirstRow <- data.frame(
      "time" = mean(c(min(xLabelLim), min(xAxisData[["time_lower"]]))),
      "time_lower" = min(xLabelLim),
      "time_upper" = min(xAxisData[["time_lower"]])
    )
    
    xAxisData <- rbind(newFirstRow, 
                       xAxisData)
  }
  
  if (max(xLabelLim) > max(xAxisData[["time_upper"]])) {
    # add new row at the end
    newLastRow <- data.frame(
      "time" = mean(c(max(xAxisData[["time_upper"]]), max(xLabelLim))),
      "time_lower" = max(xAxisData[["time_upper"]]),
      "time_upper" = max(xLabelLim)
    )
    
    xAxisData <- rbind(xAxisData,
                       newLastRow)
  }
  
  xAxisData
}

#' Get Label
#' 
#' @param xAxisData data.frame of time data of object, output of getXAxisData
#' @inheritParams plotTime
#' @param hidePastedLabels (logical) if TRUE then dont't show pasted labels 
getLabel <- function(xAxisData, deriv, hidePastedLabels = TRUE){
  if(any(xAxisData$time_lower != xAxisData$time_upper)){
    labels <- c(paste0("[", as.character(xAxisData$time_lower),"-", as.character(xAxisData$time_upper), "]"))
  } else {
    labels <- unique(sort(as.numeric(c(xAxisData$time_lower, xAxisData$time, xAxisData$time_upper))))
  }
  
  if(deriv == "2"){
    labels <- pasteLabelForDerivation(labels, hidePastedLabels = hidePastedLabels)
  }
  
  labels
}


#' Paste Label for Derivation Plot
#' 
#' Get the labels for the x axis of the plot in case of showing the derivation
#' 
#' @param axesLabel (character) output of \code{\link{getLabel}}
#' @inheritParams getLabel
pasteLabelForDerivation <- function(axesLabel, hidePastedLabels){
  n <- length(axesLabel)
  pastedLabels <- paste0(axesLabel[1:(n - 1)], ",", axesLabel[2:n])
  pastedLabels <- lapply(pastedLabels, function(x) gsub("\\],\\[", ",", x)) %>% unlist(use.names = FALSE)
  
  if (hidePastedLabels) {
    pastedLabels <- rep("", length(pastedLabels))
  }
  
  # order both vectors with alternating indices
  c(axesLabel, pastedLabels)[order(c(seq_along(axesLabel), seq_along(pastedLabels)))]
}


#' Get Breaks
#' 
#' @param time (numeric) time points of x axis
#' @inheritParams plotTime
getBreaks <- function(time, deriv){
  breaks <- time
  
  if(deriv == "2"){
    breaks <- c(breaks, joinTimeForDerivation(breaks)) %>% sort()
  }
  
  breaks
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


getXRange <- function(dat) {
  if (nrow(dat) == 0) return(list(xmin = defaultInputsForUI()$xmin,
                                  xmax = defaultInputsForUI()$xmax))
  
  xmin <- min(dat$time, na.rm = TRUE)
  xmax <- max(dat$time, na.rm = TRUE)
  
  list(xmin = xmin,
       xmax = xmax)
}

getYRange <- function(dat) {
  if (nrow(dat) == 0) return(list(ymin = defaultInputsForUI()$ymin,
                                  ymax = defaultInputsForUI()$ymax))
  
  ymin <- min(dat$lower, na.rm = TRUE)
  ymax <- max(dat$upper, na.rm = TRUE)
  rangeY <- ymax - ymin
  
  list(ymin = ymin - 0.1*rangeY,
       ymax = ymax + 0.1*rangeY)
}
