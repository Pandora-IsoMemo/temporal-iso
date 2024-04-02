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
#' @param oldPlot ggplot object
#' @param oldXAxisData data.frame of time data from object
#' @param deriv character "1" for absolute values, "2" for first differences
#' @param colorL color of line (RGB)
#' @param colorU color of uncertainty region (RGB)
#' @param alphaL Color line
#' @param alphaU Color uncertainty region
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
                     oldPlot = NULL, oldXAxisData = data.frame(), deriv = "1", colorL = NULL,
                     colorU = NULL, alphaL = 0.9, alphaU = 0.1,
                     sizeTextY = 12, sizeTextX = 12, sizeAxisX = 12, sizeAxisY = 12, secAxis = FALSE,
                     xAxisLabel = "Time", yAxisLabel = "Estimate", extendLabels = FALSE,
                     pointStyle = config()[["defaultPointStyle"]],
                     ...){
  stopifnot(prop < 1)

  x <- extractPlotData(object, prop = prop, deriv = deriv)
  
  if(is.null(oldPlot)){
    p <- basePlotTime(x = x,
                      xLim = xLim, yLim = yLim,
                      sizeTextX = sizeTextX, sizeTextY = sizeTextY,
                      sizeAxisX = sizeAxisX, sizeAxisY = sizeAxisY) %>%
      setTitles(prop, xAxisLabel, yAxisLabel)
  } else {
    rescaling <- getRescaleParams(oldLimits = oldPlot$coordinates$limits$y,
                                  newLimits = getYRange(x) %>% unlist(),
                                  secAxis = secAxis)
    x <- x %>%
      rescaleLayerData(rescaling = rescaling)
    
    p <- oldPlot %>%
      setSecondYAxis(rescaling = rescaling, yAxisLabel = yAxisLabel)
  }
  
  p %>%
    drawLinesAndRibbon(x = x, colorL = colorL, colorU = colorU, alphaL = alphaL, alphaU = alphaU) %>%
    formatPointsOfGGplot(data = x, aes(x = .data[["time"]], y = .data[["median"]]), pointStyle = pointStyle) %>%
    setXAxisLabels(xAxisData = getXAxisData(object = object, oldXAxisData = oldXAxisData),
                   extendLabels = extendLabels, 
                   xLim = xLim, 
                   deriv = deriv,
                   plotShifts = plotShifts,
                   ...)
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

drawLinesAndRibbon <- function(plot, x, colorL, colorU, alphaL, alphaU) {
  if (nrow(x) > 1) {
    plot <- plot +
      geom_line(data = x, aes(y = .data[["median"]]), colour = colorL, alpha = alphaL) +
      geom_line(data = x, aes(y = .data[["lower"]]), linewidth = 0.05, colour = colorL, alpha = alphaL) +
      geom_line(data = x, aes(y = .data[["upper"]]), linewidth = 0.05, colour = colorL, alpha = alphaL)
  } else {
    plot <- plot +
      geom_point(data = x, aes(y = .data[["median"]]), colour = colorL, alpha = alphaL) +
      geom_point(data = x, aes(y = .data[["lower"]]), size = 0.05, colour = colorL, alpha = alphaL) +
      geom_point(data = x, aes(y = .data[["upper"]]), size = 0.05, colour = colorL, alpha = alphaL)
  }
  
  if (nrow(x) > 1) {
    plot <- plot + 
      geom_ribbon(data = x, aes(ymin = .data[["lower"]], ymax = .data[["upper"]]), 
                  linetype = 2, alpha = alphaU, fill = colorU)
  }
  
  plot
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

#' Extract Plot Data
#' 
#' @inheritParams plotTime
extractPlotData <- function(object, prop = 0.8, deriv = "1") {
  x <- getPlotData(object, prop = prop, deriv = deriv)
  x$time <- adjustTimeColumn(objectTime = object@time, deriv = deriv)
  
  x
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
  
  out <- as.data.frame(
    t(apply(dat, 2, quantile, probs = c(lLim, 0.5, uLim)))
  )
  names(out) <- c("lower", "median", "upper")
  
  if (is.null(time)) time <- 1:ncol(dat)
  out$time <- time
  
  return(out)
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

#' Extract All X-Axis Data
#' 
#' Extract all x-axis data to draw x axis ticks and labels at all possible points in time present 
#'  in models
#' 
#' @param models (list) list of models
#' @param allXAxisData (data.frame) empty data.frame, or a data.frame containing xAxisData, output
#'  of \code{getXAxisData}
#' 
#' @export
extractAllXAxisData <- function(models, allXAxisData = data.frame()) {
  for (i in 1:length(models)) {
    allXAxisData <- getXAxisData(models[[i]]$fit, oldXAxisData = allXAxisData)
  }
  
  allXAxisData
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
                          lower = object@timeLower,
                          upper = object@timeUpper)
  
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
  if (min(xLabelLim) < min(xAxisData[["lower"]])) {
    # add new row at the beginning
    newFirstRow <- data.frame(
      "time" = mean(c(min(xLabelLim), min(xAxisData[["lower"]]))),
      "lower" = min(xLabelLim),
      "upper" = min(xAxisData[["lower"]])
    )
    
    xAxisData <- rbind(newFirstRow, 
                       xAxisData)
  }
  
  if (max(xLabelLim) > max(xAxisData[["upper"]])) {
    # add new row at the end
    newLastRow <- data.frame(
      "time" = mean(c(max(xAxisData[["upper"]]), max(xLabelLim))),
      "lower" = max(xAxisData[["upper"]]),
      "upper" = max(xLabelLim)
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
  if(any(xAxisData$lower != xAxisData$upper)){
    labels <- c(paste0("[", as.character(xAxisData$lower),"-", as.character(xAxisData$upper), "]"))
  } else {
    labels <- unique(sort(as.numeric(c(xAxisData$lower, xAxisData$time, xAxisData$upper))))
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
