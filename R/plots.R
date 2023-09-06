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
                     ...){
  stopifnot(prop < 1)

  x <- getPlotData(object, prop = prop, deriv = deriv)
  x$time <- adjustTimeColumn(objectTime = object@time, deriv = deriv)
  
  if (nrow(x) > 1) lineFct <- geom_line else lineFct <- geom_point
  if(is.null(oldPlot)){
    p <- ggplot(x, aes(x = .data[["time"]])) + 
      lineFct(aes(y = .data[["median"]]), colour = colorL, alpha = alphaL) +
      lineFct(aes(y = .data[["lower"]]), size = 0.05, colour = colorL, alpha = alphaL) +
      lineFct(aes(y = .data[["upper"]]), size = 0.05, colour = colorL, alpha = alphaL) +
      geom_point(aes(x = .data[["time"]], y = .data[["median"]]), colour = colorL, alpha = alphaL) +
      coord_cartesian(ylim = yLim, xlim = xLim) +
      labs(title = paste0(prop * 100, "%-Credibility-Interval for isotopic values over time"),
           x = xAxisLabel, y = yAxisLabel) + theme(panel.grid.major.x = element_line(size = 0.1)) + 
      theme(axis.title.x = element_text(size = sizeTextX),
            axis.title.y = element_text(size = sizeTextY),
            axis.text.x = element_text(size = sizeAxisX),
            axis.text.y = element_text(size = sizeAxisY))
    if (nrow(x) > 1) p <- p + geom_ribbon(aes(ymin = .data[["lower"]], ymax = .data[["upper"]]), 
                                          linetype = 2, alpha = alphaU, fill = colorU) 
  } else {
    if(secAxis){
      oldCoord <- oldPlot$coordinates$limits$y
      b <- seq(min(yLim),max(yLim), length.out = 100)
      a <- seq(min(oldCoord),max(oldCoord), length.out = 100)
      res <- lm(b~a)
      
      scale <- res$coefficients[2]
      center <- res$coefficients[1]
      x$median <- (x$median  - center ) / scale
      x$lower <- (x$lower - center ) / scale
      x$upper <- (x$upper  - center ) / scale
    }
    p <- oldPlot  + geom_line(data = x, aes(y = .data[["median"]]), colour = colorL, alpha = alphaL) +
      geom_line(data = x, aes(y = .data[["lower"]]), size = 0.05, colour = colorL, alpha = alphaL) +
      geom_line(data = x, aes(y = .data[["upper"]]), size = 0.05, colour = colorL, alpha = alphaL) +
      geom_ribbon(data = x, aes(ymin = .data[["lower"]], ymax = .data[["upper"]]), linetype = 2, alpha = alphaU, fill = colorU) +
      geom_point(data = x, aes(x = .data[["time"]], y = .data[["median"]]), colour = colorL, alpha = alphaL) +
      labs(title = paste0(prop * 100, "%-Credibility-Interval for isotopic values over time"),
           x = "Time", y = "Estimation")
    
    if(secAxis){
      p <- p + 
      theme(axis.title.y = element_text(size = sizeTextY),
            axis.text.y = element_text(size = sizeAxisY)) +   scale_y_continuous(
             # Features of the first axis
             # Add a second axis and specify its features
             sec.axis = sec_axis(~(.* scale) + center, name=yAxisLabel)
           )
    }
  }
  
  xAxisData <- getXAxisData(object = object, oldXAxisData = oldXAxisData)
  xAxisData <- xAxisData %>%
    extendXAxis(deriv = deriv, 
                xLim = ifelse(extendLabels, xLim, range(xAxisData)))
  
  breaks <- getBreaks(time = xAxisData$time, deriv = deriv)
  labels <- getLabel(xAxisData = xAxisData, deriv = deriv)
  
  p <- p + 
    scale_x_continuous(breaks = breaks, labels = labels)

  if (plotShifts){
    index <- getShiftIndex(object, ...)
    p <- p + geom_vline(xintercept = breaks[which(index)] + 0.5, col = "darkgrey")
  }
  
  p
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
#' @inheritParams plotTime
extendXAxis <- function(xAxisData, deriv, xLim) {
  if (min(xLim) < min(xAxisData)) {
    # add new row at the beginning
    newFirstRow <- data.frame(
      "time" = mean(c(min(xLim), min(xAxisData))),
      "lower" = min(xLim),
      "upper" = min(xAxisData)
    )
    
    xAxisData <- rbind(newFirstRow, 
                          xAxisData)
  }
  
  if (max(xLim) > max(xAxisData)) {
    # add new row at the end
    newLastRow <- data.frame(
      "time" = mean(c(max(xAxisData), max(xLim))),
      "lower" = max(xAxisData),
      "upper" = max(xLim)
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

#' Get Default Plot Range
#' 
#' Min/max values in x/y directions
#' 
#' @param savedModels list of models of class \code{\link{TemporalIso}}
#' @inheritParams plotTime
#' 
#' @export
getDefaultPlotRange <- function(savedModels, deriv = "1"){

  dat <- lapply(savedModels, function(model){
    if (is.null(model$fit)) return(NULL)
    fit <- model$fit
    x <- getPlotData(fit, prop = 0.8, deriv = deriv)
    x$time <- adjustTimeColumn(objectTime = fit@time, deriv = deriv)
    x
    })
  
  dat <- dat %>% bind_rows()
  
  if (nrow(dat) == 0) return(list(xmin = defaultInputsForUI()$xmin,
                                  xmax = defaultInputsForUI()$xmax,
                                  ymin = defaultInputsForUI()$ymin,
                                  ymax = defaultInputsForUI()$ymax))
  
  xmin <- min(dat$time, na.rm = TRUE)
  xmax <- max(dat$time, na.rm = TRUE)
  
  ymin <- min(dat$lower, na.rm = TRUE)
  ymax <- max(dat$upper, na.rm = TRUE)
  rangeY <- ymax - ymin
  
  list(xmin = xmin,
       xmax = xmax,
       ymin = ymin - 0.1*rangeY,
       ymax = ymax + 0.1*rangeY)
}
