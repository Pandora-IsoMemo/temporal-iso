setXAxisLabels <- function(plot, xAxisData, extendLabels, deriv, xLim = NULL) {
  # set limits
  if (length(xLim) == 2) {
    xPlotLim <- xLim
  } else {
    xPlotLim <- xLabelLim <- range(xAxisData)
  }
  
  # extend labels to full x axis range
  if (extendLabels && length(xLim) == 2) {
    xLabelLim <- xLim
    xAxisData <- xAxisData %>%
      extendXAxis(xLabelLim = xLabelLim)
  }
  
  breaks <- getBreaks(time = xAxisData$time, deriv = deriv)
  labels <- getLabel(xAxisData = xAxisData, deriv = deriv)
  
  plot <- plot + 
    scale_x_continuous(breaks = breaks, labels = labels, limits = xPlotLim) 
  
  plot
}

extractAllXAxisData <- function(extractedPlotDataList) {
  extractedPlotDataList %>%
    bind_rows() %>%
    select("time", "time_lower", "time_upper") %>% 
    distinct() %>%
    arrange(.data$time)
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
