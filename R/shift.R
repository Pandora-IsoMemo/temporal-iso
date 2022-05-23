getShiftIndex <- function(object,
                          type = T,
                          slope = F,
                          threshold = NULL,
                          probability = 0.5){

  if (is.null(threshold)){
    if (type == T){
      threshold <- 1.5
    }
    else{
      threshold <- 0.15
    }
    
  }
  
  samples <- as.data.frame(rstan::extract(object)$interval)
  
  if(any(object@timeLower != object@timeUpper)){
    labels <- c(paste0("[", as.character(object@timeLower),"-", as.character(object@timeUpper), "]"))
  } else {
    labels <- as.character(object@time)
  }
  
  names(samples) <- labels
  
  diffNumbers <- combn(1:length(names(samples)), 2)
  diffNames <- apply(combn(names(samples), 2), 2, paste, collapse = "_")
  if(slope){
    diff <- sapply(1:ncol(diffNumbers),
                   function(x) abs((samples[,diffNumbers[1,x]] - samples[,diffNumbers[2,x]]) / (diffNumbers[2,x] - diffNumbers[1,x])))
    diffMeans <- sapply(1:ncol(diffNumbers),
                        function(x) abs(0.5 * (samples[,diffNumbers[1,x]] + samples[,diffNumbers[2,x]]) / (diffNumbers[2,x] - diffNumbers[1,x])))
  } else {
    diff <- sapply(1:ncol(diffNumbers),
                   function(x) samples[,diffNumbers[1,x]] - samples[,diffNumbers[2,x]])
    diffMeans <- sapply(1:ncol(diffNumbers),
                        function(x) 0.5 * (samples[,diffNumbers[1,x]] + samples[,diffNumbers[2,x]]))
  }
    
  if (type == F){
    diff <- abs(diff / diffMeans)
  }
  q1 <- apply(diff, 2, quantile, probs = probability)
  q2 <- apply(diff, 2, quantile, probs = 1 - probability)
  out <- apply(rbind(q1, q2), 2, function(x) min(abs(x)))
  outsign <- sign(apply(rbind(q1, q2), 2, prod))
  
  out <- (out > threshold & outsign == 1)
  return(out)
}


#' Calculate time points of shifted isotopic values
#' 
#' @description The function calculates points in time where large changes happen in the isotopic values. It uses the posterior
#' distributions to estimate the probability of changes to be (absolutely or relatively) large.
#' 
#' @param object model of class \code{\link{TemporalIso}}
#' @param type  boolean If the calculation shall be based on absolute or relative differences. Defaults to TRUE
#' @param slope  boolean If the calculation shall be based on differences or slopes Defaults to FALSE (differences)
#' @param threshold numeric. The threshold for a shift to be considered "large". Defaults to 1.5 for absolute isotopic values and
#' 15 percent for relative changes.
#' @param probability the probability for the differences to be larger than the threshold. Defaults to 50 percent.
#' 
#' @return a data.frame containing the interval starts (intStart) and ends (intEnd) of changes.
#' 
#' @export
getShiftTime <- function(object,
                         type = TRUE,
                         slope = FALSE,
                         threshold = NULL,
                         probability = 0.5){
  
  if(is.null(object)){
    return(data.frame(noShifts = c()))
  }
  
  index <- getShiftIndex(object = object,
                         type = type,
                         slope = slope,
                         threshold = threshold,
                         probability = probability)

  if(sum(index) == 0){
    return(data.frame(noShifts = c()))
  }
  
  if(any(object@timeLower != object@timeUpper)){
    labels <- c(paste0("[", as.character(object@timeLower),"-", as.character(object@timeUpper), "]"))
    # time <- 0.5 * (object@timeLower + object@timeUpper)
    # labels <- as.character(time)
  } else {
    labels <- as.character(object@time)
  }
  
  labelNames <- combn(labels, 2)[, index, drop = FALSE]
  out <- do.call("rbind", lapply(1:ncol(labelNames), function(x) data.frame(
    interval_1 = labelNames[1, x],
    interval_2 = labelNames[2, x]
  )))
  return(out)
}

#' Calculate estimates for user defined intervals
#' 
#' @param object model of class \code{\link{TemporalIso}}
#' @param minim  numeric lower interval point
#' @param maxim  numeric upper interval point
#' @param type character "1" for absolute values, "2" for total turnover
#' 
#' @return a data.frame containing mean and sd within the interval
#' 
#' @export
getUserDefinedEst <- function(object,
                         minim = 0,
                         maxim = 1,
                         type = "1"){
  if(is.null(object)){
    return(data.frame(Mean = NA, SD = NA))
  }
  
  dat <- rstan::extract(object)$interval
  times <- which(object@time >= minim & object@time <= maxim)
  
  if(length(times) == 0){
    return(data.frame(Mean = NA, SD = NA))
  } else {
    if(type == "1"){
      dat <- unlist(dat[, times])
    } else {
      dat <- rowSums(abs(apply(dat[, times], 2, diff)))
    }
    return(data.frame(Mean = mean(dat), SD = sd(dat)))
  }
}
