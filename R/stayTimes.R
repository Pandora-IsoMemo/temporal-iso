#' Calculate estimated stay times for locations given isotopic values
#' 
#' @description The function calculates points in time where large changes happen in the isotopic values. It uses the posterior
#' distributions to estimate the probability of changes to be (absolutely or relatively) large.
#' 
#' @param object model of class \code{\link{TemporalIso}}
#' @param siteMeans numeric vector. Isotopic mean values of sites/locations.
#' @param siteSigma numeric vector. Isotopic standard deviation values of sites/locations..
#' @param intervalLengths numceric vector. Vector of time interval lengths. Optional (if not given equal length intervals are assumed)
#' @param print boolean. If output should be printed. Defaults to TRUE.
#'
#' @return a data.frame containing the interval starts (intStart) and ends (intEnd) of changes.
#' 
#' @export
getSiteStayTimes <- function(object, siteMeans, siteSigma, intervalLengths = NULL, print = TRUE){
  if (length(siteMeans) != length(siteSigma)){
    stop("Means and Standard Deviations have to be of equal length")
  }
  if (length(siteMeans) < 2){
    stop("Please provide at least two location means and standard deviations")
  }
  
  intervalEstimates <- rstan::extract(object)$interval
  if (!is.null(intervalLengths)){
    if (NCOL(intervalEstimates) != length(intervalLengths)){
      stop("intervalLengths vector must match with number of intervals given in model object")
    }
  }
  
  if (is.null(intervalLengths)){
    intervalLengths <- rep(1, NCOL(intervalEstimates))
  }
  dens <- lapply(1:length(siteMeans),
                 function(x) dnorm(intervalEstimates,
                           mean = siteMeans[x],
                           sd = siteSigma[x]))
  
  dens <- lapply(1:length(dens), function(x) dens[[x]] / Reduce("+", dens))
  
  props <- do.call(cbind, lapply(1:length(dens), function(x) colMeans(dens[[x]])))
  # props_0.05 <- do.call(cbind, lapply(1:length(dens),
  #function(x) apply(dens[[x]], 2, quantile, 0.05)))
  # props_0.95 <- do.call(cbind, lapply(1:length(dens),
  #function(x) apply(dens[[x]], 2, quantile, 0.95)))

  props <- round(props, 3)
  rownames(props) <- paste("Interval", 1:NROW(props))
  colnames(props) <- paste("Site", 1:NCOL(props))
  
  stayTimes <- intervalLengths %*% props
  colnames(stayTimes) <- paste("Site", 1:length(siteMeans))
  rownames(stayTimes) <- paste("Stay_time")
  
  if (print == TRUE){
    print("Estimated stay time lengths")
    print(stayTimes)
    cat("\n\n")
    print("Stay time proportion by interval")
    print(props)
    cat("\n\n")
    cat("\n\n")
    cat("\n\n")
  }
  return(list(stayTimes = stayTimes, proportions = props))
}
