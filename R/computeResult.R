#' Function to compute resulting isotopic values out of historic ones
#' 
#' @description Given renewal rates for different isotopic probes over time and normal distributions
#' of isotopic values over time, resulting normal distributions of the isotopic probes are calculated.
#' 
#' @param data A dataframe specifying the renewal rates of different probes for
#' each time interval. The renewal rates should be between 0 and 100 (percentages). The dataframe should include a column specifying a time-index (e.g. 1, 2, 3, ...)
#' as well as columns for the different bones. Furthermore, the dataframe should contain the mean and sd values for each time interval.
#' @param timeVars A character string specifying the name of the column indicating the time.
#' @param boneVars A vector of character strings indicating the relevant variables containing the
#' renewal rates of bones and teeth.
#' @param meanVar A character string specifying the name of the column indicating the mean values of the isotopic probes over time.
#' @param sdVar A character string specifying the name of the column indicating the standard deviation of the isotopic probes over time.
#' @param cor The temporal correlation between neighbouring time points. Defaults to 0.5
#'
#' @return A data.frame containing the resulting mean and standard deviation for each bone/tooth as well
#' as the covariances.
#' 
#' @examples
#' testDat <- data.frame(
#'   t = 1:3,
#'   bone = c(100, 50, 0),
#'   mean = c(1, 3, 50),
#'   sd = c(1, 3, 50)
#' )
#'   computeResult(
#'   data = testDat,
#'   timeVars = "t",
#'   boneVars = "bone",
#'   meanVar = "mean",
#'   sdVar = "sd"
#' )
#' 
#' @seealso \link{estimateIntervals}
#'
#' @export
#' 
#' 
computeResult <- function(data,
                          timeVars,
                          boneVars = NULL,
                          meanVar,
                          sdVar,
                          cor = 0.5)
{
  stopifnot(
    all(data[sdVar] > 0),
    max(data[boneVars]) <= 100,
    length(meanVar) == 1,
    length(sdVar) == 1,
    cor >= -1,
    cor <= 1
  )
  
  if (is.null(boneVars))
  {
    boneVars <- names(data)[!names(data) %in% c(timeVars, meanVar, sdVar)]
  }
  if (length(timeVars) > 1) timeVars <- timeVars[1]
  influence <- apply(data[boneVars], 2, calcInfluence)
  corrMatrix <- diag(rep(1 - cor, length(data[[sdVar]]))) + cor
  covMatrix <- data[[sdVar]] %*% t(data[[sdVar]]) * corrMatrix
  y <- data.frame(
    mean = apply(data[[meanVar]] * influence, 2, sum),
    sd = sqrt(t(influence) %*% covMatrix %*% influence)[, 1]
  )
  return(y)
}
