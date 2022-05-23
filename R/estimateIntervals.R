#' Function to estimate temporal changes in isotopic values
#' 
#' @description Given renewal rates for different isotopic probes over time, a model
#' estimates a normal distribution of the isotopic values for each time interval. Out of
#' the given rates of renewal, first the influence of each time interval on the final
#' isotopic value is calculated. This proportion is used as a regressor in a fully Bayesian
#' linear model. The variance is estimated as rbf-Kernel-Matrix, where the hyperparameters are
#' chosen such that the correlation between time intervals close to each other is about 0.5.
#' The estimation is implemented using \code{link[rstan]{rstan}}.
#' 
#' @param renewalRates A dataframe specifying the renewal rates of different probes for
#' each time interval. The renewalRates should be between 0 and 100 (percentages). The dataframe should include a column specifying a time-index (e.g. 1, 2, 3, ...)
#' as well as columns for the different bones. The renewal rates should contain the times of origin,
#' containing 100.
#' @param timeVars A character string specifying the name of the column indicating the time.
#' @param boneVars A vector of character strings indicating the relevant variables containing the
#' renewal rates of bones and teeth. If not given, the variables with bones are all variables of the dataframe apart from the time variables.
#' @param isoMean A numeric number indicating the mean of the isotopic values measured.
#' @param isoSigma A numeric, positive number indicating the standard deviation of the isotopic values measured.
#' @param mc A boolean indicating if multiple cores should be used. If \code{TRUE}, which is the default, 4 cores are used.
#' @param adapt_delta A numeric value between 0 and 1 controlling the samplers behavior. Defaults to 0.9999. See \code{\link[rstan]{stan}} for more details.
#' @param max_treedepth A numeric, positive value controling the NUTS sampler. Defaults to 25. See \code{\link[rstan]{stan}} for more details.
#' @param mu_df Hyperparameter for the mean of the interval estimates: degrees of freedom of a non-standardized t-Student distribution. Defaults to 1.
#' @param mu_mean Hyperparameter for the mean of the interval estimates: mean of a non-standardized t-Student distribution. Defaults to 0.
#' @param mu_sd Hyperparameter for the mean of the interval estimates: standard deviation of a non-standardized t-Student distribution. Defaults to 10.
#' @param rho_mean Hyperparameter for the length scale of the rbf-kernel: mean of a normal. Defaults to 1.
#' @param rho_sd Hyperparameter for the length scale of the rbf-kernel: standard deviation of a normal. Defaults to 0.25.
#' @param alpha_mean Hyperparameter for the signal variance of the rbf-kernel: mean of a normal. Defaults to 2.
#' @param alpha_sd Hyperparameter for the signal variance of the rbf-kernel: standard deviation of a normal. Defaults to 0.5.
#' @param chains Number of chains for mcmc. Defaults to 4
#' @param burnin Number of burnin iterations per chain for mcmc. Defaults to 500
#' @param iter Number of iterations per chain for mcmc. Defaults to 2000
#' @param ... Arguments passed to rstand \link[rstan]{sampling}
#'
#' @return A fitted object of class \link{TemporalIso}.
#' 
#' @examples
#' 
#' \dontrun{
#' #dataset with renewal rates
#' data <- data.frame(
#'  intStart = 0:5,
#'  intEnd = 1:6,
#'  bone1 = c(100, 50, 20, 10, 5, 2),
#'  bone2 = c(100, 10, 5, 1, 1, 1),
#'  tooth1 = c(0, 100, 0, 0, 0, 0),
#'  tooth2 = c(0, 0, 100, 0, 0, 0)
#' )
#' # isotope-values and sd
#' y_mean <- c(-10, -7, -12, -9)
#' y_sigma <- c(2, 1.5, 2.5, 2.5)
#'
#'
###############################
#estimate values
###############################
#' fit <- estimateIntervals(renewalRates = data,
#'                         timeVars = "intStart",
#'                         boneVars = c("bone1", "bone2", "tooth1", "tooth2"),
#'                         isoMean = y_mean,
#'                       isoSigma = y_sigma)
#' print(fit)
#' # implemented Methods
#' plotTime(fit)
#' #get estimates for specific time points
#' estimateTimePoint(fit, time = seq(0,5, by = 0.5))
#'
#' ###############################
#' #shift point estimation
#' ###############################
#' plotTime(fit, plotShifts = TRUE, threshold = 0.5)
#' plotTime(fit, plotShifts = TRUE, threshold = 0.5,
#'         absolute = TRUE, probability = 0.5)
#'
#' getShiftTime(fit, threshold = 0.5)
#'
#'
#' ###############################
#' #Staying time estimation
#' ###############################
#'
#' estimatedStayTimes <- getSiteStayTimes(object = fit,
#'                                       siteMeans = c(-8, -10),
#'                                       siteSigma = c(1, 1.5))
#'
#' 
#' ###############################
#' #compute resulting isotopic values out of historic ones
#' ###############################
#'
#' testDat <- data.frame(
#'  t = 1:3,
#'  bone1 = c(100, 50, 10, 10, 10, 10),
#'  bone2 = c(0, 20, 20, 20, 20, 20),
#'  teeth1 = c(0, 0, 0, 10, 50, 60),
#'  mean = c(1, 3, 3,0, -1, - 5),
#'  sd = c(1, 3, 1, 1.5, 2, 3)
#' )
#'
#' computeResult(
#'  data = testDat,
#'  timeVars = "t",
#'  boneVars = c("bone1","bone2","teeth1"),
#'  meanVar = "mean",
#'  sdVar = "sd"
#' )
#'
#' shiny::runApp(paste0(system.file(package = "OsteoBioR"),"/app"))
#' }
#' @seealso \link[rstan]{sampling}
#'
#' @export
#' 
#' 
estimateIntervals <- function(renewalRates,
                              timeVars,
                              boneVars = NULL,
                              isoMean,
                              isoSigma,
                              mu_df = 3,
                              mu_mean = 0,
                              mu_sd = 20,
                              rho_mean = 1,
                              rho_sd = 0.25,
                              alpha_mean = 2,
                              alpha_sd = 0.5,
                              mc = TRUE,
                              adapt_delta = 0.98,
                              max_treedepth = 15,
                              chains = 4,
                              iter = 2000,
                              burnin = 500,
                              ...) {
  if(length(boneVars) != length(isoMean) | length(boneVars) != length(isoSigma)){
    stop("Number of bone variables must be equal to number
         of isotopic mean and standard deviation values!")
  }
    
  stopifnot(
    isoSigma > 0,
    max(renewalRates[boneVars], na.rm = TRUE) <= 100
  )
  if (is.null(boneVars))
  {
    boneVars <- names(renewalRates)[!names(renewalRates) %in% timeVars]
  }
  if (length(timeVars) > 1){
    timeVarsNew <- timeVars[1]
    time <- 0.5 * (renewalRates[[timeVars[1]]] + renewalRates[[timeVars[2]]])
    timeLower <- renewalRates[[timeVars[1]]]
    timeUpper <- renewalRates[[timeVars[2]]]
  } else {
    timeVarsNew <- timeVars
    time <- renewalRates[[timeVarsNew]]
    timeLower <- timeUpper <- time
  }
  N <- ncol(renewalRates[boneVars])
  NT <- nrow(renewalRates[timeVarsNew])
  
  
  y_mean <- isoMean
  y_sigma <- isoSigma
  
  # for gaps
  renewalRates[is.na(renewalRates)] <- 0
  y_mean[is.na(y_mean)] <- 0
  y_sigma[is.na(y_sigma)] <- 0
  
  t <- time / min(abs(diff(time))) # normalization: nearest neighbour is 1 away
  x <- t(as.matrix(apply(renewalRates[boneVars], 2, calcInfluence)))
  
  #cores <- getOption("mc.cores", if (mc) min(4, chains) else 1)
  model <- suppressWarnings(sampling(stanmodels$linRegGP,
                     data = list(N = N,
                                 NT = NT,
                                 y_mean = y_mean,
                                 y_sigma = y_sigma,
                                 t = t,
                                 x = x),
                                     chains = chains,
                                     iter = iter,
                                     warmup = burnin,
                                     cores = 1,
                                     # verbose = FALSE,
                                     # refresh = 0,
                                     control = list(adapt_delta = adapt_delta,
                                                    max_treedepth = max_treedepth),
                                     ...))
  
  return(new("TemporalIso",
             model,
             regressor = x,
             time = time,
             timeLower = timeLower,
             timeUpper = timeUpper))
}

#' Function to estimate isotopic value for specific time point(s)
#' 
#' @description Once the isotopic values in time course are estimated along
#' with estimateIntervals(), this function provides an interface to extract the mean, standard deviation and quantiles for a specific point in time
#' @param object An object of class \link{TemporalIso} fitted with estimateIntervals()
#' @param time A vector of numerics, indicating the point in time to extract estimates
#' @param intervalProb A numeric between 0 and 1 indicating the coverage of the credible interval
#' 
#' @return A data.frame with time points and their means, standard deviations and credible intervals
#' 
#' @export
estimateTimePoint <- function(object, time, intervalProb = 0.95){
  if (is.null(object)) return(data.frame(time = NA, mean = NA, sd = NA, intLower = NA, intUpper = NA))
  
  if(any(time < min(object@timeLower) | time > max(object@timeUpper))){
    stop("Provided time must be inside fitted intervals")
  }
  if (intervalProb <= 0 | intervalProb >= 1){
    stop("Value of intervalProb must be between 0 and 1")
  }
  estimates <- rstan::extract(object)$interval
  mean <- approx(object@time, colMeans(estimates), xout = time, rule = 2)$y
  sd <- approx(object@time, apply(estimates, 2, sd), xout = time, rule = 2)$y
  intLower <- approx(object@time, apply(estimates, 2, quantile, (1 - intervalProb) / 2), xout = time, rule = 2)$y
  intUpper <- approx(object@time, apply(estimates, 2, quantile, 1 - ((1 - intervalProb) / 2)), xout = time, rule = 2)$y
  return(round(data.frame(time = time, mean = mean, sd = sd, intLower = intLower, intUpper = intUpper), 4))
}

calcInfluence <- function(x){
  if (max(x) > 1) x <- x / 100
  out <- rep(0, length(x))
  temp <- 1
  for (i in length(x):1){
    out[i] <- x[i] * temp
    temp <- temp - out[i]
  }
  return(out / sum(out + 1E-6))
}
