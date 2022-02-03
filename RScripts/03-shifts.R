library(OsteoBioR)


source("RScripts/01-simulData.R")


dat <- simulData(seed = 1)
dat
t <- dat$intStart
x <- t(as.matrix(mutate_all(dat[c(paste0("bone", 1:3), paste0("tooth", 1:5))], calcInfluence)))
trueIsoMean = c(-9, -10, -13, -14, -11, -11, -10, -9, -11, -10)
trueIsoSD = rep(1, 10)
y <- resultFromTrue(dat, trueIsoMean = trueIsoMean, trueIsoSD = trueIsoSD)
y_mean <- as.vector(unlist(y$mean))
y_sigma <- as.vector(unlist(y$sd))


fit <- estimateIntervals(renewalRates = dat,
                         timeVars = "intStart",
                         boneVars = c(paste0("bone", 1:3), paste0("tooth", 1:5)),
                         isoMean = y_mean,
                         isoSigma = y_sigma)



fit

plot(fit)
plotTime(fit)

samples <- as.data.frame(fit) %>% select(starts_with("inter"))
head(samples)
t(apply(head(samples), 1, diff)) / head(samples)[,-1]

getShiftPoints <- function(object, absolute = TRUE, threshold = NULL){
  
  if (is.null(threshold))
  {
    if (absolute){
      threshold = 2
    }
    else{
      threshold = 0.1
    }
  }
  
  samples <- as.data.frame(object)
  diff <- t(apply(samples, 1, diff))
  
  
}
