library(gdata)
library(bayesplot)
library(ggplot2)
library(OsteoBioR)
library(testthat)

## data
source("RScripts/01-simulData.R")
dat <- as.data.frame(simpleData())
y_mean <- c(-10, -7, -12, -9)
y_sigma <- c(2, 1.5, 2.5, 2.5)
fit <- estimateIntervals(renewalRates = dat,
                         timeVars = "intStart",
                         boneVars = c("bone1", "bone2", "tooth1", "tooth2"),
                         isoMean = y_mean,
                         isoSigma = y_sigma)

fit@sim
summary(rstan::extract(fit)$interval)
summary(fit)$summary[paste0("interval[", 1:6, "]"), "75%"]


prop <- 0.8
dat <- rstan::extract(fit)$interval
mean <- apply(dat, 2, mean)
quantile <- apply(dat, 2, quantile, probs = c(0.25, 0.75))
quantile

#### timeline plots
# prepare data


# test data preparation
x <- getPlotData(fit, prop = 0.5, time = 1:6)
expect_equal(dim(x), c(6, 4))
expect_equal(colnames(x), c("lower", "median", "upper", "time"))
expect_equal(
  x$upper,
  as.vector(summary(fit)$summary[paste0("interval[", 1:6, "]"), "75%"])
  )
expect_equal(
  x$lower,
  as.vector(summary(fit)$summary[paste0("interval[", 1:6, "]"), "25%"])
  )
expect_equal(
  x$median,
  as.vector(summary(fit)$summary[paste0("interval[", 1:6, "]"), "50%"])
  )

# plot
x <- getPlotData(fit, prop = 0.8, time = 1:6)
