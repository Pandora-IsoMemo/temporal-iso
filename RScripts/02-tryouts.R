
library(rstan)
library(gdata)
library(bayesplot)
library(ggplot2)
library(OsteoBioR)

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

stan_dens(fit)
class(fit)
suppressMessages(plot(fit, plotfun = "stan_plot", pars = c("interval")) + ggtitle("Isotopic Values", subtitle = "Credibility Intervals") +
  scale_y_continuous(breaks = 1:6, labels = as.vector(1:6)))

dat <- .make_plot_data(fit, pars = "interval")
## "true" posterior known
trueIsoMean = c(-9, -12, -13, -12, -11, -11)
trueIsoSD = c(1.5, 2, 1.5, 2, 1.5, 1.5)
y <- resultFromTrue(dat, trueIsoMean = trueIsoMean, trueIsoSD = trueIsoSD)
y_mean <- as.vector(unlist(y$mean))
y_sigma <- as.vector(unlist(y$sd))
stan_data <- list(N = 4, T = 6, y_mean = y_mean, y_sigma = y_sigma, t = t, y = x)

fit <- stan(file = stan_model1, data = stan_data, warmup = 500, iter = 1000, chains = 4, cores = 2, thin = 1,
            control = list(adapt_delta = 0.9999, max_treedepth = 25))
fit

## larger n:
dat <- simulData(seed = 1)
dat
t <- dat$intStart
x <- t(as.matrix(mutate_all(dat[c(paste0("bone", 1:3), paste0("tooth", 1:5))], calcInfluence)))
trueIsoMean = c(-9, -11, -13, -12, -11, -11, -10, -9, -11, -10)
trueIsoSD = c(1.5, 2, 1.5, 2, 1.5, 1.5, 1.5, 2, 1.5, 2)
y <- resultFromTrue(dat, trueIsoMean = trueIsoMean, trueIsoSD = trueIsoSD)
y_mean <- as.vector(unlist(y$mean))
y_sigma <- as.vector(unlist(y$sd))
stan_data <- list(N = 8, T = 10, y_mean = y_mean, y_sigma = y_sigma, t = t, y = x)
fit <- stan(file = stan_model1, data = stan_data, warmup = 500, iter = 1000, chains = 4, cores = 2, thin = 1,
            control = list(adapt_delta = 0.9999, max_treedepth = 25))
fit
