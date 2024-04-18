library(OsteoBioR)

#dataset with renewal rates
data <- data.frame(
  intStart = 0:5,
  intEnd = 1:6,
  bone1 = c(100, 50, 20, 10, 5, 2),
  bone2 = c(100, 10, 5, 1, 1, 1),
  tooth1 = c(0, 100, 0, 0, 0, 0),
  tooth2 = c(0, 0, 100, 0, 0, 0)
)
# isotope-values and sd
y_mean <- c(-10, -7, -12, -9)
y_sigma <- c(2, 1.5, 2.5, 2.5)


###############################
#estimate values
###############################
fit <- estimateIntervals(renewalRates = data,
                         timeVars = "intStart",
                         boneVars = c("bone1", "bone2", "tooth1", "tooth2"),
                         isoMean = y_mean,
                         isoSigma = y_sigma)
print(fit)
# implemented Methods
plot(fit)
plotTime(fit)
#get estimates for specific time points
estimateTimePoint(fit, time = seq(0,5, by = 0.5))

###############################
#shift point estimation
###############################
plotTime(fit, plotShifts = TRUE, threshold = 0.5)
plotTime(fit, plotShifts = TRUE, threshold = 0.5,
         type = TRUE, probability = 0.5)

getShiftTime(fit, threshold = 0.5)


###############################
#Staying time estimation
###############################

estimatedStayTimes <- getSiteStayTimes(object = fit,
                 siteMeans = c(-8, -10),
                 siteSigma = c(1, 1.5))


###############################
#compute resulting isotopic values out of historic ones
###############################

testDat <- data.frame(
  t = 1:3,
  bone1 = c(100, 50, 10, 10, 10, 10),
  bone2 = c(0, 20, 20, 20, 20, 20),
  teeth1 = c(0, 0, 0, 10, 50, 60),
  mean = c(1, 3, 3,0, -1, - 5),
  sd = c(1, 3, 1, 1.5, 2, 3)
)

computeResult(
  data = testDat,
  timeVars = "t",
  boneVars = c("bone1","bone2","teeth1"),
  meanVar = "mean",
  sdVar = "sd"
)

shiny::runApp(paste0(system.file(package = "OsteoBioR"),"/app"))
