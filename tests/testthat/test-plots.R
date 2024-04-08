load("testObjectDefault.RData")
load("testObjectGap.RData")

testthat::test_that("getPlotData: two date inputs",  {
  plotData1 <- getPlotData(testObjectDefault1, prop = 0.8, time = NULL, deriv = "1")
  plotData2 <- getPlotData(testObjectDefault1, prop = 0.8, time = NULL, deriv = "2")
  
  plotData1Target <- data.frame(
    lower = c(-9.81693325790957,-11.3222408192011,
              -11.4659266620266,-11.7865029656666,-12.0578255855421,
              -12.0719659127685),
    median = c(-8.16151403004918,-9.44719310014277,
               -9.37574433289511,-8.89758878855175,-8.77496693690095,
               -8.69272393530268),
    upper = c(-6.42936455233924,-7.75138719574058,
              -7.33261350484481,-6.06113063226461,-5.50042606430566,
              -5.27929770330058),
    sd = c(1.33713829843568, 1.38779685928719, 
           1.61491770752276, 2.2435686892909, 2.59687117559807, 2.70290343638949),
    time = c(1L, 2L, 3L, 4L, 5L, 6L)
  )
  
  plotData2Target <- data.frame(
    lower = c(-3.5070468715384,-1.88975603193923,
              -1.69800335864272,-1.99359111187714,-2.17984057504426),
    median = c(-1.20999187104465,0.0488322980614955,
               0.419012970657708,0.166373227374931,0.0766007232233719),
    upper = c(0.541802095090819,2.13142150909996,
              2.73026801430583,2.30758491892875,2.34360361651566),
    sd = c(1.64719396911037, 
           1.62120832948596, 1.83064197559426, 1.81419080780957, 1.88032877584619),
    time = c(1L, 2L, 3L, 4L, 5L)
  )
  
  testthat::expect_equal(plotData1, plotData1Target)
  testthat::expect_equal(plotData2, plotData2Target)
})


testthat::test_that("getPlotData: one date input",  {
  plotData1 <- getPlotData(testObjectDefault2, prop = 0.8, time = NULL, deriv = "1")
  plotData2 <- getPlotData(testObjectDefault2, prop = 0.8, time = NULL, deriv = "2")
  
  plotData1Target <- data.frame(
    lower = c(-9.89907656206635,-11.2624404529165,
              -11.4539884165524,-11.742349885122,-12.0660965464668,
              -12.2113444038383),
    median = c(-8.16935551885413,-9.44021686214228,
               -9.42891260344628,-9.08031754669828,-8.91294666761698,
               -8.86533944497804),
    upper = c(-6.4244288140299,-7.81582599339432,
              -7.38509654640456,-6.30377374939198,-5.64332388194448,
              -5.54158337328692),
    sd = c(1.34885080466708, 1.35703875903698, 
           1.5895692220176, 2.20307418350219, 2.52256867044183, 2.66143079125866
    ),
    time = c(1L, 2L, 3L, 4L, 5L, 6L)
  )
  
  plotData2Target <- data.frame(
    lower = c(-3.48242212158476,-1.83578723354548,
              -1.75177415109341,-2.10696519877873,-2.29947596900347),
    median = c(-1.1701378721665,-0.00215025735315155,
               0.372613606234133,0.142404333952154,0.0421910046768903),
    upper = c(0.584632457040193,2.11626139721327,
              2.58128427989846,2.40079211685847,2.29467532444726),
    sd = c(1.63417082215553, 
           1.58450922848466, 1.79798685302716, 1.81358388856865, 1.89855532316841
    ),
    time = c(1L, 2L, 3L, 4L, 5L)
  )
  
  testthat::expect_equal(plotData1, plotData1Target)
  testthat::expect_equal(plotData2, plotData2Target)
})

  
testthat::test_that("adjustTimeColumn: deriv = 1",  {
  plotData1 <- adjustTimeColumn(objectTime = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), deriv = "1")
  plotData2 <- adjustTimeColumn(objectTime = c(1, 2, 3, 4, 5, 6), deriv = "1")
  plotData3 <- adjustTimeColumn(objectTime = c(0, 1, 2, 3, 4, 5), deriv = "1")
  
  testthat::expect_equal(plotData1, c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5))
  testthat::expect_equal(plotData2, c(1, 2, 3, 4, 5, 6))
  testthat::expect_equal(plotData3, c(0, 1, 2, 3, 4, 5))
})


testthat::test_that("adjustTimeColumn: deriv = 2",  {
  plotData1 <- adjustTimeColumn(objectTime = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), deriv = "2")
  plotData2 <- adjustTimeColumn(objectTime = c(1, 2, 3, 4, 5, 6), deriv = "2")
  
  testthat::expect_equal(plotData1, c(1, 2, 3, 4, 5))
  testthat::expect_equal(plotData2, c(1.5, 2.5, 3.5, 4.5, 5.5))
})


testthat::test_that("getXAxisData", {
  xAxisData1 <- getXAxisData(object = testObjectGap1, oldXAxisData = data.frame())
  xAxisData2 <- getXAxisData(object = testObjectGap11, oldXAxisData = xAxisData1)
  
  testthat::expect_equal(xAxisData1, data.frame(
    time = c(0.5, 1.5, 2.5),
    time_lower = c(0, 1, 2),
    time_upper = c(1, 2, 3)
  ))
  testthat::expect_equal(xAxisData2, data.frame(
    time = c(0.5, 1.5, 2.5, 4.5, 5.5),
    time_lower = c(0, 1, 2, 4, 5),
    time_upper = c(1, 2, 3, 5, 6)
  ))
})


testthat::test_that("getLabel, deriv = 1",  {
  label1 <- getLabel(xAxisData = data.frame(time = c(0.5, 1.5, 2.5),
                                            time_lower = c(0, 1, 2),
                                            time_upper = c(1, 2, 3)),
                     deriv = "1")
  label2 <- getLabel(xAxisData = data.frame(time = c(0.5, 1.5, 2.5, 4.5, 5.5),
                                            time_lower = c(0, 1, 2, 4, 5),
                                            time_upper = c(1, 2, 3, 5, 6)),
                     deriv = "1")
  label3 <- getLabel(xAxisData = data.frame(time = c(0, 1, 2, 3, 4, 5),
                                            time_lower = c(0, 1, 2, 3, 4, 5),
                                            time_upper = c(0, 1, 2, 3, 4, 5)),
                     deriv = "1")
  label4 <- getLabel(xAxisData = data.frame(time = c(1, 2, 3, 4, 5, 6),
                                            time_lower = c(1, 2, 3, 4, 5, 6),
                                            time_upper = c(1, 2, 3, 4, 5, 6)),
                     deriv = "1")
  
  testthat::expect_equal(label1, c("[0-1]", "[1-2]", "[2-3]"))
  testthat::expect_equal(label2, c("[0-1]", "[1-2]", "[2-3]", "[4-5]", "[5-6]"))
  testthat::expect_equal(label3, c(0, 1, 2, 3, 4, 5))
  testthat::expect_equal(label4, c(1, 2, 3, 4, 5, 6))
})


testthat::test_that("getLabel, deriv = 2",  {
  label1 <- getLabel(xAxisData = data.frame(time = c(0.5, 1.5, 2.5),
                                            time_lower = c(0, 1, 2),
                                            time_upper = c(1, 2, 3)),
                     deriv = "2")
  label2 <- getLabel(xAxisData = data.frame(time = c(0.5, 1.5, 2.5, 4.5, 5.5),
                                            time_lower = c(0, 1, 2, 4, 5),
                                            time_upper = c(1, 2, 3, 5, 6)),
                     deriv = "2",
                     hidePastedLabels = FALSE)
  label3 <- getLabel(xAxisData = data.frame(time = c(0, 1, 2, 3, 4, 5),
                                            time_lower = c(0, 1, 2, 3, 4, 5),
                                            time_upper = c(0, 1, 2, 3, 4, 5)),
                     deriv = "2")
  label4 <- getLabel(xAxisData = data.frame(time = c(1, 2, 3, 4, 5, 6),
                                            time_lower = c(1, 2, 3, 4, 5, 6),
                                            time_upper = c(1, 2, 3, 4, 5, 6)),
                     deriv = "2",
                     hidePastedLabels = FALSE)
  
  testthat::expect_equal(label1, c("[0-1]", "", "[1-2]", "", "[2-3]"))
  testthat::expect_equal(label2, c("[0-1]", "[0-1,1-2]", "[1-2]", "[1-2,2-3]", "[2-3]", "[2-3,4-5]", 
                                   "[4-5]", "[4-5,5-6]", "[5-6]"))
  testthat::expect_equal(label3, c("0", "", "1", "", "2", "", "3", "", "4", "", "5"))
  testthat::expect_equal(label4, c("1", "1,2", "2", "2,3", "3", "3,4", "4", "4,5", "5", "5,6", 
                                   "6"))
})


testthat::test_that("getBreaks, deriv = 1",  {
  breaks1 <- getBreaks(time = c(0.5, 1.5, 2.5), deriv = "1")
  breaks2 <- getBreaks(time = c(0.5, 1.5, 2.5, 4.5, 5.5), deriv = "1")
  breaks3 <- getBreaks(time = c(0, 1, 2, 3, 4, 5), deriv = "1")
  
  testthat::expect_equal(breaks1, c(0.5, 1.5, 2.5))
  testthat::expect_equal(breaks2, c(0.5, 1.5, 2.5, 4.5, 5.5))
  testthat::expect_equal(breaks3, c(0, 1, 2, 3, 4, 5))
})


testthat::test_that("getBreaks, deriv = 2",  {
  breaks1 <- getBreaks(time = c(0.5, 1.5, 2.5), deriv = "2")
  breaks2 <- getBreaks(time = c(0.5, 1.5, 2.5, 4.5, 5.5), deriv = "2")
  breaks3 <- getBreaks(time = c(0, 1, 2, 3, 4, 5), deriv = "2")
  
  testthat::expect_equal(breaks1, c(0.5, 1, 1.5, 2, 2.5))
  testthat::expect_equal(breaks2, c(0.5, 1, 1.5, 2, 2.5, 3.5, 4.5, 5, 5.5))
  testthat::expect_equal(breaks3, c(0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5))
})

testthat::test_that("extendXAxis", {
  oldXAxisData <- structure(list(
    time = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), 
    time_lower = c(0, 1, 2, 3, 4, 5), 
    time_upper = c(1, 2, 3, 4, 5, 6)),
    class = "data.frame", 
    row.names = c(NA,  -6L))
  
  xAxisData <- getXAxisData(object = testObjectDefault1, oldXAxisData = oldXAxisData)
  
  testXAxisData <- xAxisData %>%
    extendXAxis(xLabelLim = c(-1, 8))
  
  testthat::expect_equal(nrow(xAxisData) + 2, nrow(testXAxisData))
  
  breaks <- getBreaks(time = testXAxisData$time, deriv = "1")
  labels <- getLabel(xAxisData = testXAxisData, deriv = "1")
  
  testthat::expect_equal(breaks, c(-0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 7))
  testthat::expect_equal(labels, c("[-1-0]", "[0-1]", "[1-2]", "[2-3]", "[3-4]", "[4-5]", "[5-6]", "[6-8]"))
})

testthat::test_that("getLim", {
  testRanges <- list(xAxis = list(min = 0L, max = 1L, fromData = TRUE), 
                     yAxis = list(min = 0L, max = 1L, fromData = FALSE))
  
  expect_equal(getLim(testRanges, axis = "xAxis"), numeric(0))
  expect_equal(getLim(testRanges, axis = "yAxis"), 0:1)
})
