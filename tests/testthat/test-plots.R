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
    time = c(1L, 2L, 3L, 4L, 5L, 6L)
  )
  
  plotData2Target <- data.frame(
    lower = c(-3.5070468715384,-1.88975603193923,
              -1.69800335864272,-1.99359111187714,-2.17984057504426),
    median = c(-1.20999187104465,0.0488322980614955,
               0.419012970657708,0.166373227374931,0.0766007232233719),
    upper = c(0.541802095090819,2.13142150909996,
              2.73026801430583,2.30758491892875,2.34360361651566),
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
    time = c(1L, 2L, 3L, 4L, 5L, 6L)
  )
  
  plotData2Target <- data.frame(
    lower = c(-3.48242212158476,-1.83578723354548,
              -1.75177415109341,-2.10696519877873,-2.29947596900347),
    median = c(-1.1701378721665,-0.00215025735315155,
               0.372613606234133,0.142404333952154,0.0421910046768903),
    upper = c(0.584632457040193,2.11626139721327,
              2.58128427989846,2.40079211685847,2.29467532444726),
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
    lower = c(0, 1, 2),
    upper = c(1, 2, 3)
  ))
  testthat::expect_equal(xAxisData2, data.frame(
    time = c(0.5, 1.5, 2.5, 4.5, 5.5),
    lower = c(0, 1, 2, 4, 5),
    upper = c(1, 2, 3, 5, 6)
  ))
})


testthat::test_that("getLabel, deriv = 1",  {
  label1 <- getLabel(xAxisData = data.frame(time = c(0.5, 1.5, 2.5),
                                            lower = c(0, 1, 2),
                                            upper = c(1, 2, 3)),
                     deriv = "1")
  label2 <- getLabel(xAxisData = data.frame(time = c(0.5, 1.5, 2.5, 4.5, 5.5),
                                            lower = c(0, 1, 2, 4, 5),
                                            upper = c(1, 2, 3, 5, 6)),
                     deriv = "1")
  label3 <- getLabel(xAxisData = data.frame(time = c(0, 1, 2, 3, 4, 5),
                                            lower = c(0, 1, 2, 3, 4, 5),
                                            upper = c(0, 1, 2, 3, 4, 5)),
                     deriv = "1")
  label4 <- getLabel(xAxisData = data.frame(time = c(1, 2, 3, 4, 5, 6),
                                            lower = c(1, 2, 3, 4, 5, 6),
                                            upper = c(1, 2, 3, 4, 5, 6)),
                     deriv = "1")
  
  testthat::expect_equal(label1, c("[0-1]", "[1-2]", "[2-3]"))
  testthat::expect_equal(label2, c("[0-1]", "[1-2]", "[2-3]", "[4-5]", "[5-6]"))
  testthat::expect_equal(label3, c(0, 1, 2, 3, 4, 5))
  testthat::expect_equal(label4, c(1, 2, 3, 4, 5, 6))
})


testthat::test_that("getLabel, deriv = 2",  {
  label1 <- getLabel(xAxisData = data.frame(time = c(0.5, 1.5, 2.5),
                                            lower = c(0, 1, 2),
                                            upper = c(1, 2, 3)),
                     deriv = "2")
  label2 <- getLabel(xAxisData = data.frame(time = c(0.5, 1.5, 2.5, 4.5, 5.5),
                                            lower = c(0, 1, 2, 4, 5),
                                            upper = c(1, 2, 3, 5, 6)),
                     deriv = "2",
                     hidePastedLabels = FALSE)
  label3 <- getLabel(xAxisData = data.frame(time = c(0, 1, 2, 3, 4, 5),
                                            lower = c(0, 1, 2, 3, 4, 5),
                                            upper = c(0, 1, 2, 3, 4, 5)),
                     deriv = "2")
  label4 <- getLabel(xAxisData = data.frame(time = c(1, 2, 3, 4, 5, 6),
                                            lower = c(1, 2, 3, 4, 5, 6),
                                            upper = c(1, 2, 3, 4, 5, 6)),
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


testthat::test_that("getDefaultPlotRange, deriv = 2",  {
  savedModels <- list(
    list(fit = testObjectDefault1),
    list(fit = testObjectDefault2),
    list(fit = testObjectGap1),
    list(fit = testObjectGap11)
  )
  testRange1 <- getDefaultPlotRange(savedModels, deriv = "1")
  testRange2 <- getDefaultPlotRange(savedModels, deriv = "2")
  
  testthat::expect_equal(testRange1 %>% unlist(),
                         c(xmin = 0.5, xmax = 6, 
                           ymin = -12.9045490738921, ymax = -4.58609303324681))
  testthat::expect_equal(testRange2 %>% unlist(),
                         c(xmin = 1, xmax = 5.5,
                           ymin = -4.2100298618177, ymax = 3.69646935224212))
})

testthat::test_that("addXScale", {
  x <- getPlotData(testObjectDefault1, prop = 0.8, time = NULL, deriv = "1")
  x$time <- adjustTimeColumn(objectTime = testObjectDefault1@time, deriv = "1")
  
  p <- ggplot(x, aes(x = .data[["time"]])) + 
    geom_line(aes(y = .data[["median"]]), alpha = 0.9)
  
  oldXAxisData <- structure(list(
    time = c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5), 
    lower = c(0, 1, 2, 3, 4, 5), 
    upper = c(1, 2, 3, 4, 5, 6)),
    class = "data.frame", 
    row.names = c(NA,  -6L))
  
  pTest <- p %>%
    addXScale(xAxisData = getXAxisData(object = testObjectDefault1, oldXAxisData = oldXAxisData),
              deriv = "1", 
              xLim = c(-1, 8))
  
  expect_true(inherits(pTest$data, "data.frame"))
})
