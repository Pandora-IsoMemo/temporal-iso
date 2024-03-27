load("testObjectDefault.RData")
load("testObjectGap.RData")

testthat::test_that("plotTime",  {
  plot1 <- plotTime(object = testObjectDefault1, prop = 0.8, yLim = c(-10,-5), xLim = c(0, 8),
                    colorL = "#002350", colorU = "#002350")
  
  expect_equal(plot1$labels, list(x = "Time", y = "Estimate", 
                                  title = "80%-Credibility-Interval for isotopic values over time", 
                                  ymin = "lower", ymax = "upper"))
  
  plot2 <- plotTime(object = testObjectGap1, prop = 0.8, yLim = c(-10,-5), xLim = c(0, 4),
                    colorL = "#002350", colorU = "#002350")
  
  expect_equal(plot2$labels, list(x = "Time", y = "Estimate", 
                                  title = "80%-Credibility-Interval for isotopic values over time", 
                                  ymin = "lower", ymax = "upper"))
  
  
  xAxisData1 <- getXAxisData(object = testObjectDefault1)
  oldXAxisData <- getXAxisData(object = testObjectGap1, oldXAxisData = xAxisData1)
  
  plot <- plotTime(object = testObjectGap1, prop = 0.8, yLim = c(-10,-5), xLim = c(0, 8),
                   oldPlot = plot1, oldXAxisData = oldXAxisData,
                   colorL = "#002350", colorU = "#002350")
  
  expect_equal(plot$labels, list(x = "Time", y = "Estimate", 
                                 title = "80%-Credibility-Interval for isotopic values over time", 
                                 ymin = "lower", ymax = "upper"))
})

testthat::test_that("basePlotTime",  {
  plotData <- extractPlotData(object = testObjectDefault1, prop = 0.8, deriv = "1")
  plot <- basePlotTime(x = plotData,
                       yLim = c(-10,-5), xLim = c(0, 8)) %>%
    drawLinesAndRibbon(x = plotData,
                       colorL = "#002350", colorU = "#002350", alphaL = 0.9, alphaU = 0.1) %>%
    formatPointsOfGGplot(data = plotData,
                         aes(x = .data[["time"]], y = .data[["median"]]), 
                         pointStyle = config()[["defaultPointStyle"]]) %>%
    setTitles(prop = 0.8, xAxisLabel = "Time", yAxisLabel = "Estimate") %>%
    setXAxisLabels(xAxisData = getXAxisData(object = testObjectDefault1),
                extendLabels = FALSE, 
                xLim = c(0, 8), 
                deriv = "1",
                plotShifts = FALSE)
  
  expect_equal(plot$labels, list(x = "Time", y = "Estimate",
                                 title = "80%-Credibility-Interval for isotopic values over time", 
                                 ymin = "lower", ymax = "upper"))
})

testthat::test_that("layerPlotTime",  {
  allModels <- list(
    list(fit = testObjectDefault1),
    list(fit = testObjectGap1),
    list(fit = testObjectGap11)
  )
  allXAxisData <- extractAllXAxisData(allModels)
  
  plotData1 <- extractPlotData(object = testObjectDefault1, prop = 0.8, deriv = "1")
  plot1 <- basePlotTime(x = plotData1,
                        yLim = c(-10,-5), xLim = c(0, 8)) %>%
    drawLinesAndRibbon(x = plotData1,
                       colorL = "#002350", colorU = "#002350", alphaL = 0.9, alphaU = 0.1) %>%
    formatPointsOfGGplot(data = plotData1,
                         aes(x = .data[["time"]], y = .data[["median"]]), 
                         pointStyle = config()[["defaultPointStyle"]]) %>%
    setTitles(prop = 0.8, xAxisLabel = "Time", yAxisLabel = "Estimate") %>%
    setXAxisLabels(xAxisData = allXAxisData,
                   extendLabels = FALSE, 
                   xLim = c(0, 8), 
                   deriv = "1",
                   plotShifts = FALSE)
  
  expect_equal(plot1$labels, list(x = "Time", y = "Estimate", 
                                  title = "80%-Credibility-Interval for isotopic values over time", 
                                  ymin = "lower", ymax = "upper"))
  
  # add another plot
  plotData2 <- extractPlotData(object = testObjectGap1, prop = 0.8, deriv = "1")
  plot <- plot1 %>%
    layerPlotTime(x = plotData2,
                  yLim = c(-10,-5)) %>%
    drawLinesAndRibbon(x = plotData2,
                       colorL = "#000000", colorU = "#000000", alphaL = 0.9, alphaU = 0.1) %>%
    formatPointsOfGGplot(data = plotData2,
                         aes(x = .data[["time"]], y = .data[["median"]]), 
                         pointStyle = list(dataPoints = list(symbol = 1, color = "#002350", 
                                                             colorBg = "#002350",  size = 5L,
                                                             alpha = 1L, lineWidthBg = 2L, 
                                                             hide = FALSE)))
  
  expect_equal(plot$labels, list(x = "Time", y = "Estimate", 
                                 title = "80%-Credibility-Interval for isotopic values over time", 
                                 ymin = "lower", ymax = "upper"))
  
  # add second axis
  plot <- plot1 %>%
    layerPlotTime(x = plotData2,
                  yLim = c(-10,-5),
                  secAxis = TRUE, yAxisLabel = "Estimate 2",) %>%
    drawLinesAndRibbon(x = plotData2,
                       colorL = "#000000", colorU = "#000000", alphaL = 0.9, alphaU = 0.1) %>%
    formatPointsOfGGplot(data = plotData2,
                         aes(x = .data[["time"]], y = .data[["median"]]), 
                         pointStyle = config()[["defaultPointStyle"]])
  
  expect_equal(plot$labels, list(x = "Time", y = "Estimate", 
                                 title = "80%-Credibility-Interval for isotopic values over time", 
                                 ymin = "lower", ymax = "upper"))
})
