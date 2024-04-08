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
  expect_equal(plot$coordinates$limits, list(x = c(0, 8), y = c(-10, -5)))
})

testthat::test_that("basePlotTime",  {
  plotData <- getPlotData(object = testObjectDefault1, prop = 0.8, deriv = "1") %>%
    updateTime(time = testObjectDefault1@time, deriv = "1")
  # specify x, y limits
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
  expect_equal(plot$coordinates$limits, list(x = c(0, 8), y = c(-10, -5)))
  
  # default x, y limits
  plot <- basePlotTime(x = plotData) %>%
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
  expect_equal(plot$coordinates$limits, 
               list(x = c(xmin = 0.5, xmax = 5.5), 
                    y = c(ymin = -12.7512327337153, ymax = -4.60003088235379)))
})

testthat::test_that("setSecondYAxis",  {
  allModels <- list(
    list(fit = testObjectDefault1),
    list(fit = testObjectGap1),
    list(fit = testObjectGap11)
  )
  allXAxisData <- extractAllXAxisData(allModels)
  
  plotData1 <- getPlotData(object = testObjectDefault1, prop = 0.8, deriv = "1") %>%
    updateTime(time = testObjectDefault1@time, deriv = "1")
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
  expect_equal(plot1$coordinates$limits, list(x = c(0, 8), y = c(-10, -5)))
  
  # add another plot
  plotData2 <- getPlotData(object = testObjectGap1, prop = 0.8, deriv = "1") %>%
    updateTime(time = testObjectGap1@time, deriv = "1")
  rescaling <- getRescaleParams(oldLimits = plot1$coordinates$limits$y,
                                newLimits = getYRange(plotData2) %>% unlist(),
                                secAxis = FALSE)
  plotData2Re <- plotData2 %>%
    rescaleLayerData(rescaling = rescaling)
  plot <- plot1 %>%
    setSecondYAxis(rescaling = rescaling) %>%
    setPlotLimits(newData = plotData2) %>%
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
  expect_equal(plot$coordinates$limits, 
               list(x = c(xmin = 0.5, xmax = 5.5), 
                    y = c(ymin = -12.7512327337153, ymax = -4.60003088235379)))
  
  # add rescaling for second axis
  ## use always data-based new y limits! We now only set global limits not(!) per model
  rescaling <- getRescaleParams(oldLimits = plot1$coordinates$limits$y,
                                newLimits = getYRange(plotData2) %>% unlist(),
                                secAxis = TRUE)
  plotData2Re <- plotData2 %>%
    rescaleLayerData(rescaling = rescaling)
  plot <- plot1 %>%
    setSecondYAxis(rescaling = rescaling,
                   yAxisLabel = "Estimate 2") %>%
    setPlotLimits(newData = plotData2Re) %>%
    drawLinesAndRibbon(x = plotData2Re,
                       colorL = "#000000", colorU = "#000000", alphaL = 0.9, alphaU = 0.1) %>%
    formatPointsOfGGplot(data = plotData2Re,
                         aes(x = .data[["time"]], y = .data[["median"]]), 
                         pointStyle = config()[["defaultPointStyle"]])
  
  expect_equal(plot$labels, list(x = "Time", y = "Estimate", 
                                 title = "80%-Credibility-Interval for isotopic values over time", 
                                 ymin = "lower", ymax = "upper"))
  expect_equal(plot$coordinates$limits, 
               list(x = c(xmin = 0.5, xmax = 5.5), 
                    y = c(ymin = -12.7512327337153, ymax = -4.60003088235379)))
})
