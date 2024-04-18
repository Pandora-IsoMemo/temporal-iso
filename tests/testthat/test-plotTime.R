load(testthat::test_path("testdata/testObjectDefault.RData"))
load(testthat::test_path("testdata/testObjectGap.RData"))

testthat::test_that("plotTime",  {
  plot1 <- plotTime(object = testObjectDefault1, prop = 0.8, yLim = c(-10,-5), xLim = c(0, 8),
                    color = "#002350")
  
  expect_equal(plot1$labels, list(x = "Time", y = "Estimate", 
                                  title = "80%-Credibility-Interval for isotopic values over time", 
                                  colour = "individual",
                                  ymin = "lower", ymax = "upper",
                                  fill = "individual",
                                  shape = "individual",
                                  size = "individual"))
  
  plot2 <- plotTime(object = testObjectGap1, prop = 0.8, yLim = c(-10,-5), xLim = c(0, 4),
                    color = "#002350")
  
  expect_equal(plot2$labels, list(x = "Time", y = "Estimate", 
                                  title = "80%-Credibility-Interval for isotopic values over time", 
                                  colour = "individual",
                                  ymin = "lower", ymax = "upper",
                                  fill = "individual",
                                  shape = "individual",
                                  size = "individual"))
})

testthat::test_that("basePlotTime",  {
  plotData <- getPlotData(object = testObjectDefault1, prop = 0.8, deriv = "1") %>%
    updateTime(object = testObjectDefault1, deriv = "1")
  plotDataDF <- list(ind_1 = plotData) %>%
    extractPlotDataDF(models = "ind_1",
                      credInt = 0.8) %>%
    na.omit()
  
  pointStyleList <- list() %>%
    getDefaultPointFormatForModels(modelNames = c("ind_1"))
  
  # specify x, y limits
  plot <- basePlotTime(x = plotDataDF,
                       yLim = c(-10,-5),
                       xLim = c(0, 8)) %>%
    setTitles(prop = 0.8, xAxisLabel = "Time", yAxisLabel = "Estimate") %>%
    setXAxisLabels(xAxisData = list(ind_1 = plotData) %>%
                     extractAllXAxisData(), # labels for all x axis data
                   extendLabels = FALSE, 
                   xLim = c(0, 8), 
                   deriv = FALSE,
                   plotShifts = FALSE) %>%
    drawLinesAndRibbon(
      pointStyleList = pointStyleList,
      alphaL = 0.7,
      alphaU = 0.1,
      legendName = "testLegend"
      ) %>%
    setLegendPosition(hideLegend = FALSE,
                      legendPosition = "top")
  
  expect_equal(
    plot$labels,
    list(
      x = "Time",
      y = "Estimate",
      title = "80%-Credibility-Interval for isotopic values over time",
      colour = "individual",
      ymin = "lower",
      ymax = "upper",
      fill = "individual",
      shape = "individual",
      size = "individual"
    )
  )
  expect_equal(plot$coordinates$limits, list(x = c(0, 8), y = c(-10, -5)))
  
  # default x, y limits
  plot <- basePlotTime(x = plotDataDF) %>%
    setXAxisLabels(xAxisData = list(ind_1 = plotData) %>%
                     extractAllXAxisData(), # labels for all x axis data
                   extendLabels = FALSE, 
                   deriv = FALSE,
                   plotShifts = FALSE) %>%
    drawLinesAndRibbon(
      pointStyleList = pointStyleList,
      alphaL = 0.7,
      alphaU = 0.1,
      legendName = "testLegend"
    )
  
  expect_equal(
    plot$labels,
    list(
      x = "time",
      y = "median",
      colour = "individual",
      ymin = "lower",
      ymax = "upper",
      fill = "individual",
      shape = "individual",
      size = "individual"
    )
  )
  expect_equal(plot$coordinates$limits,
               list(
                 x = c(xmin = 0.5, xmax = 5.5),
                 y = c(
                   ymin = -12.7512327337153,
                   ymax = -4.60003088235379
                 )
               ))
})

testthat::test_that("drawLinesAndRibbon",  {
  plotData1 <- getPlotData(object = testObjectDefault1, prop = 0.8, deriv = "1") %>%
    updateTime(object = testObjectDefault1, deriv = "1")
  plotData2 <- getPlotData(object = testObjectGap1, prop = 0.8, deriv = "1") %>%
    updateTime(object = testObjectGap1, deriv = "1")
  allPlotDataDF <- list(ind_1 = plotData1,
                        ind_2 = plotData2) %>%
    extractPlotDataDF(models = c("ind_1", "ind_2"),
                      credInt = 0.8) %>%
    na.omit()
  
  pointStyleList <- list() %>%
    getDefaultPointFormatForModels(modelNames = c("ind_1", "ind_2"))
  
  plot <- basePlotTime(x = allPlotDataDF) %>%
    drawLinesAndRibbon(
      pointStyleList = pointStyleList,
      alphaL = 0.7,
      alphaU = 0.1,
      legendName = "testLegend"
    )
  
  expect_equal(
    plot$labels,
    list(
      x = "time",
      y = "median",
      colour = "individual",
      ymin = "lower",
      ymax = "upper",
      fill = "individual",
      shape = "individual",
      size = "individual"
    )
  )
  expect_equal(plot$coordinates$limits,
               list(
                 x = c(xmin = 0.5, xmax = 5.5),
                 y = c(
                   ymin = -12.7512327337153,
                   ymax = -4.60003088235379
                 )
               ))
})

testthat::test_that("setSecondYAxis",  {
  plotData1 <- getPlotData(object = testObjectDefault1, prop = 0.8, deriv = "1") %>%
    updateTime(object = testObjectDefault1, deriv = "1")
  plotData2 <- getPlotData(object = testObjectGap1, prop = 0.8, deriv = "1") %>%
    updateTime(object = testObjectGap1, deriv = "1")
  allPlotDataDF <- list(ind_1 = plotData1,
                        ind_2 = plotData2) %>%
    extractPlotDataDF(models = c("ind_1", "ind_2"),
                      credInt = 0.8) %>%
    na.omit()
  
  pointStyleList <- list() %>%
    getDefaultPointFormatForModels(modelNames = c("ind_1", "ind_2"))
  plotTexts <- getDefaultTextFormat()
  
  index <- allPlotDataDF$individual == "ind_2"
  rescaling <- getRescaleParams(oldLimits = getYRange(allPlotDataDF) %>% unlist(),
                                newLimits = getYRange(allPlotDataDF[index, ]) %>% unlist(),
                                secAxis = TRUE)
  
  plot <- allPlotDataDF %>%
    rescaleSecondAxisData(individual = "ind_2",
                          rescaling = rescaling) %>%
    basePlotTime() %>%
    shinyTools::formatTitlesOfGGplot(text = plotTexts) %>%
    drawLinesAndRibbon(
      pointStyleList = pointStyleList,
      alphaL = 0.7,
      alphaU = 0.1,
      legendName = "testLegend"
    ) %>%
    setSecondYAxis(rescaling = rescaling,
                   titleFormat = plotTexts[["yAxisTitle"]],
                   textFormat = plotTexts[["yAxisText"]],
                   yAxisLabel = "axis2Test",
                   yAxisTitleColor = "#002350")
  
  expect_equal(plot$labels, list(x = "time", y = "median", colour = "individual", ymin = "lower", 
                                 ymax = "upper", fill = "individual", shape = "individual", 
                                 size = "individual"))
  expect_equal(plot$coordinates$limits, 
               list(x = c(xmin = 0.5, xmax = 5.5), 
                    y = c(ymin = -12.7512327337153, ymax = -4.60003088235379)))
})
