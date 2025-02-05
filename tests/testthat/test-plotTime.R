load(testthat::test_path("testdata/testObjectDefault.RData"))
load(testthat::test_path("testdata/testObjectGap.RData"))

testthat::test_that("plotTime",  {
  plot <- plotTime(object = testObjectDefault1, prop = 0.8, 
                    plotShifts = FALSE,
                    deriv = "1", 
                    yLim = c(-10,-5), xLim = c(0, 8),
                    color = "#002350")
  
  expect_equal(plot$labels, list(x = "Time", y = "Estimate", 
                                  title = "80%-Credibility-Interval for isotopic values over time", 
                                  colour = "individual",
                                  ymin = "lower", ymax = "upper",
                                  fill = "individual",
                                  shape = "individual",
                                  size = "individual"))
  expect_equal(ggplot_build(plot)$layout$panel_scales_x[[1]]$breaks, 
               c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5))
  expect_equal(ggplot_build(plot)$layout$panel_scales_x[[1]]$labels, 
               c("[0-1]", "[1-2]", "[2-3]", "[3-4]", "[4-5]", "[5-6]"))
  
  # second derivation
  plot <- plotTime(object = testObjectDefault1, prop = 0.8, 
                    plotShifts = FALSE,
                    deriv = "2", 
                    yLim = c(-4,4), xLim = c(0, 8),
                    color = "#002350")
  
  expect_equal(plot$labels, list(x = "Time", y = "Estimate", 
                                 title = "80%-Credibility-Interval for isotopic values over time", 
                                 colour = "individual",
                                 ymin = "lower", ymax = "upper",
                                 fill = "individual",
                                 shape = "individual",
                                 size = "individual"))
  expect_equal(ggplot_build(plot)$layout$panel_scales_x[[1]]$breaks, 
               c(0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5))
  expect_equal(ggplot_build(plot)$layout$panel_scales_x[[1]]$labels, 
               c("[0-1]", "", "[1-2]", "", "[2-3]", "", "[3-4]", "", "[4-5]", "", "[5-6]"))
  
  # shifts
  plot <- plotTime(object = testObjectGap1, prop = 0.8, 
                   plotShifts = TRUE,
                   yLim = c(-10,-5), xLim = c(0, 4),
                   color = "#002350")
  
  expect_equal(plot$labels, list(x = "Time", y = "Estimate", 
                                 title = "80%-Credibility-Interval for isotopic values over time", 
                                 colour = "individual",
                                 ymin = "lower", ymax = "upper",
                                 fill = "individual",
                                 shape = "individual",
                                 size = "individual",
                                 xintercept = "xintercept"))
  expect_equal(ggplot_build(plot)$layout$panel_scales_x[[1]]$breaks, 
               c(0.5, 1.5, 2.5))
  expect_equal(ggplot_build(plot)$layout$panel_scales_x[[1]]$labels, 
               c("[0-1]", "[1-2]", "[2-3]"))
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
  plot <- basePlotTime(df = plotDataDF,
                       yLim = c(-10,-5),
                       xLim = c(0, 8)) %>%
    setDefaultTitles(prop = 0.8, xAxisLabel = "Time", yAxisLabel = "Estimate") %>%
    setXAxisLabels(xAxisData = list(ind_1 = plotData) %>%
                     extractAllXAxisData(), # labels for all x axis data
                   extendLabels = FALSE, 
                   xLim = c(0, 8), 
                   deriv = FALSE) %>%
    drawLinesAndRibbon(
      pointStyleList = pointStyleList,
      alphaL = 0.7,
      alphaU = 0.1
      )
  
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
  expect_equal(ggplot_build(plot)$layout$panel_scales_x[[1]]$breaks, 
               c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5))
  expect_equal(ggplot_build(plot)$layout$panel_scales_x[[1]]$labels, 
               c("[0-1]", "[1-2]", "[2-3]", "[3-4]", "[4-5]", "[5-6]"))
  
  # default x, y limits
  plot <- basePlotTime(df = plotDataDF) %>%
    setXAxisLabels(xAxisData = list(ind_1 = plotData) %>%
                     extractAllXAxisData(), # labels for all x axis data
                   extendLabels = FALSE, 
                   deriv = FALSE) %>%
    drawLinesAndRibbon(
      pointStyleList = pointStyleList,
      alphaL = 0.7,
      alphaU = 0.1
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
                 x = c(0.5, 5.5),
                 y = c(-12.7512327337153, -4.60003088235379)
               ))
  expect_equal(ggplot_build(plot)$layout$panel_scales_x[[1]]$breaks, 
               c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5))
  expect_equal(ggplot_build(plot)$layout$panel_scales_x[[1]]$labels, 
               c("[0-1]", "[1-2]", "[2-3]", "[3-4]", "[4-5]", "[5-6]"))
})

testthat::test_that("drawLinesAndRibbon",  {
  # second derivation
  plotData1 <- getPlotData(object = testObjectDefault1, prop = 0.8, deriv = "2") %>%
    updateTime(object = testObjectDefault1, deriv = "2")
  plotData2 <- getPlotData(object = testObjectGap1, prop = 0.8, deriv = "2") %>%
    updateTime(object = testObjectGap1, deriv = "2")
  allPlotDataDF <- list(ind_1 = plotData1,
                        ind_2 = plotData2) %>%
    extractPlotDataDF(models = c("ind_1", "ind_2"),
                      credInt = 0.8) %>%
    na.omit()
  
  pointStyleList <- list() %>%
    getDefaultPointFormatForModels(modelNames = c("ind_1", "ind_2"))
  
  plot <- basePlotTime(df = allPlotDataDF) %>%
    setXAxisLabels(xAxisData = list(ind_1 = plotData1,
                                    ind_2 = plotData2) %>%
                     extractAllXAxisData(), # labels for all x axis data
                   extendLabels = FALSE, 
                   deriv = "2") %>% # if "1" is set here, breaks in-between are left out
    drawLinesAndRibbon(
      pointStyleList = pointStyleList,
      alphaL = 0.7,
      alphaU = 0.1
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
               list(x = c(1, 5), y = c(-4.17929722147457, 3.35841030846769)))
  expect_equal(ggplot_build(plot)$layout$panel_scales_x[[1]]$breaks, 
               c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5))
  expect_equal(ggplot_build(plot)$layout$panel_scales_x[[1]]$labels, 
               c("[0.5-1.5]", "", "[1.5-2.5]", "", "[2.5-3.5]", "", "[3.5-4.5]", "", "[4.5-5.5]"))
})
