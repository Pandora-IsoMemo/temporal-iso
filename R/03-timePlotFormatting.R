# We need a separate namespace for formatting inputs for the model down- and upload.
# With grepl we can get the relevant inputs for formatting

#' UI function of time plot formatting
#'
#' @param id module id
#'
#' @return actionButton
#' @export
timePlotFormattingUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(8,
             selectizeInput(ns("plotTimeModels"), "Display Models / Individuals", 
                            choices = NULL,
                            multiple = TRUE,
                            width = "100%")),
      column(2,
             selectizeInput(ns("formatTimePlot"), "Format Model / Individual",
                            choices = NULL)),
      column(2,
             align = "right",
             style = "margin-top: 1.2em;",
             actionButton(ns("applyFormatToTimePlot"), "Apply"))
    ),
    tags$br(),
    fluidRow(
      column(4,
             tags$h4("Plot"),
             radioButtons(ns("deriv"), 
                          "Type", 
                          choices = c("Absolute values" = "1", "First derivate" = "2")), 
             sliderInput(ns("modCredInt"),
                         "Credibility interval:",
                         min = 0,
                         max = .99,
                         value = .8,
                         step = .05),
             selectizeInput(ns("secAxisModel"), "Add a new secondary axis",
                            choices = c("Choose one Model / Individual ..." = "")),
             helpText("The first element from 'Display Models / Individuals' is always used for the first (left) axis."),
             conditionalPanel(
               ns = ns,
               condition = "input.secAxisModel != ''",
               textInput(ns("secAxisText"), label = "Title of secondary axis",
                         value = "Estimate"),
               colourInput(ns("secAxisColor"),
                           label = "Title color of secondary axis",
                           value = config()[["defaultIntervalTimePlotTitle"]][["color"]])
             )
      ),
      column(2,
             shinyTools::plotTitlesUI(
               id = ns("plotLabels"),
               type = "ggplot",
               initText = list(plotTitle = config()[["defaultIntervalTimePlotTitle"]])
               )
      ),
      column(2,
             shinyTools::plotRangesUI(
               id = ns("plotRanges"), 
               initRanges = list(xAxis = config()[["plotRange"]],
                                 yAxis = config()[["plotRange"]])
             ),
             checkboxInput(inputId = ns("extendLabels"),
                           label = "Extend x-axis labels to lower and upper limits",
                           value = FALSE)
      ),
      column(2,
             tags$h4("Lines"),
             colourInput(inputId = ns("colorL"),
                         label = "Color line",
                         value = rgb(0, 35 / 255, 80 / 255, alpha = 0.6)),
             sliderInput(ns("alphaL"), "Transparency lines", min = 0, max = 1, value = 0.9),
             tags$br(),
             tags$br(),
             colourInput(inputId = ns("colorU"),
                         label = "Color uncertainty region",
                         value = rgb(0, 35 / 255, 80 / 255, alpha = 0.6)),
             sliderInput(ns("alphaU"),
                         "Transparency uncertainty region",
                         min = 0, 
                         max = 1,
                         value = 0.1)
      ),
      column(2,
             shinyTools::plotPointsUI(id = ns("pointStyle"),
                                      initStyle = config()[["defaultPointStyle"]])
      )
    )
  )
}

#' Server function of time plot formatting
#'
#' Backend for plot formatting module
#'
#' @param id namespace id
#'
#' @export
timePlotFormattingServer <- function(id, fit, savedModels) {
  moduleServer(id,
               function(input, output, session) {
                 # shift all observers calling inputs from above UI into this server
                 # remove the code from server.R
                 
                 formattedPlot <- reactiveVal()
                 
                 plotTexts <- shinyTools::plotTitlesServer(
                   "plotLabels",
                   type = "ggplot", 
                   initText = list(plotTitle  = config()[["defaultIntervalTimePlotTitle"]],
                                   xAxisTitle = config()[["defaultIntervalTimePlotTitle"]],
                                   yAxisTitle = config()[["defaultIntervalTimePlotTitle"]],
                                   xAxisText  = config()[["defaultIntervalTimePlotText"]],
                                   yAxisText  = config()[["defaultIntervalTimePlotText"]])
                 )
                 plotRanges <- shinyTools::plotRangesServer(
                   "plotRanges",
                   type = "ggplot",
                   initRanges = list(xAxis = config()[["plotRange"]],
                                     yAxis = config()[["plotRange"]])
                 )
                 
                 pointStyle <- shinyTools::plotPointsServer(
                   "pointStyle", 
                   type = "ggplot",
                   initStyle = config()[["defaultPointStyle"]]
                   )
                 
                 allXAxisData <- reactiveVal(data.frame())
                 pointStyleList <- reactiveValues()
                 lineStyleList <- reactiveValues()
                 
                 observe({
                   req(length(savedModels()) > 0)
                   modelChoices <- names(savedModels())
                   
                   # setup lists with default values for style specs
                   for (i in modelChoices) {
                     if (is.null(pointStyleList[[i]])) pointStyleList[[i]] <- config()[["defaultPointStyle"]]
                     if (is.null(lineStyleList[[i]])) lineStyleList[[i]] <- config()[["defaultLineStyle"]]
                   }
                   
                   selectedModel <- names(savedModels())[length(savedModels())]
                   
                   #fit(savedModels()[[selectedModel]]$fit)
                   
                   # inputs in tab "Credibility intervals over time"
                   updateSelectizeInput(session, "plotTimeModels", 
                                        choices = modelChoices, selected = selectedModel)
                   updateSelectizeInput(session, "formatTimePlot", choices = modelChoices)
                   
                   allXAxisData(extractAllXAxisData(models = savedModels(), allXAxisData = allXAxisData()))
                 }) %>%
                   bindEvent(savedModels())
                 
                 observe({
                   nDisplayedModels <- length(input[["plotTimeModels"]])
                   if (nDisplayedModels > 1) {
                     # remove first element that always gives the first axis
                     secAxisChoices <- input[["plotTimeModels"]][2:nDisplayedModels]
                     updateSelectizeInput(session, "secAxisModel", 
                                          choices = c("Choose one Model / Individual ..." = "", 
                                                      secAxisChoices))
                   } else {
                     updateSelectizeInput(session, "secAxisModel", 
                                          choices = c("Choose one Model / Individual ..." = ""))
                   }
                 }) %>%
                   bindEvent(input[["plotTimeModels"]])
                 
                 observe({
                   req(input[["formatTimePlot"]])
                   # observe point style
                   pointStyleList[[input[["formatTimePlot"]]]] <- pointStyle
                   # observe line style
                   lineStyleList[[input[["formatTimePlot"]]]][["colorL"]] <- input[["colorL"]]
                   lineStyleList[[input[["formatTimePlot"]]]][["colorU"]] <- input[["colorU"]]
                   lineStyleList[[input[["formatTimePlot"]]]][["alphaL"]] <- input[["alphaL"]]
                   lineStyleList[[input[["formatTimePlot"]]]][["alphaU"]] <- input[["alphaU"]]
                   #lineStyleList[[input[["formatTimePlot"]]]][["secAxis"]] <- input[["secAxis"]]
                 }) %>%
                   bindEvent(input[["applyFormatToTimePlot"]])
                 
                 allFits <- reactive({
                   getEntry(savedModels(), "fit")
                 })
                 
                 allExtractedPlotData <- reactive({
                   # extract plot data from model object
                   lapply(allFits(), function(x) {
                     extractPlotData(object = x, 
                                     prop = input$modCredInt, 
                                     deriv = input$deriv)
                   })
                 })
                 
                 observe({
                   req(savedModels(), input[["plotTimeModels"]])
                   
                   # draw basePlot (first element of input[["plotTimeModels"]])
                   firstModel <- input[["plotTimeModels"]][1]
                   basePlotData <- allExtractedPlotData()[[firstModel]]
                   p <- basePlotTime(x = basePlotData) %>%
                     setTitles(prop = input$modCredInt) %>%
                     shinyTools::formatTitlesOfGGplot(text = plotTexts) %>%
                     shinyTools::formatRangesOfGGplot(ranges = plotRanges) %>%
                     setXAxisLabels(xAxisData = allXAxisData(),
                                    extendLabels = input$extendLabels, 
                                    xLim = getLim(plotRanges = plotRanges, axis = "xAxis"), 
                                    deriv = input$deriv,
                                    plotShifts = FALSE) %>%
                     drawLinesAndRibbon(x = basePlotData,
                                        colorL = lineStyleList[[firstModel]]$colorL,
                                        colorU = lineStyleList[[firstModel]]$colorU,
                                        alphaL = lineStyleList[[firstModel]]$alphaL, 
                                        alphaU = lineStyleList[[firstModel]]$alphaU) %>%
                     shinyTools::formatPointsOfGGplot(data = basePlotData,
                                                      aes(x = .data[["time"]], y = .data[["median"]]), 
                                                      pointStyle = pointStyleList[[firstModel]])
                   
                   # loop over multiple elements of input[["plotTimeModels"]]
                   nDisplayedModels <- length(input[["plotTimeModels"]])
                   if (nDisplayedModels > 1) {
                     for (i in input[["plotTimeModels"]][2:nDisplayedModels]) {
                       layerPlotData <- allExtractedPlotData()[[i]]
                       ## use always data based newYLimits, we only set global limits not(!) per model
                       rescaling <- getRescaleParams(oldLimits = p$coordinates$limits$y,
                                                     newLimits = getYRange(layerPlotData) %>% unlist(),
                                                     secAxis = input[["secAxisModel"]] == i)
                       layerPlotData <- layerPlotData %>%
                         rescaleLayerData(rescaling = rescaling)
                       
                       p <- p %>%
                         setSecondYAxis(rescaling = rescaling,
                                        titleFormat = plotTexts[["yAxisTitle"]],
                                        textFormat = plotTexts[["yAxisText"]],
                                        yAxisLabel = input[["secAxisText"]],
                                        yAxisTitleColor = input[["secAxisColor"]]) %>%
                         setPlotLimits(newData = layerPlotData) %>%
                         drawLinesAndRibbon(x = layerPlotData,
                                            colorL = lineStyleList[[i]]$colorL,
                                            colorU = lineStyleList[[i]]$colorU,
                                            alphaL = lineStyleList[[i]]$alphaL,
                                            alphaU = lineStyleList[[i]]$alphaU) %>%
                         shinyTools::formatPointsOfGGplot(data = layerPlotData,
                                                          aes(x = .data[["time"]], y = .data[["median"]]), 
                                                          pointStyle = pointStyleList[[i]])
                     }
                   }
                   
                   formattedPlot(p)
                 }) #%>% bindEvent(input[["plotTimeModels"]])
                 
                 observeEvent(fit(), { # when do we need this??
                   # not as default plot, when fit() is ready, xLim & yLim are not: DO WE NEED THIS?
                   req(fit(), formattedPlot())
                   browser()
                   p <- plotTime(fit(), prop = input$modCredInt, 
                                 xLim = getLim(plotRanges, axis = "xAxis"), 
                                 yLim = getLim(plotRanges, axis = "yAxis"),
                                 deriv = input$deriv,
                                 oldXAxisData = allXAxisData(), # draws ticks at all data's times of x axis
                                 colorL = input$colorL, colorU = input$colorU,
                                 alphaL = input$alphaL, alphaU =  input$alphaU,
                                 extendLabels = input$extendLabels,
                                 pointStyle = pointStyle) %>%
                     setTitles(prop = input$modCredInt) %>%
                     shinyTools::formatTitlesOfGGplot(text = plotTexts)
                   formattedPlot(p)
                 })
                 
                 return(reactive(formattedPlot()))
               })
}
