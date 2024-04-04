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
    tags$h4("Time Plot"),
    plotOutput(ns("plotTime")) %>% withSpinner(color = "#20c997"),
    tags$br(),
    fluidRow(
      column(7,
             selectizeInput(ns("plotTimeModels"), "Display Models / Individuals", 
                            choices = c("Fit or import a model ..." = ""),
                            multiple = TRUE,
                            selected = "",
                            width = "100%")),
      column(1,
             align = "right",
             style = "margin-top: 1.2em;",
             actionButton(ns("applyFormatToTimePlot"), "Apply")),
      column(3,
             selectizeInput(ns("formatTimePlot"), "Format Model / Individual",
                            choices = c("Fit or import a model ..." = ""))),
      column(1,
             align = "right",
             style = "margin-top: 1.2em;",
             actionButton(ns("applyFormatToTimePlotModel"), "Apply"))
    ),
    tags$br(),
    fluidRow(
      column(4,
             tags$h4("Data"),
             radioButtons(ns("deriv"), 
                          "Type", 
                          choices = c("Absolute values" = "1", "First derivate" = "2"),
                          inline = TRUE,
                          width = "100%"), 
             sliderInput(ns("modCredInt"),
                         "Credibility interval:",
                         min = 0,
                         max = .99,
                         value = .8,
                         step = .05,
                         width = "100%"),
             tags$h4("Secondary Axis"),
             selectizeInput(ns("secAxisModel"), "Add a new secondary axis",
                            choices = c("Choose one Model / Individual ..." = "")),
             helpText("The first element from 'Display Models / Individuals' is always used for the first (left) axis."),
             conditionalPanel(
               ns = ns,
               condition = "input.secAxisModel != ''",
               fluidRow(
                 column(6, textInput(ns("secAxisText"), label = "Title",
                                     value = "Estimate")),
                 column(6, colourInput(ns("secAxisColor"),
                                       label = "Title color",
                                       value = config()[["defaultIntervalTimePlotTitle"]][["color"]]))
               ))
      ),
      column(2,
             shinyTools::plotTitlesUI(
               id = ns("plotLabels"),
               title = "Texts",
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
                                      title = "Points",
                                      initStyle = config()[["defaultPointStyle"]])
      )
    ),
    plotExportButton(ns("exportCredIntTimePlot")),
    tags$br(),
    tags$br(),
    tags$h4("Plot Data"),
    tableOutput(ns("plotData")),
    tags$br(),
    dataExportButton(ns("exportCredIntTimeData")),
    tags$br()
  )
}

#' Server function of time plot formatting
#'
#' Backend for plot formatting module
#'
#' @param id namespace id
#' @param savedModels list of models of class \code{\link{TemporalIso}}
#'
#' @export
timePlotFormattingServer <- function(id, savedModels) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 
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
                   
                   allXAxisData(extractAllXAxisData(models = savedModels(), 
                                                    allXAxisData = allXAxisData()))
                 }) %>%
                   bindEvent(savedModels())
                 
                 observe({
                   # choices for formatting of lines and points
                   updateSelectizeInput(session, "formatTimePlot", 
                                        choices = input[["plotTimeModels"]])
                   
                   # choices for secondary axis
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
                 }) %>%
                   bindEvent(input[["applyFormatToTimePlotModel"]])
                 
                 allFits <- reactive({
                   getEntry(savedModels(), "fit")
                 })

                 extractedPlotDataList <- reactive({
                   # extract plot data from model object
                   lapply(allFits(), function(x) {
                     extractPlotData(object = x,
                                     prop = input$modCredInt,
                                     deriv = input$deriv)
                   })
                 })
                 
                 extractedPlotDataDF <- reactive({
                   # filter for displayed models:
                   plotData <- extractedPlotDataList()[input[["plotTimeModels"]]] %>%
                     bind_rows(.id = "individual") %>%
                     # add column with 
                     mutate(cred_interval = sprintf("%.0f%%", input$modCredInt * 100)) %>%
                     group_by(.data$time) %>%
                     # add id for each x value:
                     mutate(id_time = cur_group_id()) %>% 
                     ungroup() %>%
                     group_by(.data$individual) %>%
                     # add id for each individual:
                     mutate(id_model = cur_group_id()) %>%
                     # add an empty line after each individual (containing only NA values):
                     do(add_na_row(.)) %>% 
                     ungroup() %>%
                     select("id_model", "individual", "id_time", "time", 
                            "cred_interval", "lower", "median", "upper") %>%
                     # remove last line containing only NA values
                     slice(1:(n() - 1))
                 })
                 
                 output$plotData <- renderTable({
                   validate(need(input[["plotTimeModels"]],
                                 "Choose at least one element from 'Display Models / Individuals' ..."))
                   extractedPlotDataDF()
                 })
                 
                 dataExportServer("exportCredIntTimeData", reactive(function() {extractedPlotDataDF()}))
                 
                 savedPlot <- reactiveVal(list())
                 
                 observe({
                   req(savedModels(), input[["plotTimeModels"]])
                   # draw basePlot (first element of input[["plotTimeModels"]])
                   firstModel <- input[["plotTimeModels"]][1]
                   basePlotData <- extractedPlotDataList()[[firstModel]]
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
                       layerPlotData <- extractedPlotDataList()[[i]]
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
                   savedPlot(p)
                 }) %>%
                   bindEvent(list(input[["applyFormatToTimePlot"]], 
                                  input[["applyFormatToTimePlotModel"]]))
                 
                 output$plotTime <- renderPlot({
                   validate(need(formattedPlot(), "Choose at least one element from 'Display Models / Individuals' and press 'Apply' ..."))
                   formattedPlot()
                 })
                 
                 plotExportServer("exportCredIntTimePlot",
                                  plotFun = reactive(function() savedPlot()),
                                  filename = sprintf("%s_Credibility_Intervals_Over_Time",
                                                     gsub("-", "", Sys.Date()))
                                  )
                 
                 return(reactive(formattedPlot()))
               })
}

#' Add NA Row
#' 
#' Function to add a row with NA values at the end of a data.frame
#' 
#' @param df (data.frame) data.frame
add_na_row <- function(df) {
  na_row <-  matrix(rep(NA, ncol(df)), 
                    nrow = 1, 
                    dimnames = list("", colnames(df))) %>% 
    as.data.frame()
  bind_rows(df, na_row)
}
