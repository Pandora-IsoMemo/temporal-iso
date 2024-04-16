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
      column(8,
             selectizeInput(ns("plotTimeModels"), "Display Models / Individuals", 
                            choices = c("Fit / import a model ..." = ""),
                            multiple = TRUE,
                            selected = "",
                            width = "100%")),
      column(1,
             align = "right",
             style = "margin-top: 1.2em;",
             actionButton(ns("applyFormatToTimePlot"), "Apply")),
      column(2,
             selectizeInput(ns("formatTimePlot"), "Format Model / Individual",
                            choices = c("Fit / import a model ..." = ""),
                            width = "100%")),
      column(1,
             align = "right",
             style = "margin-top: 1.2em;",
             actionButton(ns("applyFormatToTimePlotModel"), "Apply"))
    ),
    tags$br(),
    fluidRow(
      column(3,
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
             tags$br(),
             tags$h4("Secondary Y Axis"),
             selectizeInput(ns("secAxisModel"), "Add a new secondary axis",
                            choices = c("Choose one Model / Individual ..." = "")),
             helpText("The first element from 'Display Models / Individuals' is always used for the first (left) axis."),
             conditionalPanel(
               ns = ns,
               condition = "input.secAxisModel != ''",
               fluidRow(
                 column(6, textInput(ns("secAxisText"), label = "Title",
                                     value = "",
                                     placeholder = "Custom title ...")),
                 column(6, colourInput(ns("secAxisColor"),
                                       label = "Title color",
                                       value = config()[["defaultIntervalTimePlotTitle"]][["color"]]))
               ))
      ),
      column(2,
             shinyTools::plotTitlesUI(
               id = ns("plotLabels"),
               title = "Text",
               type = "ggplot",
               initText = list(plotTitle = config()[["defaultIntervalTimePlotTitle"]])
               )
      ),
      column(2,
             shinyTools::plotRangesUI(
               id = ns("plotRanges"), 
               title = "Axis Range",
               initRanges = list(xAxis = config()[["plotRange"]],
                                 yAxis = config()[["plotRange"]])
             ),
             checkboxInput(inputId = ns("extendLabels"),
                           label = "Extend x-axis labels to lower and upper limits",
                           value = FALSE)
      ),
      column(2,
             tags$h4("Transparency"),
             sliderInput(ns("alphaL"), "Points / Lines", min = 0, max = 1, value = 0.9),
             sliderInput(ns("alphaU"),
                         "Uncertainty Region",
                         min = 0,
                         max = 1,
                         value = 0.1),
             tags$br(),
             tags$h4("Legend"),
             checkboxInput(inputId = ns("hideLegend"),
                           label = "Hide legend",
                           value = FALSE)
      ),
      column(3,
             shinyTools::plotPointsUI(id = ns("pointStyle"),
                                      title = "Points / Lines",
                                      initStyle = config()[["defaultPointStyle"]])
      )
    ),
    fluidRow(column(12, align = "right", plotExportButton(ns("exportCredIntTimePlot")))),
    tags$hr(),
    tags$h4("Plot Data"),
    tableOutput(ns("plotData")),
    tags$br(),
    fluidRow(column(12, align = "right", dataExportButton(ns("exportCredIntTimeData")))),
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
                   initStyle = config()[["defaultPointStyle"]],
                   hideInput = c("hide", "alpha", "colorBg")
                   )
                 
                 pointStyleList <- reactiveValues()
                 
                 observe({
                   req(length(savedModels()) > 0)
                   modelChoices <- names(savedModels())
                   
                   # default colours
                   defaultColours <- ggplot2::scale_color_discrete()$palette(n = length(savedModels()))
                   names(defaultColours) <- modelChoices
                   
                   # setup lists with default values for style specs
                   for (i in modelChoices) {
                     if (is.null(pointStyleList[[i]])) 
                       pointStyleList[[i]] <- config()[["defaultPointStyle"]][["dataPoints"]]
                     # use default colour per model
                     pointStyleList[[i]]["color"] <- defaultColours[i]
                   }
                   
                   selectedModel <- names(savedModels())[length(savedModels())]
                   
                   #fit(savedModels()[[selectedModel]]$fit)
                   
                   # inputs in tab "Credibility intervals over time"
                   updateSelectizeInput(session, "plotTimeModels", 
                                        choices = modelChoices, selected = selectedModel)
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
                   pointStyleList[[input[["formatTimePlot"]]]] <- pointStyle[["dataPoints"]]
                 }) %>%
                   bindEvent(input[["applyFormatToTimePlotModel"]])
                 
                 allFits <- reactive({
                   getEntry(savedModels(), "fit")
                 })

                 extractedPlotDataList <- reactive({
                   # extract plot data from model object
                   lapply(allFits(), function(x) {
                     getPlotData(object = x, prop = input$modCredInt, deriv = input$deriv) %>%
                       updateTime(object = x, deriv = input$deriv)
                   })
                 })
                 
                 extractedPlotDataDF <- reactive({
                   extractPlotDataDF(plotDataList = extractedPlotDataList(),
                                      models = input[["plotTimeModels"]],
                                      credInt = input$modCredInt)
                 })
                 
                 output$plotData <- renderTable({
                   validate(need(input[["plotTimeModels"]],
                                 "Choose at least one element from 'Display Models / Individuals' ..."))
                   extractedPlotDataDF()
                 })
                 
                 plotDataExport <- reactiveVal()
                 
                 observe({
                   plotDataExport(extractedPlotDataDF())
                 }) %>%
                   bindEvent(input[["plotTimeModels"]])
                 
                 dataExportServer("exportCredIntTimeData",
                                  reactive(function() {plotDataExport()}))
                 
                 # set default: no rescaling
                 rescalingSecAxis <- reactiveVal(list(scale = 1, center = 0))
                 observe({
                   req(input[["plotTimeModels"]])
                   
                   plotData <- extractedPlotDataDF() %>%
                     na.omit()
                   # get index for filter
                   index <- plotData$individual == input[["secAxisModel"]]
                   
                   # get rescaling parameters
                   req(nrow(plotData[index, ]) > 0)
                   
                   # update title of second axis
                   updateTextInput(session, "secAxisText", 
                                   value = sprintf("%s Estimate", input[["secAxisModel"]]))
                   
                   ## use always data based newYLimits, we only set global limits not(!) per model
                   rescaling <- getRescaleParams(oldLimits = getYRange(plotData) %>% unlist(),
                                                 newLimits = getYRange(plotData[index, ]) %>% unlist(),
                                                 secAxis = TRUE)
                   rescalingSecAxis(rescaling)
                   
                   
                 }) %>%
                   bindEvent(input[["secAxisModel"]])
                 
                 observe({
                   req(savedModels(), input[["plotTimeModels"]])
                   
                   p <- extractedPlotDataDF() %>%
                     na.omit() %>%
                     rescaleSecondAxisData(individual = input[["secAxisModel"]],
                                           rescaling = rescalingSecAxis()) %>%
                     basePlotTime(xLim = getLim(plotRanges = plotRanges, axis = "xAxis"),
                                  yLim = getLim(plotRanges = plotRanges, axis = "yAxis")) %>%
                     setTitles(prop = input$modCredInt) %>%
                     shinyTools::formatTitlesOfGGplot(text = plotTexts) %>%
                     shinyTools::formatRangesOfGGplot(ranges = plotRanges) %>%
                     setXAxisLabels(xAxisData = extractedPlotDataList() %>%
                                      extractAllXAxisData(), # labels for all x axis data
                                    extendLabels = input$extendLabels, 
                                    xLim = getLim(plotRanges = plotRanges, axis = "xAxis"), 
                                    deriv = input$deriv,
                                    plotShifts = FALSE) %>%
                     drawLinesAndRibbon(
                       pointStyleList = pointStyleList,
                       alphaL = input[["alphaL"]],
                       alphaU = input[["alphaU"]]) %>%
                     setSecondYAxis(rescaling = rescalingSecAxis(),
                                    titleFormat = plotTexts[["yAxisTitle"]],
                                    textFormat = plotTexts[["yAxisText"]],
                                    yAxisLabel = input[["secAxisText"]],
                                    yAxisTitleColor = input[["secAxisColor"]])
                   
                   if (input[["hideLegend"]]) {
                     p <- p + theme(legend.position = "none")
                   }
                   
                   formattedPlot(p)
                 }) %>%
                   bindEvent(list(input[["applyFormatToTimePlot"]], 
                                  input[["applyFormatToTimePlotModel"]]))
                 
                 output$plotTime <- renderPlot({
                   validate(need(formattedPlot(), "Choose at least one element from 'Display Models / Individuals' and press 'Apply' ..."))
                   formattedPlot()
                 })
                 
                 plotExportServer("exportCredIntTimePlot",
                                  plotFun = reactive(function() formattedPlot()),
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
