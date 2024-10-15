#' Message for no models to plot
messageNoModelsToPlot <- function() {
  "Select 'Model(s) / Individual(s) to display' and press 'Draw Plot' first ..."
}

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
  
  defaultChoices <- ""
  names(defaultChoices) <- messageNoModelsToPlot()
  
  tagList(
    # Time Plot ----
    fluidRow(
      column(3, tags$h3("Time Plot")),
      column(6,
             selectizeInput(ns("plotTimeModels"), "Model(s) / Individual(s) to display",
                            choices = c("Fit or import a model ..." = ""),
                            multiple = TRUE,
                            selected = "",
                            width = "100%")),
      column(3,
             align = "right",
             style = "margin-top: 1.2em;",
             actionButton(ns("applyFormatToTimePlot"), "Draw Plot"),
             plotExportButton(ns("exportCredIntTimePlot")))
    ),
    tags$br(),
    plotOutput(ns("plotTime")) %>% withSpinner(color = "#20c997"),
    tags$br(),
    tabsetPanel(
      id = ns("plotDataTabs"),
      tabPanel(
        # Format Plot ----
        "Format Model / Individual",
        value = "formatPlotTab",
        tags$br(),
        fluidRow(
          column(3,
                 style = "margin-top: 1.2em;",
                 tags$h4("Format Time Plot")),
          column(6,
                 selectizeInput(ns("formatTimePlot"), "'Apply' formatting for Model / Individual:",
                                choices = defaultChoices,
                                width = "100%")),
          column(3,
                 align = "right",
                 style = "margin-top: 1.2em;",
                 actionButton(ns("applyFormatToTimePlotModel"), "Apply"),
                 actionButton(ns("resetFormatTimePlotModel"), "Reset Format"))
        ),
        tags$br(),
        fluidRow(
          column(3,
                 tags$h4("Data"),
                 radioButtons(ns("deriv"), 
                              "Type", 
                              choices = c("Absolute values" = "1", "First derivate" = "2")), 
                 sliderInput(ns("modCredInt"),
                             "Credibility interval:",
                             min = 0,
                             max = .99,
                             value = .8,
                             step = .05,
                             width = "100%"),
                 tags$br(),
                 sliderInput(ns("alphaU"),
                             "Transparency of uncertainty region",
                             min = 0,
                             max = 1,
                             value = 0.1,
                             step = 0.05),
                 sliderInput(ns("alphaL"), 
                             "Transparency of points / lines",
                             min = 0, 
                             max = 1, 
                             value = 0.9,
                             step = 0.05),
                 tags$br(),
                 plotLegendUI(ns("legend"))
          ),
          column(3,
                 shinyTools::plotTitlesUI(
                   id = ns("plotLabels"),
                   title = "Text",
                   type = "ggplot",
                   initText = list(plotTitle = config()[["defaultIntervalTimePlotTitle"]],
                                   xAxisText = config()[["defaultIntervalTimePlotText"]])
                 )
          ),
          column(3,
                 shinyTools::plotRangesUI(id = ns("plotRanges"), title = "Axis"), 
                 conditionalPanel(
                   ns = ns,
                   condition = "!input['plotRanges-fromData'] & input['plotRanges-labelName'] == 'xAxis'",
                   checkboxInput(inputId = ns("extendLabels"),
                                 label = "Extend x-axis labels to full range",
                                 value = FALSE)
                 ),
                 tags$br(), tags$br(),
                 selectizeInput(ns("secAxisModel"), "Add a new secondary y axis",
                                choices = c("Choose one Model / Individual ..." = "")),
                 helpText("The first element of 'Model(s) / Individual(s) to display' is always used for the first (left) axis.")
          ),
          column(3,
                 shinyTools::plotPointsUI(id = ns("pointStyle"),
                                          title = "Points / Lines",
                                          initStyle = config()[["defaultPointStyle"]])
          )
        )
      ),
      tabPanel(
        # Plot Data ----
        "Plot Data",
        value = "plotDataTab",
        tags$br(),
        tableOutput(ns("plotData")),
        fluidRow(column(12, align = "right", dataExportButton(ns("exportCredIntTimeData"))))
      ),
      tabPanel(
        # Break Point Detection ----
        "Break point detection",
        value = "breakPointTab",
        breakPointDetectionUI(ns("breakPointDetection"))
      )
    )
    ,
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
                   availableElements = c("title", "axis", "yaxis2", "legend"),
                   showParseButton = FALSE,
                   initText = getDefaultTextFormat()
                 )
                 plotRanges <- shinyTools::plotRangesServer(
                   "plotRanges",
                   type = "ggplot",
                   initRanges = list(xAxis = config()[["plotRange"]],
                                     yAxis = config()[["plotRange"]],
                                     yAxis2 = config()[["plotRange"]]),
                   axes = c("x axis" = "xAxis", "y axis" = "yAxis", "2nd y axis" = "yAxis2")
                 )
                 
                 pointStyle <- shinyTools::plotPointsServer(
                   "pointStyle", 
                   type = "ggplot",
                   initStyle = config()[["defaultPointStyle"]],
                   hideInput = c("hide", "alpha", "colorBg")
                   )
                 
                 legend <- shinyTools::plotLegendServer("legend")
                 
                 pointStyleList <- reactiveValues()
                 
                 observe({
                   req(length(savedModels()) > 0)
                   modelNames <- names(savedModels())
                   
                   pointStyleList <- pointStyleList %>%
                     getDefaultPointFormatForModels(modelNames = modelNames)
                   
                   updateSelectizeInput(session, "plotTimeModels", 
                                        choices = modelNames, 
                                        selected = modelNames[length(modelNames)])
                 }) %>%
                   bindEvent(savedModels())
                 
                 observe({
                   # choices for formatting of lines and points
                   if (is.null(input[["plotTimeModels"]])) {
                     newChoices <- ""
                     names(newChoices) <- messageNoModelsToPlot()
                   } else {
                     newChoices <- input[["plotTimeModels"]]
                   }
                   updateSelectizeInput(session, "formatTimePlot", choices = newChoices)
                   
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
                   bindEvent(input[["plotTimeModels"]], ignoreNULL = FALSE, ignoreInit = TRUE)
                 
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
                   }) %>%
                     removeEmptyModels()
                 })
                 
                 extractedPlotDataDF <- reactive({
                   extractedPlotDataList() %>%
                     extractPlotDataDF(models = input[["plotTimeModels"]],
                                       credInt = input$modCredInt)  %>%
                     shinyTryCatch(errorTitle = "'Credibility intervals over time': Error when extracting data", 
                                   warningTitle = "'Credibility intervals over time': Warning when extracting data",
                                   alertStyle = "shinyalert")
                 })
                 
                 # render plot data table ----
                 output$plotData <- renderTable({
                   validate(need(input[["plotTimeModels"]], messageNoModelsToPlot()),
                            need(nrow(extractedPlotDataDF()) > 0,
                                 "No data available for selected model(s) ..."))
                   extractedPlotDataDF()
                 })
                 
                 # export plot data ----
                 plotDataExport <- reactiveVal()
                 
                 observe({
                   plotDataExport(extractedPlotDataDF())
                 }) %>%
                   bindEvent(input[["plotTimeModels"]])
                 
                 dataExportServer("exportCredIntTimeData",
                                  reactive(function() {plotDataExport()}))
                 
                 newPlot <- reactive({
                   logDebug("%s: Entering: reactive 'newPlot'", id)
                   
                   # setup base plot
                   p <- extractedPlotDataDF() %>%
                     na.omit() %>%
                     rescaleSecondAxisData(individual = input[["secAxisModel"]],
                                           plotRanges = plotRanges) %>%
                     basePlotTime(xLim = getUserLimits(plotRanges = plotRanges[["xAxis"]]),
                                  yLim = getUserLimits(plotRanges = plotRanges[["yAxis"]])) %>%
                     setDefaultTitles(prop = input$modCredInt) %>%
                     shinyTools::formatTitlesOfGGplot(text = plotTexts)
                   
                   # specify x-axis labels from x data of all models
                   allXAxisData <- extractedPlotDataList() %>%
                     extractAllXAxisData() %>%
                     extendXAxis(xLabelLim = getUserLimits(plotRanges = plotRanges[["xAxis"]]), 
                                 extendLabels = input$extendLabels)
                   
                   p %>%
                     shinyTools::formatScalesOfGGplot(
                       ranges = plotRanges,
                       xLabels = list(
                         # input$deriv already included within extractedPlotDataList()
                         breaks = getBreaks(time = allXAxisData$time, deriv = "1"),
                         labels = getLabel(xAxisData = allXAxisData, deriv = "1")
                       ),
                       ySecAxisTitle = getSecAxisTitle(modelName = input[["secAxisModel"]],
                                                       customTitle = plotTexts[["yAxisTitle2"]])
                     ) %>%
                     drawLinesAndRibbon(
                       pointStyleList = pointStyleList,
                       alphaL = input[["alphaL"]],
                       alphaU = input[["alphaU"]],
                       # UPDATE LEGEND HERE <- -------
                       legendName = plotTexts[["legendTitle"]][["text"]]
                       ) %>%
                     shinyTools::formatLegendOfGGplot(legend = legend)
                 })
                 
                 observe({
                   req(savedModels(), input[["plotTimeModels"]])
                   logDebug("%s: Entering get formattedPlot()", id)
                   
                   p <- newPlot() %>%
                     shinyTools::shinyTryCatch(errorTitle = "Plotting failed")
                   formattedPlot(p)
                 }) %>%
                   bindEvent(list(input[["applyFormatToTimePlot"]], 
                                  input[["applyFormatToTimePlotModel"]]))
                 
                 observe({
                   req(savedModels(), input[["plotTimeModels"]])
                   logDebug("%s: Entering reset formattedPlot()", id)
                   
                   modelNames <- names(savedModels())
                   defaultStyle <- pointStyleList %>%
                     reactiveValuesToList() %>%
                     getDefaultPointFormatForModels(modelNames = modelNames)
                   pointStyleList[[input[["formatTimePlot"]]]] <- defaultStyle[[input[["formatTimePlot"]]]]
                   
                   p <- newPlot() %>%
                     shinyTools::shinyTryCatch(errorTitle = "Plotting failed")
                   formattedPlot(p)
                 }) %>%
                   bindEvent(input[["resetFormatTimePlotModel"]])
                 
                 # render plot ----
                 output$plotTime <- renderPlot({
                   validate(need(formattedPlot(), messageNoModelsToPlot()))
                   formattedPlot()
                 })
                 
                 # export plot ----
                 plotExportServer("exportCredIntTimePlot",
                                  plotFun = reactive(function() formattedPlot()),
                                  filename = sprintf("%s_Credibility_Intervals_Over_Time",
                                                     gsub("-", "", Sys.Date()))
                                  )
                 
                 # Break point detection ----
                 breakPointDetectionServer(id = "breakPointDetection", plotData = extractedPlotDataDF)
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
