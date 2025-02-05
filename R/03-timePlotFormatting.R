#' Message for no models to plot
messageNoModelsToPlot <- function() {
  "Select 'Model(s) / Individual(s) to display' and press 'Apply' first ..."
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
      column(2, tags$h3("Time Plot")),
      column(3,
             selectizeInput(ns("plotTimeModels"), "Model(s) / Individual(s) to display",
                            choices = c("Fit or import a model ..." = ""),
                            multiple = TRUE,
                            selected = "",
                            width = "100%")),
      column(3,
             sliderInput(ns("modCredInt"),
                         "Credibility interval:",
                         min = 0,
                         max = .99,
                         value = .8,
                         step = .05,
                         width = "100%")),
      column(2,
             radioButtons(ns("deriv"), 
                          "Type", 
                          choices = c("Absolute values" = "1", "First derivate" = "2"))),
      column(2,
             align = "right",
             style = "margin-top: 1.2em;",
             actionButton(ns("plotTimeModels-apply"), "Apply"),
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
          column(4,
                 tags$h4("Plot"),
                 tabsetPanel(
                   id = ns("tabs_text_legend"),
                   selected = "Data",
                   tabPanel("Data",
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
                                        step = 0.05)
                   ),
                   tabPanel("Axis",
                            shinyTools::plotRangesUI(id = ns("plotRanges"), title = NULL), 
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
                            helpText("The first element of 'Model(s) / Individual(s) to display' is always used for the first (left) axis."),
                            actionButton(ns("axis-apply"), "Apply")
                   ),
                   tabPanel("Text",
                            shinyTools::plotTitlesUI(
                              id = ns("plotLabels"),
                              title = NULL,
                              type = "ggplot",
                              initText = list(plotTitle = config()[["defaultIntervalTimePlotTitle"]],
                                              xAxisText = config()[["defaultIntervalTimePlotText"]])
                            ),
                            actionButton(ns("plotLabels-apply"), "Apply")
                   ),
                   tabPanel("Legend",
                            shinyTools::plotLegendUI(ns("legend"))
                   )
                 )
          ),
          column(4,
                 tags$h4("Model / Individual"),
                 tabsetPanel(
                   id = ns("tabs_models"),
                   selected = "Points & Lines",
                   tabPanel("Points & Lines",
                            selectizeInput(ns("formatTimePlot"), "Select model / individual:",
                                           choices = defaultChoices,
                                           width = "100%"),
                            shinyTools::plotPointsUI(id = ns("pointStyle"),
                                                     title = NULL,
                                                     initStyle = config()[["defaultPointStyle"]]),
                            actionButton(ns("pointStyle-apply"), "Apply"),
                            actionButton(ns("pointStyle-reset"), "Reset Models")
                   )
                 )
          ),
          # custom points ----
          column(4, shinyTools::customPointsUI(ns("customPoints")))
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
        changePointsUI(ns("changePoints"))
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
                 
                 plotTexts <- shinyTools::plotTitlesServer(
                   "plotLabels",
                   type = "ggplot", 
                   availableElements = c("title", "axis", "yaxis2"),
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
                 
                 pointStyleList <- reactiveVal(list()) # empty list, no models yet
                 plotAxisStyleList <- reactiveVal(list(xAxis = config()[["plotRange"]],
                                                       yAxis = config()[["plotRange"]],
                                                       yAxis2 = config()[["plotRange"]],
                                                       secAxisModel = "",
                                                       extendLabels = FALSE))
                 plotTextStyleList <- reactiveVal(getDefaultTextFormat())
                 
                 observe({
                   req(length(savedModels()) > 0)
                   modelNames <- names(savedModels())
                   
                   newList <- pointStyleList() %>%
                     getDefaultPointFormatForModels(modelNames = modelNames)
                   pointStyleList(newList)
                   
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
                 
                 extractedPlotDataDF <- reactiveVal()
                 observe({
                   req(savedModels(), input[["plotTimeModels"]])
                   logDebug("%s: Entering get extractedPlotDataDF()", id)
                   
                   df <- extractedPlotDataList() %>%
                     extractPlotDataDF(models = input[["plotTimeModels"]],
                                       credInt = input$modCredInt)  %>%
                     shinyTryCatch(errorTitle = "'Credibility intervals over time': Error when extracting data", 
                                   warningTitle = "'Credibility intervals over time': Warning when extracting data",
                                   alertStyle = "shinyalert")
                   
                   extractedPlotDataDF(df)
                 }) %>%
                   bindEvent(input[["plotTimeModels-apply"]])
                 
                 # render plot data table ----
                 output$plotData <- renderTable({
                   validate(need(input[["plotTimeModels"]], messageNoModelsToPlot()),
                            need(nrow(extractedPlotDataDF()) > 0,
                                 "No data available for selected model(s) ..."))
                   extractedPlotDataDF()
                 })
                 
                 # export plot data ----
                 dataExportServer("exportCredIntTimeData", filename = "timePlotData", reactive(function() {
                   if (length(input[["plotTimeModels"]]) == 0 ||
                       any(input[["plotTimeModels"]] == ""))
                     return(NULL)
                   
                   extractedPlotDataDF()
                 }))
                 
                 legend <- shinyTools::plotLegendServer(
                   "legend", 
                   legend_title = reactive({"individual"}),
                   legend_labels = reactive({levels(na.omit(extractedPlotDataDF())[["individual"]])}))
                 
                 # custom points ----
                 custom_points <- reactiveVal(list())
                 customPointsServer("customPoints", plot_type = "ggplot", custom_points = custom_points)
                 
                 # buttons: input[["plotTimeModels"]] button -----
                 # disable if nothing is selected in
                 buttons <- c("plotTimeModels-apply", "axis-apply", "plotLabels-apply")
                 lapply(buttons, function(btn) {
                   observe({
                     if (length(input[["plotTimeModels"]]) == 0 ||
                         any(input[["plotTimeModels"]] == "")) {
                       logDebug("%s: Disable button '%s'", id, btn)
                       shinyjs::disable(ns(btn), asis = TRUE)
                     } else {
                       logDebug("%s: Enable button '%s'", id, btn)
                       shinyjs::enable(ns(btn), asis = TRUE)
                     }
                   })
                 })
                 
                 observe({
                   logDebug("%s: Apply new point layout", id)
                   newList <- plotAxisStyleList()
                   for (name in names(plotRanges)) {
                     newList[[name]] <- plotRanges[[name]]
                   }
                   newList[["extendLabels"]] <- input[["extendLabels"]]
                   newList[["secAxisModel"]] <- input[["secAxisModel"]]
                   
                   plotAxisStyleList(newList)
                 }) %>%
                   bindEvent(input[["axis-apply"]])
                 
                 observe({
                   logDebug("%s: Apply new text layout", id)
                   newList <- plotTextStyleList()
                   for (name in names(plotTexts)) {
                     newList[[name]] <- plotTexts[[name]]
                   }
                   
                   plotTextStyleList(newList)
                 }) %>%
                   bindEvent(input[["plotLabels-apply"]])
                 
                 # button: input[["formatTimePlot"]] ----
                 # disable if nothing is selected in
                 observe({
                   if (length(input[["formatTimePlot"]]) == 0 ||
                       any(input[["formatTimePlot"]] == "")) {
                     logDebug("%s: Disable button 'pointStyle-apply'", id)
                     shinyjs::disable(ns("pointStyle-apply"), asis = TRUE)
                   } else {
                     logDebug("%s: Enable button 'pointStyle-apply'", id)
                     shinyjs::enable(ns("pointStyle-apply"), asis = TRUE)
                   }
                 })
                 
                 observe({
                   logDebug("%s: Apply new point layout", id)
                   
                   # update point style list
                   newList <- pointStyleList()
                   newList[[input[["formatTimePlot"]]]] <- pointStyle[["dataPoints"]]
                   pointStyleList(newList)
                 }) %>%
                   bindEvent(input[["pointStyle-apply"]])
                 
                 observe({
                   req(savedModels(), input[["plotTimeModels"]])
                   logDebug("%s: Entering reset pointStyleList()", id)
                   defaultStyle <- pointStyleList() %>%
                     getDefaultPointFormatForModels(modelNames = names(savedModels()), isFullReset = TRUE)
                   newList <- pointStyleList()
                   newList[[input[["formatTimePlot"]]]] <- defaultStyle[[input[["formatTimePlot"]]]]
                   pointStyleList(newList)
                 }) %>%
                   bindEvent(input[["pointStyle-reset"]])
                 
                 # render plot ----
                 plotToExport <- reactiveVal(NULL)
                 output$plotTime <- renderPlot({
                   validate(need(extractedPlotDataDF(), messageNoModelsToPlot()))
                   
                   logDebug("%s: draw basePlot", id)
                   p <- extractedPlotDataDF() %>%
                     na.omit() %>%
                     rescaleSecondAxisData(individual = plotAxisStyleList()[["secAxisModel"]],
                                           plotRanges = plotAxisStyleList()) %>%
                     basePlotTime(xLim = getUserLimits(plotRanges = plotAxisStyleList()[["xAxis"]]),
                                  yLim = getUserLimits(plotRanges = plotAxisStyleList()[["yAxis"]])) %>%
                     setDefaultTitles(prop = isolate(input[["modCredInt"]])) %>%
                     shinyTools::formatTitlesOfGGplot(text = plotTextStyleList()) %>%
                     shinyTools::shinyTryCatch(errorTitle = "Plotting failed")
                   
                   # specify x-axis labels from x data of all models
                   allXAxisData <- extractedPlotDataList() %>%
                     extractAllXAxisData() %>%
                     extendXAxis(xLabelLim = getUserLimits(plotRanges = plotAxisStyleList()[["xAxis"]]), 
                                 extendLabels = plotAxisStyleList()[["extendLabels"]])
                   
                   logDebug("%s: draw lines", id)
                   p <- p %>%
                     drawLinesAndRibbon(
                       pointStyleList = pointStyleList(),
                       alphaL = input[["alphaL"]],
                       alphaU = input[["alphaU"]],
                       legend = reactiveValuesToList(legend)
                     ) %>%
                     shinyTools::formatScalesOfGGplot(
                       ranges = plotAxisStyleList(),
                       xLabels = list(
                         # input$deriv already included within extractedPlotDataList()
                         breaks = getBreaks(time = allXAxisData$time, deriv = "1"),
                         labels = getLabel(xAxisData = allXAxisData, deriv = "1")
                       ),
                       ySecAxisTitle = getSecAxisTitle(modelName = plotAxisStyleList()[["secAxisModel"]],
                                                       customTitle = plotTextStyleList()[["yAxisTitle2"]])
                     ) %>%
                     shinyTools::addCustomPointsToGGplot(custom_points = custom_points()) %>%
                     shinyTools::shinyTryCatch(errorTitle = "Plotting failed")
                   
                   plotToExport(p)
                   p
                 })
                 
                 # export plot ----
                 plotExportServer("exportCredIntTimePlot",
                                  plotFun = reactive(function() plotToExport()),
                                  filename = sprintf("%s_Credibility_Intervals_Over_Time",
                                                     gsub("-", "", Sys.Date()))
                                  )
                 
                 # Break point detection ----
                 changePointData <- reactiveValues()
                 observe({
                   changePointData$mainData <- extractedPlotDataDF()
                 }) %>%
                   bindEvent(extractedPlotDataDF())
                 
                 changePointsServer(
                   "changePoints",
                   file_data = changePointData, 
                   mcp_columns = c(x = "time", y = "median")
                 )
                 # note: restoring a whole session as in ChangeR will not be possible (much too
                 # complex) since inputs are send much earlier than all reactive objects are updated.
                 # As a result the inputs cannot be set correctly and plots will remain empty.
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
