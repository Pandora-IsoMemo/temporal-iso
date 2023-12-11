library(shiny)
library(shinyWidgets)
library(shinythemes)
library(colourpicker)
library(OsteoBioR)
library(shinyMatrix)
library(dplyr)
library(shinycssloaders)
library(ggplot2)
library(rstan)

tagList(
  shinyjs::useShinyjs(),
  shiny::navbarPage(
    header = includeCSS("www/custom.css"),
    title = paste("OsteoBioR App", packageVersion("OsteoBioR")),
    theme = shinythemes::shinytheme("flatly"),
    id = "tab",
    position = "fixed-top",
    collapsible = TRUE,
    # DATA  ------------------------------------------------------------------------------------------------
    tabPanel(
      title = "Data",
      id = "Data",
      value = "Data",
      sidebarLayout(
        sidebarPanel(
          ## left sidebar ----
          width = 2,
          style = "position:fixed; width:15%; max-width:350px; overflow-y:auto; height:85%",
          HTML("<h5>Upload of datasets</h5>"),
          DataTools::importDataUI("fileData", "Renewal rates"),
          tags$br(), tags$br(),
          checkboxInput("renewUnc", "Use renewal rates uncertainty (optional)"),
          conditionalPanel(
            condition = "input.renewUnc == true",
            DataTools::importDataUI("fileDataSD", HTML("Renewal rates uncertainty")),
          ),
          tags$br(), tags$br(),
          DataTools::importDataUI("fileIso", "Measurements"),
          tags$br(), tags$br(),
          HTML("<h5>Generate Data</h5>"),
          actionButton("exampleData", "Load Example Data")
        ),
        mainPanel(
          ## main panel ----
          HTML("<h5>Renewal rates dataset </h5>"),
          matrixInput(
            inputId = "dataMatrix",
            #inputClass = "matrix-input-rownames",
            class = "numeric",
            value = matrix(ncol = 6, dimnames = list(
              c(""),  c("individual", "intStart", "intEnd", "bone1", "bone2", "tooth1")
            )),
            #copy = TRUE,
            #paste = TRUE,
            cols = list(
              names = TRUE,
              extend = TRUE,
              delta = 1,
              editableNames = TRUE
            ),
            rows = list(
              names = FALSE,
              editableNames = TRUE,
              extend = TRUE,
              delta = 1
            )
          ),
          conditionalPanel(
            condition = "input.renewUnc == true",
            HTML("<h5>Renewal rates uncertainty dataset (standard deviation, optional) </h5>"),
            matrixInput(
              inputId = "dataMatrixSD",
              #inputClass = "matrix-input-rownames",
              class = "numeric",
              value = matrix(0, ncol = 6, dimnames = list(
                c(""),  c("individual", "intStart", "intEnd", "bone1", "bone2", "tooth1")
              )),
              #copy = TRUE,
              #paste = TRUE,
              cols = list(
                names = TRUE,
                extend = FALSE,
                delta = 1,
                editableNames = FALSE
              ),
              rows = list(
                names = FALSE,
                editableNames = FALSE,
                extend = FALSE,
                delta = 1
              )
            ),
          ),
          # To do: Add  time cuts: Split predictions into groups at the following points in time
          # for the selected individual
          HTML("<h5>Mean and (optional) standard deviation of measurements</h5>"),
          matrixInput(
            inputId = "isotope",
            #inputClass = "matrix-input-rownames",
            class = "numeric",
            value = matrix(ncol = 3, dimnames = list(c(""),  c("individual", "y_mean", "y_sigma"))),
            #copy = TRUE,
            #paste = TRUE,
            cols = list(
              names = TRUE,
              extend = FALSE,
              editableNames = FALSE,
              delta = 0
            ),
            rows = list(
              names = FALSE,
              editableNames = FALSE,
              extend = TRUE,
              delta = 1
            )
          )
        )
      )
    ),
    # MODEL ----------------------------------------------------------------------------------------------
    tabPanel("Model",
             id = "Model",
             fluidRow(
               ## left sidebar ----
               sidebarPanel(
                 width = 2,
                 style = "position:fixed; width:15%; max-width:350px; overflow-y:auto; height:85%",
                 DataTools::importDataUI("modelUpload", label = "Import Model"),
                 tags$hr(),
                 modelSpecificationsUI("modelSpecification", "Model Specification"),
                 checkboxInput("rndmSeed", label = "Random seed", value = TRUE),
                 conditionalPanel(
                   condition = "input.rndmSeed == false",
                   numericInput("fixedSeed", label = "Fixed seed value", value = 12345),
                   tags$br()
                   ),
                 actionButton("fitModel", "Fit Model"),
                 tags$hr(),
                 selectInput("selectedModels", label = "Download model object(s)",
                             choices = c("Save or upload models ..." = ""),
                             multiple = T),
                 DataTools::downloadModelUI("modelDownload", label = "Download")
               ),
               ## main panel ----
               mainPanel(
                 tabsetPanel(
                   id = "modTabs",
                   header = tagList(
                     tags$br(),
                     tags$br(),
                     fluidRow(
                       column(width = 3,
                              style = "margin-top: 0.7em;",
                              htmlOutput("fittingTimeTxt")),
                       column(width = 3,
                              style = "margin-top: -0.3em;",
                              selectInput("savedModels", label = "Load Model", choices = NULL)),
                       column(width = 1, 
                              style = "margin-top: 1em;",
                              actionButton("loadModel", "Load", width = "100%")),
                       column(width = 3, 
                              offset = 1,
                              style = "margin-top: -0.7em;",
                              textInput("modelName", label = "Save Model", placeholder = "model name")),
                       column(width = 1, 
                              style = "margin-top: 1em;",
                              actionButton("saveModel", "Save", width = "100%"))
                       ),
                     tags$hr()
                   ),
                   tabPanel(
                     "Summary",
                     value = "summaryTab",
                     verbatimTextOutput("summary") %>% withSpinner(color =
                                                                     "#20c997"),
                     actionButton("exportSummary", "Export Interval Data")
                   ),
                   tabPanel(
                     "Credibility Intervals",
                     value = "credibilityIntervalsTab",
                     plotOutput("plot") %>% withSpinner(color =
                                                          "#20c997"),
                     actionButton("exportCredIntPlot", "Export Plot"),
                     actionButton("exportCredIntDat", "Export Data")
                   ),
                   tabPanel(
                     "Credibility intervals over time",
                     value = "credibilityIntervalsOverTimeTab",
                     HTML("<br>"),
                     plotOutput("plotTime") %>% withSpinner(color = "#20c997"),
                     tags$br(),
                     tags$br(),
                     fluidRow(
                       column(2,
                              numericInput("xmin", "Lower x limit", 
                                           value = defaultInputsForUI()$xmin),
                              numericInput("xmax", "Upper x limit", 
                                           value = defaultInputsForUI()$xmax),
                              textInput("xAxisLabel", label = "X-Axis title", value = "Time"),
                              numericInput(inputId = "sizeTextX", label = "Font size x-axis title", value = 24),
                              numericInput(inputId = "sizeAxisX", label = "Font size x-axis", value = 18),
                              checkboxInput(inputId = "extendLabels",
                                            label = "Extend x-axis labels to lower and upper limits",
                                            value = FALSE)
                              ),
                       column(2,
                              numericInput("ymin", "Lower y limit",
                                           value = defaultInputsForUI()$ymin),
                              numericInput("ymax", "Upper y limit",
                                           value = defaultInputsForUI()$ymax),
                              textInput("yAxisLabel", label = "Y-Axis title", value = "Estimate"),
                              numericInput(inputId = "sizeTextY", label = "Font size y-axis title", value = 24),
                              numericInput(inputId = "sizeAxisY", label = "Font size y-axis", value = 18)
                              ),
                       column(4,
                              colourInput(inputId = "colorL",
                                          label = "Color line",
                                          value = rgb(0, 35 / 255, 80 / 255, alpha = 0.6)),
                              sliderInput("alphaL", "Transparency lines", min = 0, max = 1, value = 0.9),
                              tags$br(),
                              tags$br(),
                              colourInput(inputId = "colorU",
                                          label = "Color uncertainty region",
                                          value = rgb(0, 35 / 255, 80 / 255, alpha = 0.6)),
                              sliderInput("alphaU", "Transparency uncertainty region", min = 0, max = 1, value = 0.1)
                              ),
                       column(4,
                              checkboxInput("secAxis", "Add new secondary axis to existing plot", value = F),
                              radioButtons("deriv", "Type", choices = c("Absolute values" = "1", "First derivate" = "2")), 
                              sliderInput("modCredInt",
                                          "Credibility interval:",
                                          min = 0,
                                          max = .99,
                                          value = .8,
                                          step = .05),
                              tags$br(),
                              tags$br(),
                              selectizeInput("credIntTimePlot", "Select Models / Individuals", choices = NULL),
                              actionButton("newPlot", "New Plot"),
                              actionButton("addPlot", "Add Plot"),
                              actionButton("exportCredIntTimePlot", "Export Plot")
                       )
                     )
                   ),
                   tabPanel(
                     "Shift detection",
                     value = "shiftTime",
                     HTML("<br>"),
                     fluidRow(
                       column(4,
                              pickerInput("savedModelsShift",
                                          "Select Models / Individuals",
                                          choices = NULL, multiple = TRUE,
                                          options = list(`actions-box` = TRUE))
                              ),
                       column(3,
                              radioButtons(
                                "shiftTimeAbsOrRel",
                                "Shift detection:",
                                choices = c("absolute", "relative"),
                                selected = "absolute"),
                              radioButtons(
                                "slope",
                                "Shift detection type:",
                                choices = c("difference", "slope"),
                                selected = "difference")
                              ),
                       column(3,
                              numericInput("shiftTimeThreshold", 
                                           "Shift time threshold:", value = 0),
                              sliderInput("shiftTimeProb",
                                          "Shift time probability:",
                                          min = 0.5, max = 0.999, 
                                          value = 0.5, step = 0.001)
                              )
                     ),
                     tags$br(),
                     verbatimTextOutput("shiftTimePoints") %>% withSpinner(color = "#20c997")
                   ),
                   tabPanel(
                     "Time point estimates",
                     value = "timePointEstimatesTab",
                     tags$br(),
                     fluidRow(
                       column(4,
                              pickerInput("savedModelsTime", "Select Models / Individuals", choices = NULL, multiple = TRUE,
                                          options = list(`actions-box` = TRUE)),
                              tags$br(),
                              actionButton("estSpecTimePoint", "Estimate")
                       ),
                       column(4,
                              numericInput("from", "From", defaultInputsForUI()$from),
                              numericInput("to", "To", defaultInputsForUI()$to),
                              numericInput("by", "By", defaultInputsForUI()$by)
                              ),
                       column(4,
                              tags$br(),
                              actionButton("exportTimePointEst", "Export Time Point Estimates")
                              )
                     ),
                     tags$br(),
                     verbatimTextOutput("timePointEstimates")
                   ),
                   tabPanel(
                     "Estimates for user defined interval",
                     value = "userIntervalTab",
                     HTML("<br>"),
                     fluidRow(
                       column(4,
                              pickerInput("savedModelsUserDefined", "Select Models / Individuals", choices = NULL, multiple = TRUE,
                                          options = list(`actions-box` = TRUE))
                              ),
                       column(4,
                              radioButtons("typeEstUser", "Type", 
                                           choices = c("Absolute Mean + SD" = "1", "Total turnover Mean + SD" = "2"))
                              ),
                       column(4,
                              numericInput("from2", "From", value = defaultInputsForUI()$from2),
                              numericInput("to2", "To", value = defaultInputsForUI()$to2)
                              )
                       
                     ),
                     tags$br(),
                     verbatimTextOutput("userDefined") %>% withSpinner(color ="#20c997")
                   )
                 )
               )
             )
             ),
    # RESIDING TIME ------------------------------------------------------------------------------------------------------
    tabPanel("Residence time",
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 style = "position:fixed; width:15%; max-width:350px; overflow-y:auto; height:85%",
                 HTML("<h5>Upload</h5>"),
                 DataTools::importDataUI("stayTimeData", "Import Data"),
                 tags$br(), tags$br(),
                 HTML("<h5>Generate Data</h5>"),
                 actionButton("loadStayTimeData", "Load Example Data"),
                 HTML("<hr>"),
                 HTML("<h5>Estimation</h5>"),
                 actionButton("stayingTime", "Estimate Residence Time")
               ),
               mainPanel(
                 HTML("<h5>Data</h5>"),
                 helpText("Two columns with mean and standard deviation; each row stands for unique site."),
                 matrixInput(
                   inputId = "stayTimeMatrix",
                   inputClass = "matrix-input-rownames",
                   class = "numeric",
                   value = matrix(ncol = 2, dimnames = list(c(""),  c(
                     "siteMeans", "siteSigma"
                   ))),
                   #copy = TRUE,
                   #paste = TRUE,
                   cols = list(
                     names = TRUE,
                     extend = TRUE,
                     delta = 1,
                     editableNames = TRUE
                   ),
                   rows = list(
                     names = FALSE,
                     editableNames = TRUE,
                     extend = TRUE
                   )
                 ),
                 conditionalPanel(condition = "input.stayingTime",
                                  HTML("<h3>Results</h3>")),
                 verbatimTextOutput("estimatedStayTimes"),
                 tags$br(),
                 actionButton("exportStayTimeDat", "Export estimated residence time lengths")
               )
             )
             ),
    # ISOTOPIC VALUES ---------------------------------------------------------------------------------------------
    tabPanel("Measurement simulation",
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 style = "position:fixed; width:15%; max-width:350px; overflow-y:auto; height:85%",
                 HTML("<h5>Upload</h5>"),
                 DataTools::importDataUI("fileHistData", "Import Data"),
                 tags$br(), tags$br(),
                 HTML("<h5>Generate Data</h5>"),
                 # EXAMPLE DATA
                 actionButton("loadHistData", "Load Example Data"),
                 
                 HTML("<hr>"),
                 # SPECIFICATION
                 HTML("<h5>Specification</h5>"),
                 
                 pickerInput(
                   inputId = "timeVarHist",
                   label = "Time Variable:",
                   choices = character(0),
                   options = list(
                     "actions-box" = FALSE,
                     "none-selected-text" = 'No variables selected',
                     "max-options" = 1
                   ),
                   multiple = TRUE
                 ),
                 HTML("<br>"),
                 pickerInput(
                   inputId = "boneVarsHist",
                   label = "Variables for elements:",
                   choices = character(0),
                   options = list(
                     `actions-box` = FALSE,
                     size = 10,
                     `none-selected-text` = "No variables selected"
                   ),
                   multiple = TRUE
                 ),
                 HTML("<br>"),
                 pickerInput(
                   inputId = "meanVarHist",
                   label = "Measurement mean:",
                   choices = character(0),
                   options = list(
                     "actions-box" = FALSE,
                     "none-selected-text" = 'No variables selected',
                     "max-options" = 1
                   ),
                   multiple = TRUE
                 ),
                 HTML("<br>"),
                 pickerInput(
                   inputId = "sdVarHist",
                   label = "Measurement standard deviation:",
                   choices = character(0),
                   options = list(
                     "actions-box" = FALSE,
                     "none-selected-text" = 'No variables selected',
                     "max-options" = 1
                   ),
                   multiple = TRUE
                 ),
                 HTML("<br>"),
                 
                 actionButton("calcIsotopicValues", "Calculate Values")
               ),
               mainPanel(
                 HTML("<h5>Historic data</h5>"),
                 matrixInput(
                   inputId = "historicData",
                   inputClass = "matrix-input-rownames",
                   class = "numeric",
                   value = matrix(ncol = 5, dimnames = list(
                     c(""),  c("t", "bone1", "bone2", "mean", "sd")
                   )),
                   #copy = TRUE,
                   #paste = TRUE,
                   cols = list(
                     names = TRUE,
                     extend = TRUE,
                     delta = 1,
                     editableNames = TRUE
                   ),
                   rows = list(
                     names = FALSE,
                     editableNames = TRUE,
                     extend = TRUE
                   )
                 ),
                 conditionalPanel(condition = "input.calcIsotopicValues",
                                  HTML("<h3>Results</h3>")),
                 tableOutput("isotopicValues"),
                 verbatimTextOutput("quant"),
                 tags$br(),
                 actionButton("exportResultsDat", "Export Isotopic Values")
               )
             ))
    # STYLE of navbarPage ----
  ),
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  # ),
  div(
    id = "header-right",
    # div(
    #   id = "logo-mpi",
    #   tags$a(
    #     href = "https://www.mpg.de/en",
    #     img(src = "MPIlogo.png", alt = "Supported by the Max Planck society"),
    #     target = "_blank"
    #   )
    # ),
    # div(
    #   id = "logo-isomemo",
    #   tags$a(
    #     href = "https://isomemo.com/",
    #     img(src = "IsoMemoLogo.png", alt = "IsoMemo"),
    #     target = "_blank"
    #   )
    # ),
    div(
      id = "further-help",
      tags$button(onclick = "window.open('https://isomemo.com','_blank');",
                  class = "btn btn-default",
                  "Further Help")
    ),
    div(id = "help",
        actionButton("getHelp", "?"))
  )
)
