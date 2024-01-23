#' ui function of modelSpecifications module
#'
#' @param id namespace
#' @param title title in tab
#'
#' @export
modelSpecificationsUI <- function(id, title) {
  ns <- NS(id)
  
  tagList(
    HTML(paste0("<h5>",title, "</h5>")),
    # TIME VARS
    pickerInput(
      inputId = ns("timeVars"),
      label = "Time variable(s):",
      choices = character(0),
      options = list(
        "actions-box" = FALSE,
        "none-selected-text" = 'No variables selected',
        "max-options" = 2
      ),
      multiple = TRUE
    ),
    helpText("Max. 2 time variables"),
    # BONE VARS
    pickerInput(
      inputId = ns("boneVars"),
      label = "Element variables:",
      choices = character(0),
      options = list(
        `actions-box` = TRUE,
        size = 10,
        `none-selected-text` = "No variables selected"
      ),
      multiple = TRUE
    ),
    tags$br(),
    checkboxInput(ns("rownamesAsIndVar"), label = "Use rownames as individual variable", value = FALSE),
    conditionalPanel(
      ns = ns,
      condition = "input.rownamesAsIndVar == false",
      selectizeInput(
        inputId = ns("indVar"),
        label = "Individual variable:",
        choices = character(0)
      ),
    ),
    tags$br(),
    sliderInput(inputId = ns("iter"),
                label = "Total iterations:",
                min = 500, max = 10000, step = 100, value = defaultModelSpecValues()$iter),
    sliderInput(inputId = ns("burnin"),
                label = "Burnin iterations:",
                min = 200, max = 3000, step = 100, value = defaultModelSpecValues()$burnin),
    sliderInput(inputId = ns("chains"),
                label = "MCMC chains:",
                min = 1, max = 12, step = 1, value = defaultModelSpecValues()$chains),
    checkboxInput(ns("rndmSeed"), label = "Random seed", value = defaultModelSpecValues()$rndmSeed),
    conditionalPanel(
      condition = "input.rndmSeed == false",
      ns = ns,
      numericInput(ns("fixedSeed"), label = "Fixed seed value", value = defaultModelSpecValues()$fixedSeed),
      tags$br()
    ),
    tags$br()
  )
}


#' server funtion of modelSpecifications module
#'
#' @param id An ID string that corresponds with the ID used to call the module's UI function.
#' @param dataMatrix (reactive) shinyMatrix matrixInput, see \code{\link{estimateIntervals}}: 
#' specifying the renewal rates of different probes for each time interval.
#' The renewalRates should be between 0 and 100 (percentages). The dataframe should include
#' a column specifying a time-index (e.g. 1, 2, 3, ...)
#' as well as columns for the different bones. The renewal rates should contain the times of origin,
#' containing 100.
#' @param uploadedModelSpecInputs (reactive) modelSpecifications for the current fit
#' 
#' @export
modelSpecificationsServer <- function(id, dataMatrix, uploadedModelSpecInputs = reactiveVal()){
  moduleServer(
    id,
    function(input, output, session) {
      values <- reactiveValues()
      
      observe({
        req(dataMatrix())
        if (length(uploadedModelSpecInputs()) == 0) {
          updatePickerInput(session, "timeVars", choices = dataMatrix() %>% colnames(),
                            selected = character(0))
          updatePickerInput(session, "boneVars", choices = dataMatrix() %>% colnames(),
                            selected = character(0))
          updateSelectizeInput(session, "indVar", choices = dataMatrix() %>% colnames())
          updateSliderInput(session, "iter", value = defaultModelSpecValues()$iter)
          updateSliderInput(session, "burnin", value = defaultModelSpecValues()$burnin)
          updateSliderInput(session, "chains", value = defaultModelSpecValues()$chains)
          updateCheckboxInput(session, "rndmSeed", value = defaultModelSpecValues()$rndmSeed)
          updateNumericInput(session, "fixedSeed", value = defaultModelSpecValues()$fixedSeed)
        }
        
        req(uploadedModelSpecInputs())
        updatePickerInput(session, "timeVars", choices = dataMatrix() %>% colnames(),
                          selected = uploadedModelSpecInputs()$timeVars)
        updatePickerInput(session, "boneVars", choices = dataMatrix() %>% colnames(),
                          selected = uploadedModelSpecInputs()$boneVars)
        updateSelectizeInput(session, "indVar", choices = dataMatrix() %>% colnames(),
                             selected = uploadedModelSpecInputs()$indVar)
        updateSliderInput(session, "iter", value = uploadedModelSpecInputs()$iter)
        updateSliderInput(session, "burnin", value = uploadedModelSpecInputs()$burnin)
        updateSliderInput(session, "chains", value = uploadedModelSpecInputs()$chains)
        updateCheckboxInput(session, "rndmSeed", value = uploadedModelSpecInputs()$rndmSeed)
        updateNumericInput(session, "fixedSeed", value = uploadedModelSpecInputs()$fixedSeed)
      })
      
      observeEvent(input$timeVars, {
        values$timeMinimum <- getTimeMin(mtrx = dataMatrix(),
                                         timeVars = input$timeVars)
        values$timeMaximum <- getTimeMax(mtrx = dataMatrix(),
                                         timeVars = input$timeVars)
        values$timeVars <- input$timeVars
      })
      
      observeEvent(input$boneVars, {
        values$boneVars <- input$boneVars
      })
      
      observeEvent(input$indVar, {
        values$indVar <- input$indVar
      })
      
      observeEvent(input$rownamesAsIndVar, {
        if (!input$rownamesAsIndVar) {
          values$indVar <- input$indVar
        } else {
          values$indVar <- character(0)
          updateSelectizeInput(session = session, "indVar", selected = character(0))
        }
      })
      
      observeEvent(input$iter, {
        values$iter <- input$iter
      })
      
      observeEvent(input$burnin, {
        values$burnin <- input$burnin
      })
      
      observeEvent(input$chains, {
        values$chains <- input$chains
      })
      
      observeEvent(input$rndmSeed, {
        values$rndmSeed <- input$rndmSeed
      })
      
      observeEvent(input$fixedSeed, {
        values$fixedSeed <- input$fixedSeed
      })
      
      reactive(values)
    })
  }

#' Get Time Minimum 
#' 
#' @mtrx (matrix) data matrix
#' @timeVars (character) column names of time variables
#' @default (numeric) default result
#' 
#' @return (numeric) minimal time
#' @export
getTimeMin <- function(mtrx, timeVars, default = 0) {
  if (length(timeVars) == 0 || any(sapply(timeVars, function(x) x == ""))) {
    return(default)
  }
  mtrx[, timeVars] %>% min()
}

#' Get Time Maximum 
#' 
#' @mtrx (matrix) data matrix
#' @timeVars (character) column names of time variables
#' @default (numeric) default result
#' 
#' @return (numeric) maximal time
#' @export
getTimeMax <- function(mtrx, timeVars, default = 1) {
  if (length(timeVars) == 0 || any(sapply(timeVars, function(x) x == ""))) {
    return(default)
  }
  mtrx[, timeVars] %>% max()
}

defaultModelSpecValues <- function() {
  list(iter = 2000,
       burnin = 500,
       chains = 4,
       rndmSeed = TRUE,
       fixedSeed = 12345)
}