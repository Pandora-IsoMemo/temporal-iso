#' Break Point Detection UI
#'
#' UI of the module
#'
#' @rdname breakPointDetectionServer
breakPointDetectionUI <- function(id) {
  ns <- NS(id)
  tagList(tags$br(),
          tabsetPanel(
            id = ns("breakPointTabs"),
            tabPanel("1. MCP Lists from Segments & Priors", mcpFormulasUI(ns("formulas"))),
            tabPanel(
              "2. MCP Modeling",
              mcpModelingUI(ns("mcp")),
              mcpShowSingleModelUI(ns("singleModelOut"))
            ),
            tabPanel("3. Comparison of Models", mcpCompareModelsUI(ns(
              "compareModelsOut"
            )))
          ),
          tags$br())
}

#' Break Point Detection Server
#'
#' Server function of the module
#'
#' @param id The module id
#' @param plotData The reactive plot data
breakPointDetectionServer <- function(id, plotData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    mcpData <- reactiveVal()
    
    observe({
      req(nrow(plotData()) > 0)
      # select relevant columns
      newData <- plotData()[, c("time", "median")]
      # rename columns
      colnames(newData) <- c("x", "y")
      # remove rows with NA
      newData <- na.omit(newData)
      
      mcpData(newData)
    }) %>% bindEvent(plotData())
    
    # example data instead of plot data
    # observe({
    #   mcpData(read.csv(file.path("data", "example_breakPoints.csv")))
    # }) %>%
    #   bindEvent(input[["mcp-loadExampleDf"]])
    
    formulasAndPriors <- mcpFormulasServer(id = "formulas")
    
    mcpFitList <- mcpModelingServer(id = "mcp",
                                    mcpData = mcpData,
                                    formulasAndPriors = formulasAndPriors)
    
    mcpShowSingleModelServer(
      id = "singleModelOut",
      mcpData = mcpData,
      formulasAndPriors = formulasAndPriors,
      mcpFitList = mcpFitList
    )
    
    mcpCompareModelsServer(
      id = "compareModelsOut",
      mcpData = mcpData,
      formulasAndPriors = formulasAndPriors,
      mcpFitList = mcpFitList
    )
  })
}


# 1. MCP Segments & Priors ----

#' MCP Formulas UI
#'
#' @rdname mcpFormulasServer
mcpFormulasUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$br(),
    fluidRow(column(8, tags$h4("Segments")), column(
      4, align = "right", infoButtonUI(ns("formulaInfo"), label = "Segment Description")
    )),
    matrixUI(
      ns("segments"),
      title = NULL,
      defaultCellContent = "y ~ 1 + x",
      exampleLabel = "Example Segments"
    ),
    tags$br(),
    fluidRow(column(8, tags$h4("Priors")), column(
      4, align = "right", infoButtonUI(ns("PriorInfo"), label = "Prior Description")
    )),
    matrixUI(
      ns("priors"),
      title = NULL,
      defaultCellContent = "x_1 = dunif(-4, -0.5);",
      exampleLabel = "Example Priors"
    ),
    tags$br(),
    fluidRow(
      column(10, verbatimTextOutput(ns("mcpFormulas"))),
      column(
        2,
        align = "right",
        actionButton(ns("apply"), "Create MCP Lists", disabled = TRUE)
      )
    )
  )
}

#' MCP Formulas Server
#'
#' @param id The module id
mcpFormulasServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    formulasAndPriors <- reactiveVal()
    
    segmentsMatrix <- matrixServer(
      "segments",
      exampleFunction = readExampleMatrix,
      validateCellFunction = validateFormula,
      path = file.path("data", "example_breakPointSegments.csv")
    )
    priorsMatrix <- matrixServer(
      "priors",
      exampleFunction = readExampleMatrix,
      path = file.path("data", "example_breakPointPriors.csv")
    )
    
    infoButtonServer(
      id = "formulaInfo",
      title = "'Formula' (segment) description",
      text = "The 'formula' argument in mcp defines the model structure. It specifies the relationship between
        variables and the change points in your data. The formula should be written similarly to formulas
        in base R, with additional support for change points.",
      link = "https://lindeloev.github.io/mcp/articles/formulas.html"
    )
    
    infoButtonServer(
      id = "PriorInfo",
      title = "'Prior' description",
      text = "The 'prior' argument in mcp specifies the prior distributions for the parameters in your model.
        It should be a named list where the names correspond to the model parameters,
        and the values are prior distributions in JAGS notation. If you don't specify a prior for a parameter,
        mcp will use a default weakly informative prior.",
      link = "https://lindeloev.github.io/mcp/articles/priors.html"
    )
    
    observe({
      req(segmentsMatrix(), priorsMatrix())
      
      # enable the 'Create MCP Formulas' button
      shinyjs::enable(ns("apply"), asis = TRUE)
      #updateActionButton(session, "apply", disabled = FALSE) # not working with current version in docker
    }) %>% bindEvent(list(segmentsMatrix(), priorsMatrix()))
    
    observe({
      newFormulasAndPriors <- getComb(segments = segmentsMatrix(), priors = priorsMatrix()) %>%
        cleanComb() %>%
        splitComb() %>%
        setFormulasAndPriors() %>%
        shinyTryCatch(errorTitle = "Error in creating MCP lists", warningTitle = "Warning in creating MCP lists")
      
      formulasAndPriors(newFormulasAndPriors)
    }) %>%
      bindEvent(input[["apply"]])
    
    output$mcpFormulas <- renderPrint({
      validate(need(
        formulasAndPriors(),
        "Please 'Set' Segments and Priors first ..."
      ))
      formulasAndPriors()
    })
    
    return(formulasAndPriors)
  })
}

#' Info Button UI
#'
#' @param label The label of the button
#' @rdname infoButtonServer
infoButtonUI <- function(id, label = "Description") {
  ns <- NS(id)
  tagList(actionButton(ns("show_info"), icon = icon("info-circle"), label))
}

#' Info Button Server
#'
#' @param id The module id
#' @param title The title of the modal
#' @param text The text to display in the modal
#' @param link The link to the documentation
infoButtonServer <- function(id,
                             title = "Description",
                             text,
                             link = NULL) {
  moduleServer(id, function(input, output, session) {
    observe({
      showModal(
        modalDialog(
          title = title,
          p(text),
          # Add a hyperlink to the github help page
          if (!is.null(link)) {
            p(
              "For more details, visit the",
              a("documentation", href = link, target = "_blank"),
              "."
            )
          } else
            NULL,
          footer = modalButton("Close"),
          easyClose = TRUE
        )
      )
    }) %>%
      bindEvent(input$show_info)
  })
}


# 2. MCP Modeling ----

#' MCP Modeling UI
#'
#' @rdname mcpModelingServer
mcpModelingUI <- function(id) {
  ns <- NS(id)
  tagList(tags$br(),
          fluidRow(
            column(
              3,
              numericInput(ns("adapt"), "Burn in length", value = 5000),
              helpText("Increase for better conversion (makes the run slower).")
            ),
            column(3, numericInput(
              ns("chains"),
              "Number of chains",
              value = 3,
              min = 1
            )),
            column(
              3,
              numericInput(
                ns("iter"),
                "Number of iterations",
                value = 3000,
                min = 1
              )
            ),
            column(
              3,
              align = "right",
              style = "margin-top: 1.75em;",
              # load example data instead of plot data:
              #actionButton(ns("loadExampleDf"), "Load Example Data"),
              actionButton(ns("apply"), "Run MCP", disabled = TRUE)
            )
          ),
          tags$hr())
}

#' MCP Modeling Server
#'
#' @inheritParams mcpOutServer
mcpModelingServer <- function(id, formulasAndPriors, mcpData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    mcpFitList <- reactiveVal()
    
    observe({
      req(formulasAndPriors(), mcpData())
      # enable the 'Run Model' button
      shinyjs::enable(ns("apply"), asis = TRUE) # use this instead of updateActionButton
      #updateActionButton(session, "apply", disabled = FALSE) # not working with current version in docker
    }) %>% bindEvent(formulasAndPriors(), mcpData())
    
    observe({
      res <- runMcp(
        lists = formulasAndPriors(),
        data = mcpData(),
        adapt = input[["adapt"]],
        chains = input[["chains"]],
        iter = input[["iter"]]
      ) %>%
        shinyTryCatch(errorTitle = "Error in fitting mcp model", warningTitle = "Warning in fitting mcp model") %>%
        withProgress(message = "Fitting MCP model...", value = 0.5)
      
      mcpFitList(res)
    }) %>%
      bindEvent(input[["apply"]])
    
    return(mcpFitList)
  })
}

#' MCP Show Single Model UI
#'
#' @rdname mcpShowSingleModelServer
mcpShowSingleModelUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("showModel"),
      "Show MCP model",
      choices = c("'Run MCP' first ..." = "")
    ),
    tags$br(),
    fluidRow(column(
      6,
      mcpOutUI(
        id = ns("summary"),
        title = "Model Summary",
        outFUN = verbatimTextOutput,
        showWidth = TRUE
      )
    ), column(
      6,
      mcpOutUI(
        id = ns("waic"),
        title = "Model WAIC",
        outFUN = verbatimTextOutput
      )
    )),
    mcpOutUI(
      id = ns("plot"),
      title = "Model Plot",
      outFUN = plotOutput
    )
  )
}

#' MCP Show Single Model Server
#'
#' @inheritParams mcpOutServer
mcpShowSingleModelServer <- function(id,
                                     mcpData,
                                     formulasAndPriors,
                                     mcpFitList) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    mcpModel <- reactiveVal()
    
    observe({
      mcpModels <- seq_along(mcpFitList())
      names(mcpModels) <- paste("Model", mcpModels)
      if (length(mcpModels) > 0)
        mcpSelected <- 1
      else
        mcpSelected <- NULL
      updateSelectInput(session,
                        "showModel",
                        choices = mcpModels,
                        selected = mcpSelected)
    }) %>%
      bindEvent(mcpFitList())
    
    observe({
      req(mcpFitList(), input[["showModel"]])
      res <- mcpFitList()[[as.numeric(input[["showModel"]])]]
      mcpModel(res)
    }) %>% bindEvent(input[["showModel"]])
    
    mcpOutServer(
      id = "summary",
      formulasAndPriors = formulasAndPriors,
      mcpData = mcpData,
      mcpFitList = mcpFitList,
      mcpModel = mcpModel,
      outFUN = summary,
      renderFUN = renderPrint
    )
    
    mcpOutServer(
      id = "waic",
      formulasAndPriors = formulasAndPriors,
      mcpData = mcpData,
      mcpFitList = mcpFitList,
      mcpModel = mcpModel,
      outFUN = waic,
      renderFUN = renderPrint
    )
    
    mcpOutServer(
      id = "plot",
      formulasAndPriors = formulasAndPriors,
      mcpData = mcpData,
      mcpFitList = mcpFitList,
      mcpModel = mcpModel,
      outFUN = plot,
      renderFUN = renderPlot
    )
  })
}

#' MCP Output UI
#'
#' @param title The title of the output
#' @param showWidth Show the width slider
#' @rdname mcpOutServer
mcpOutUI <- function(id,
                     title,
                     outFUN = verbatimTextOutput,
                     showWidth = FALSE) {
  ns <- NS(id)
  
  tagList(tags$h4(title),
          outFUN(ns("modelOut")) %>% withSpinner(color = "#20c997"),
          if (showWidth) {
            sliderInput(
              ns("width"),
              "Summary width",
              min = 0.01,
              max = 0.99,
              value = 0.95,
              step = 0.01,
              width = "100%"
            )
          })
}

#' MCP Output Server
#'
#' @param id The module id
#' @param formulasAndPriors The reactive formulas and priors
#' @param mcpData The reactive mcp data
#' @param mcpFitList The reactive mcp fit list
#' @param mcpModel The reactive mcp model
#' @param outFUN The output function
#' @param renderFUN The render function
mcpOutServer <- function(id,
                         formulasAndPriors,
                         mcpData,
                         mcpFitList,
                         mcpModel,
                         outFUN,
                         renderFUN = renderPrint) {
  moduleServer(id, function(input, output, session) {
    output$modelOut <- renderFUN({
      validate(need(
        formulasAndPriors(),
        "Please 'Create MCP Lists' and 'Run MCP' first ..."
      ))
      validate(need(mcpData(), "Please load 'Plot Data' and 'Run MCP' first ..."))
      validate(need(mcpFitList(), "Please 'Run MCP' first ..."))
      validate(need(mcpModel(), "Please select MCP model first ..."))
      
      params <- ifelse(is.null(input[["width"]]), list(), list(width = input[["width"]]))
      
      do.call(outFUN, c(list(mcpModel()), params)) %>%
        shinyTryCatch(
          errorTitle = sprintf("Error during creating '%s' output", id),
          warningTitle = sprintf("Warning during creating '%s' output", id)
        )
    })
  })
}

# 3. Comparison of Models ----

#' MCP Compare Models UI
#'
#' @rdname mcpCompareModelsServer
mcpCompareModelsUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    selectInput(ns("method"), "Method", c("loo", "waic", "heuristic")),
    tags$br(),
    verbatimTextOutput(ns("compareModels")) %>% withSpinner(color = "#20c997")
  )
}

#' MCP Compare Models Server
#'
#' @inheritParams mcpOutServer
mcpCompareModelsServer <- function(id,
                                   formulasAndPriors,
                                   mcpData,
                                   mcpFitList) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    compareModels <- reactiveVal()
    
    output$compareModels <- renderPrint({
      validate(need(
        formulasAndPriors(),
        "Please 'Create MCP Lists' and 'Run MCP' first ..."
      ))
      validate(need(mcpData(), "Please load 'Plot Data' and 'Run MCP' first ..."))
      validate(need(mcpFitList(), "Please 'Run MCP' first ..."))
      
      compareFUN <- switch(input[["method"]],
                           loo = compareWithLoo,
                           waic = compareWithWAIC,
                           heuristic = compareWithHeuristic)
      
      mcpFitList() %>%
        compareFUN() %>%
        shinyTryCatch(errorTitle = "Error in model comparison", warningTitle = "Warning in model comparison")
    })
  })
}
