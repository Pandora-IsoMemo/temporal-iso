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
            tabPanel("MCP Lists from Segments & Priors", mcpFormulasUI(ns("formulas"))),
            tabPanel("MCP Modeling",
              mcpModelingUI(ns("mcp")),
              tags$hr(),
              fluidRow(
                column(6, selectInput(ns("showModel"), "Show MCP model", choices = c("'Run MCP' first ..." = ""))),
                column(6, sliderInput(ns("MCPSummaryWidth"), "Summary width", min = 0.01, max = 0.99, value = 0.95, step = 0.01))
              ),
              tags$h4("Model Summary"),
              verbatimTextOutput(ns("modelSummary")) %>% withSpinner(color = "#20c997")
            ),
            tabPanel("Comparison of Models",
                     selectInput(ns("method"), "Method", c("loo", "waic", "heuristic")),
                     tags$br(),
                     verbatimTextOutput(ns("compareModels")) %>% withSpinner(color = "#20c997")
                     )
          ),
          tags$br())
}

#' MCP Modeling UI
#'
#' @param id The module id
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
              #actionButton(ns("loadExampleDf"), "Load Example Data"),
              actionButton(ns("apply"), "Run MCP", disabled = TRUE)
            )
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
    mcpFitList <- reactiveVal()
    mcpModel <- reactiveVal()
    
    # load data ----
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
    
    # observe({
    #   mcpData(read.csv(file.path("data", "example_breakPoints.csv")))
    # }) %>%
    #   bindEvent(input[["mcp-loadExampleDf"]])
    
    # Formulas tab ----
    formulasAndPriors <- mcpFormulasServer("formulas")
    
    observe({
      req(formulasAndPriors(), mcpData())
      
      # enable the 'Run Model' button
      shinyjs::enable(ns("mcp-apply"), asis = TRUE)
      #updateActionButton(session, "mcp-apply", disabled = FALSE) # not working with current version in docker
    }) %>% bindEvent(formulasAndPriors(), mcpData())
    
    # Model tab ----
    
    ## run the mcp model ----
    observe({
      res <- runMcp(
        lists = formulasAndPriors(),
        data = mcpData(),
        adapt = input[["mcp-adapt"]],
        chains = input[["mcp-chains"]],
        iter = input[["mcp-iter"]]
      ) %>%
        shinyTryCatch(errorTitle = "Error in fitting mcp model", warningTitle = "Warning in fitting mcp model") %>%
        withProgress(message = "Fitting MCP model...", value = 0.5)
      
      mcpModels <- seq_along(res)
      names(mcpModels) <- paste("Model", mcpModels)
      if (length(mcpModels) > 0) mcpSelected <- 1 else mcpSelected <- NULL
      updateSelectInput(session, "showModel", choices = mcpModels, selected = mcpSelected)
      
      mcpFitList(res)
    }) %>%
      bindEvent(input[["mcp-apply"]])
    
    observe({
      req(mcpFitList(), input[["showModel"]])
      res <- mcpFitList()[[as.numeric(input[["showModel"]])]]
      mcpModel(res)
    }) %>% bindEvent(input[["showModel"]])
    
    ## render the output of MCP models ----
    output$modelSummary <- renderPrint({
      validate(need(
        formulasAndPriors(),
        "Please 'Create MCP Lists' and 'Run MCP' first"
      ))
      validate(need(mcpData(), "Please load 'Plot Data' and 'Run MCP' first ..."))
      validate(need(mcpFitList(), "Please 'Run MCP' first ..."))
      validate(need(mcpModel(), "Please select MCP model first ..."))
      
      #plot(fit[[x]]) plot for model x
      summary(mcpModel(), width = input[["MCPSummaryWidth"]])
    })
    
    ## render the output of comparing models ----
    output$compareModels <- renderPrint({
      validate(need(
        formulasAndPriors(),
        "Please 'Create MCP Lists' and 'Run MCP' first ..."
      ))
      validate(need(mcpData(), "Please load 'Plot Data' and 'Run MCP' first ..."))
      validate(need(mcpFitList(), "Please 'Run MCP' first ..."))
      
      compareFUN <- switch(
        input[["method"]],
        loo = compareWithLoo,
        waic = compareWithWAIC,
        heuristic = compareWithHeuristic
      )
      
      mcpFitList() %>%
        compareFUN() %>%
        shinyTryCatch(errorTitle = "Error in model comparison", warningTitle = "Warning in model comparison")
    })
  })
}

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
    
    ## render the output of creating formulas ----
    output$mcpFormulas <- renderPrint({
      validate(need(formulasAndPriors(), "Please 'Set' Segments and Priors first ..."))
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
