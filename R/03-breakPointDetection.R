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
            tabPanel("MCP Lists", formulasUI(ns("formulas"))),
            tabPanel(
              "MCP Model",
              mcpUI(ns("mcp")),
              tags$h4("Comparing models using loo"),
              verbatimTextOutput(ns("compareWithLoo"))
            )
          ),
          tags$br())
}

#' MCP UI
#'
#' @param id The module id
mcpUI <- function(id) {
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
              actionButton(ns("apply"), "Run Model", disabled = TRUE)
            )
          ),
          tags$br())
}


#' Break Point Detection Server
#'
#' Server function of the module
#'
#' @param id The module id
breakPointDetectionServer <- function(id, plotData) {
  moduleServer(id, function(input, output, session) {
    mcpData <- reactiveVal()
    mcpFit <- reactiveVal()
    
    # load data ----
    observe({
      mcpData(read.csv(file.path("data", "example_breakPoints.csv")))
    })
    
    # Formulas tab ----
    formulasAndPriors <- formulasServer("formulas")
    
    observe({
      req(formulasAndPriors())
      
      # enable the 'Run Model' button
      updateActionButton(session, "mcp-apply", disabled = FALSE)
    }) %>% bindEvent(formulasAndPriors())
    
    # Model tab ----
    
    ## run the mcp model ----
    observe({
      mcpFit(
        runMcp(
          lists = formulasAndPriors(),
          data = mcpData(),
          adapt = input[["mcp-adapt"]],
          chains = input[["mcp-chains"]],
          iter = input[["mcp-iter"]]
        )
      ) %>%
        withProgress(message = "Fitting mcp model", value = 0.5) %>%
        shinyTryCatch(errorTitle = "Error in fitting mcp model", warningTitle = "Warning in fitting mcp model")
    }) %>%
      bindEvent(input[["mcp-apply"]])
    
    ## render the output of comparing with loo ----
    output$compareWithLoo <- renderPrint({
      validate(need(
        formulasAndPriors(),
        "Please 'Create MCP Lists' and 'Run Model' first"
      ))
      validate(need(mcpFit(), "Please 'Run Model' first"))
      mcpFit() %>%
        compareWithLoo() %>%
        shinyTryCatch(errorTitle = "Error in comparing with loo", warningTitle = "Warning in comparing with loo")
    })
  })
}

#' Formulas UI
#'
#' @rdname formulasServer
formulasUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$br(),
    matrixUI(
      ns("segments"),
      title = "Segments",
      defaultCellContent = "d15N ~ 1 + time",
      exampleLabel = "Example Segments"
    ),
    tags$br(),
    matrixUI(
      ns("priors"),
      title = "Priors",
      defaultCellContent = "time_1 = dunif(-4, -0.5);",
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

#' Formulas Server
#'
#' @param id The module id
formulasServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    formulasAndPriors <- reactiveVal()
    
    segmentsMatrix <- matrixServer("segments", exampleFunction = getExampleMatrix, path = file.path("data", "example_breakPointSegments.csv"))
    priorsMatrix <- matrixServer("priors", exampleFunction = getExampleMatrix, path = file.path("data", "example_breakPointPriors.csv"))
    
    observe({
      req(segmentsMatrix(), priorsMatrix())
      
      # enable the 'Create MCP Formulas' button
      updateActionButton(session, "apply", disabled = FALSE)
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
      validate(need(formulasAndPriors(), "Please 'Create MCP Lists' first"))
      formulasAndPriors()
    })
    
    return(formulasAndPriors)
  })
}
