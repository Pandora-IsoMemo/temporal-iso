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
              verbatimTextOutput(ns("compareWithLoo")) %>% withSpinner(color = "#20c997")
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
              #actionButton(ns("loadExampleDf"), "Load Example Data"),
              actionButton(ns("apply"), "Run Model with 'Plot Data'", disabled = TRUE)
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
    mcpData <- reactiveVal()
    mcpFit <- reactiveVal()
    
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
    formulasAndPriors <- formulasServer("formulas")
    
    observe({
      req(formulasAndPriors(), mcpData())
      
      # enable the 'Run Model' button
      updateActionButton(session, "mcp-apply", disabled = FALSE)
    }) %>% bindEvent(formulasAndPriors(), mcpData())
    
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
        shinyTryCatch(errorTitle = "Error in fitting mcp model", warningTitle = "Warning in fitting mcp model") %>%
        withProgress(message = "Fitting MCP model...", value = 0.5)
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

#' Formulas Server
#'
#' @param id The module id
formulasServer <- function(id) {
  moduleServer(id, function(input, output, session) {
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
        and the values are prior distributions in JAGS notation. If you don’t specify a prior for a parameter,
        mcp will use a default weakly informative prior.",
      link = "https://lindeloev.github.io/mcp/articles/priors.html"
    )
    
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
