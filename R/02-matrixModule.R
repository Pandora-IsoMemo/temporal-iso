#' Matrix Module
#'
#' @param maxRows maximum number of rows
#' @param maxColumns maximum number of columns
#' @param defaultCellContent default cell content
#' @param exampleLabel example label
#' @rdname matrixServer
matrixUI <- function(id,
                     title = "Matrix",
                     maxRows = 10,
                     maxColumns = 5,
                     defaultCellContent = "",
                     exampleLabel = "Example Matrix") {
  ns <- NS(id)
  tagList(
    tags$h4(title),
    fluidRow(
      column(
        width = 2,
        selectInput(
          ns("rows"),
          "No. of rows",
          selected = 1,
          choices = 1:maxRows
        )
      ),
      column(
        width = 2,
        selectInput(
          ns("cols"),
          "No. of columns",
          selected = 1,
          choices = 1:maxColumns
        )
      ),
      column(
        width = 2,
        selectInput(
          ns("cellID"),
          "Cell ID (row, column)",
          selected = 1,
          choices = 1:1
        )
      ),
      column(
        width = 3,
        textInput(ns("cell"), "Cell content", value = defaultCellContent)
      ),
      column(
        width = 1,
        style = "margin-top: 1.75em;",
        actionButton(ns("set"), "Set")
      ),
      column(
        width = 2,
        align = "right",
        style = "margin-top: 1.75em;",
        actionButton(ns("example"), exampleLabel)
      )
    ),
    # output matrix
    tags$br(),
    verbatimTextOutput(ns("outputMatrix"))
  )
}

#' Matrix Server
#'
#' @param id namespace id
#' @param exampleFunction example function
#' @param ... additional arguments to example function
matrixServer <- function(id, exampleFunction, ...) {
  moduleServer(id, function(input, output, session) {
    dataMatrix <- reactiveVal()
    
    observe({
      # define empty matrix with number of rows and columns inputs
      req(input[["rows"]], input[["cols"]])
      nrow <- input[["rows"]] %>% as.numeric()
      ncol <- input[["cols"]] %>% as.numeric()
      dataMatrix(matrix(
        data = "",
        nrow = nrow,
        ncol = ncol,
        byrow = TRUE
      ))
      updateSelectInput(session, "cellID", choices = getCellChoices(nrow = nrow, ncol = ncol))
    }) %>%
      bindEvent(list(input[["rows"]], input[["cols"]]))
    
    output$outputMatrix <- renderPrint({
      validate(need(
        dataMatrix(),
        "Please click 'Set' first or load 'Example Priors'"
      ))
      dataMatrix()
    })
    
    observe({
      req(input[["cellID"]], input[["cell"]], length(dataMatrix()))
      # set new value
      id <- as.numeric(input[["cellID"]])
      new_matrix <- dataMatrix()
      new_matrix[id] <- input[["cell"]]
      dataMatrix(new_matrix)
      
      # inc id
      maxID <- length(dataMatrix())
      newID <- ifelse(id == maxID, 1, id + 1)
      updateSelectInput(session, "cellID", selected = newID)
      
    }) %>%
      bindEvent(input[["set"]])
    
    observe({
      newMatrix <- exampleFunction(...)
      dataMatrix(newMatrix)
      updateSelectInput(session, "cellID", choices = getCellChoices(nrow = nrow(newMatrix), ncol = ncol(newMatrix)))
    }) %>%
      bindEvent(input[["example"]])
    
    return(dataMatrix)
  })
}

#' Get cell choices
#' 
#' @param nrow Number of rows
#' @param ncol Number of columns
#' 
#' @return A named vector of cell choices
getCellChoices <- function(nrow, ncol) {
  getCellLabel <- function(cellID, matrixDim) {
    cellInd <- arrayInd(cellID, matrixDim)
    
    sprintf("(%s, %s)", cellInd[1], cellInd[2])
  }
  
  choices <- 1:(nrow * ncol)
  
  # get ID labels
  labels <- sapply(choices, getCellLabel, matrixDim = c(nrow, ncol))
  names(choices) <- labels
  
  choices
}
