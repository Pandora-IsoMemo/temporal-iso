#' Download model module
#'
#' UI function to download a zip file with notes and a list of models
#'
#' @param id id of module
#' @param label label of module
#'
#' @rdname downloadModel
#'
#' @export
downloadModelUI <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    tags$h5(label),
    pickerInput(ns("selectedModels"),
                choices = NULL,
                options = list(`actions-box` = TRUE),
                multiple = T),
    checkboxInput(ns("onlyInputs"), "Store only input data and model options"),
    textAreaInput(ns("notes"), "Notes"),
    HTML("<br>"),
    downloadButton(ns("downloadModel"), "Download"),
    tags$br()
  )
}


#' Server function download model
#'
#' Backend for download model module
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param savedModels (reactive) list of models of class \code{\link{TemporalIso}}
#' @param uploadedNotes (reactive) variable that stores content for README.txt
#'
#' @export
downloadModel <- function(input, output, session, savedModels, uploadedNotes){
  
  observe({
    req(savedModels())
    updatePickerInput(session, "selectedModels", choices = names(savedModels()),
                      selected = names(savedModels())[length(savedModels())])
    updateTextAreaInput(session, "notes", value = uploadedNotes())
  })
  
  output$downloadModel <- downloadHandler(
    filename = function() {
      gsub("[ ]", "_", paste0(Sys.time(), "_OsteoBioR.zip"))
    },
    content = function(file) {
      zipdir <- tempdir()
      modelfile <- file.path(zipdir, "model.rds")
      notesfile <- file.path(zipdir, "README.txt")
      helpfile <- file.path(zipdir, "help.html")
      
      req(savedModels(), input$selectedModels)
      
      if (input$onlyInputs) {
        model <- lapply(savedModels()[input$selectedModels], 
                        function(thisModel) {
                          thisModel$fit <- NULL 
                          thisModel
               })
      } else {
        model <- savedModels()[input$selectedModels]
      }
      
      
      req(model)
      saveRDS(list(model = model,
                   version = packageVersion("OsteoBioR")),
              file = modelfile)
      writeLines(input$notes, notesfile)
      save_html(getHelp(input$tab), helpfile)
      zipr(file, c(modelfile, notesfile, helpfile))
    }
  )
}


#' Upload model module
#'
#' UI function to upload a zip file with notes and a list of models
#'
#' @param id id of module
#' @param label label of module
#'
#' @rdname uploadModel
#'
#' @export
uploadModelUI <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    tags$h5(label),
    selectInput(
      ns("remoteModel"),
      label = "Select remote model",
      choices = dir(file.path(settings$pathToSavedModels)) %>%
        sub(pattern = '\\.zip$', replacement = ''),
      selected = NULL
    ),
    actionButton(ns("loadRemoteModel"), "Load Remote Model"),
    tags$br(), tags$br(),
    #helpText("Remote models are only available on on https://isomemoapp.com")
    fileInput(ns("uploadModel"), label = NULL)
  )
}


#' Server function upload model
#'
#' Backend for upload model module
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param savedModels (reactive) list of models of class \code{\link{TemporalIso}}
#' @param uploadedNotes (reactive) variable that stores content of README.txt
#' @param fit (reactive) model of class \code{\link{TemporalIso}} that is currently displayed
#' @param uploadedModelSpecInputs (reactive) modelSpecifications for the current fit
#' @param uploadedDataMatrix (reactive) shinyMatrix matrixInput
#' @param uploadedIsotope (reactive) shinyMatrix matrixInput
#'
#' @export
uploadModel <- function(input, output, session, savedModels, uploadedNotes, fit,
                        uploadedModelSpecInputs, uploadedDataMatrix, uploadedIsotope){
  pathToModel <- reactiveVal(NULL)
  
  observeEvent(input$uploadModel, {
    pathToModel(input$uploadModel$datapath)
  })
  
  observeEvent(input$loadRemoteModel, {
    pathToModel(file.path(settings$pathToSavedModels, paste0(input$remoteModel, ".zip")))
  })
  
  observeEvent(pathToModel(), {
    req(pathToModel)
    model <- NULL
    
    res <- try({
      zip::unzip(pathToModel())
      modelImport <- readRDS("model.rds")
      uploadedNotes(readLines("README.txt"))
    })
    
    if (inherits(res, "try-error")) {
      shinyjs::alert("Could not load file.")
      return()
    }
    
    file.remove("model.rds")
    file.remove("README.txt")
    file.remove("help.html")
    
    if (!exists("modelImport")) {
      shinyjs::alert("File format not valid. Model object not found.")
      return()
    }
    
    if (is.null(modelImport$model)) {
      shinyjs::alert("Empty model object.")
      return()
    }
    
    savedModels(c(savedModels(), extractModel(modelImport$model)))
    
    currentModel <- savedModels()[[length(savedModels())]]
    
    fit(currentModel$fit)
    uploadedModelSpecInputs(currentModel$modelSpecifications)
    uploadedDataMatrix(currentModel$inputDataMatrix)
    uploadedIsotope(currentModel$inputIsotope)
    
    alert("Model loaded")
  })
  
}


#' Extract Model
#' 
#' Extract model fit and possibly model specification inputs from a previously exported
#' model object.
#' @param modelFromImport imported model object
extractModel <- function(modelFromImport) {
  if (all(names(modelFromImport[[1]]) %in% c("fit", "modelSpecifications", 
                                             "inputDataMatrix", "inputIsotope"))) {
    # current format of exported models: list(fit = fit, modelSpecifications = modelSpecifications)
    return(modelFromImport)
  } else { 
    # former format of exported models: fit
    return(lapply(modelFromImport, function(fit) list(fit = fit,
                                                      modelSpecifications = list(),
                                                      inputDataMatrix = NULL,
                                                      inputIsotope = NULL)
                  ))
  }
}
