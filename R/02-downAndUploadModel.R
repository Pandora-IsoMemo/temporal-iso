#' Download model module
#'
#' UI function to download a zip file with notes and a list of models
#'
#' @param id id of module
#' @param label label of module
#'
#' @rdname downloadModel
downloadModelUI <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    tags$h5(label),
    pickerInput(ns("selectedModels"),
                choices = NULL,
                options = list(`actions-box` = TRUE),
                multiple = T),
    HTML("<br>"),
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
      modelfile <- file.path(zipdir, "model.Rdata")
      notesfile <- file.path(zipdir, "README.txt")
      
      req(savedModels(), input$selectedModels)
      model <- savedModels()[input$selectedModels]
      
      req(model)
      save(model, file = modelfile)
      writeLines(input$notes %>% addPackageVersionNo(),
                 notesfile)
      zipr(file, c(modelfile, notesfile))
    }
  )
}


#' Upwnload model module
#'
#' UI function to upload a zip file with notes and a list of models
#'
#' @param id id of module
#' @param label label of module
#'
#' @rdname uploadModel
uploadModelUI <- function(id, label) {
  ns <- NS(id)
  
  tagList(
    HTML("<br>"),
    tags$h5(label),
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
uploadModel <- function(input, output, session, savedModels, uploadedNotes){

  observeEvent(input$uploadModel, {
    model <- NULL
    
    res <- try({
      zip::unzip(input$uploadModel$datapath)
      load("model.Rdata")
      uploadedNotes(readLines("README.txt") %>% .[1])
    })
    
    if (inherits(res, "try-error") || !exists("model")) {
      alert("Could not read model from file")
      return()
    }
    
    if (!is.null(model)) {
      savedModels(c(savedModels(), model))
      updateSelectInput(session, "savedModels", choices = names(savedModels()))
    }
    
    alert("Model loaded")
  })
  
}


#' Add Package Version Number
#' 
#' @param txt (character) text to which version number is appended
addPackageVersionNo <- function(txt){
  versionNo <- packageVersion("OsteoBioR") %>%
    as.character() %>%
    strsplit(split = "\\.") %>% 
    unlist() %>% 
    paste(collapse = ".")
  
  paste0(txt, "\n\n", "OsteoBioR version ", versionNo, " .")
}
