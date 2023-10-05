#' Extract Saved Models
#' 
#' @param upload (list) list of saved model objects with entries "data", "inputs" and "model"
extractSavedModels <- function(upload) {
  # old format < 23.10.0
  if (length(upload[["data"]]) == 0) {
    uploadedData <- upload[["model"]]
    return(uploadedData)
  }
  
  if (length(upload[["model"]]) > 0) {
    # model output was saved
    uploadedData <- combineDataAndModelOutputs(
      modelData = upload[["data"]],
      modelOutput = upload[["model"]]
    )
    return(uploadedData)
  } else {
    # only data and inputs were saved
    uploadedData <- upload[["data"]]
  }
  
  return(uploadedData)
}

#' Combine Data and Model Outputs
#'
#' @param modelData list of model data objects
#' @param modelOutput list of model output objects
combineDataAndModelOutputs <- function(modelData, modelOutput) {
  stopifnot("Cannot combine data and model output!" = all(names(modelData) == names(modelOutput)))
  
  model <- modelData
  for (name in names(model)) {
    model[[name]] <- c(model[[name]], modelOutput[[name]])
  }
  
  model
}

#' Remove Model Outputs
#'
#' @param models list of model objects
removeModelOutputs <- function(models) {
  lapply(models, function(model) {
    model$fit <- NULL
  })
}

#' Extract Model Outputs
#'
#' @param models list of model objects
extractModelOutputs <- function(models) {
  lapply(models, function(model) {
    model$modelSpecifications <- NULL
    model$inputDataMatrix <- NULL
    model$inputIsotope <- NULL
    model
  })
}
