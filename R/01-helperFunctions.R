# helper functions

#' Get Example Matrix of Data
#' 
#' @export
getExampleDataMatrix <- function(sd = FALSE) {
  #   as.matrix(data.frame(
  #     individual = rep(1:2, each = 6),
  #     intStart = rep(0:5, 2),
  #     intEnd = rep(1:6, 2),
  #     bone1 = c(100, 50, 20, 10, 5, 2, 100, 80, 30, 8, 15, 4),
  #     bone2 = c(100, 10, 5, 1, 1, 1, 90, 40, 5, 1, 12, 1),
  #     tooth1 = c(0, 100, 0, 0, 0, 0, 0, 80, 20, 0, 0, 0),
  #     tooth2 = c(0, 0, 100, 0, 0, 0, NA, NA, NA, NA, NA, NA)
  #   ))
  
  exmpl <- data.frame(
    individual = rep(1:2, each = 6),
    intStart = rep(0:5, 2),
    intEnd = rep(1:6, 2),
    bone1 = c(100, 50, 20, 10, 5, 2, 100, 80, 30, 8, 15, 4),
    bone2 = c(100, 10, 5, 1, 1, 1, 90, 40, 5, 1, 12, 1),
    tooth1 = c(0, 100, 0, 0, 0, 0, 0, 80, 20, 0, 0, 0),
    tooth2 = c(0, 0, 100, 0, 0, 0, NA, NA, NA, NA, NA, NA)
  ) %>% slice(-4)
  exmpl$individual[4:5] <- 11
  if(sd == TRUE){
    exmpl[,c("bone1","bone2", "tooth1", "tooth2")] <- 0
  }
  as.matrix(exmpl)
}

#' Get Example Matrix of Isotopic Values
#'
#' @export
getExampleIsotopeVals <- function() {
  # matrix(c(c(1,1,1,1,2,2,2,2),
  #                         c(-10, -7, -12, -9, -14, -15, -8, NA), 
  #                         c(2, 1.5, 2.5, 2.5, 1.5, 3.5, 3, NA)), 
  #                       ncol = 3, 
  #                       dimnames = list(NULL, c("individual", "y_mean", "y_sigma")))
  #
  data.frame(individual = rep(c(1,11,2), each = 4),
             y_mean = c(rep(c(-10, -7, -12, -9), 2), c(-14, -15, -8, NA)),
             y_sigma = c(rep(c(2, 1.5, 2.5, 2.5), 2), c(1.5, 3.5, 3, NA))
  ) %>%
    as.matrix()
}


#' Get Entry
#' 
#' Get all list elements "entry" of a list of models 
#' 
#' @param modelList (list) of saved model objects
#' @param entry (character) name of the list element, e.g. "fit"
#' 
#' @export
getEntry <- function(modelList, entry) {
  lapply(modelList, function(model) model[[entry]])
}


#' Default Inputs for UI
#' 
#' Default values for user inputs
#' 
#' @export
defaultInputsForUI <- function() {
  list(xmin = 0,
       xmax = 1,
       ymin = 0,
       ymax = 1,
       from = 0,
       to = 5,
       by = 0.5,
       from2 = 0,
       to2 = 5)
}

#' Update only matrix row/colnames
#' 
#' Used for renewal uncertainties
#' 
#' @export
updateMatrixNamesInput <- function(session, inputId, value, value2) {
  stopifnot(is.matrix(value))
  
  message <- list(value = list(
    data = value2,
    rownames = rownames(value),
    colnames = colnames(value)
  ))
  session$sendInputMessage(inputId, message)
}