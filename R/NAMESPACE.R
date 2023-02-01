#' The 'OsteoBioR' package.
#' 
#' @description A DESCRIPTION OF THE PACKAGE
#' 
#' @docType package
#' @name OsteoBioR-package
#' @aliases OsteoBioR
#' @useDynLib OsteoBioR, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @import rstantools
#' @import shiny
#' @import shinythemes
#' @importFrom colourpicker colourInput 
#' @importFrom DataTools checkAnyNonNumericColumns importDataUI importDataServer
#' @importFrom dplyr arrange bind_rows distinct slice
#' @importFrom ggplot2 aes_string element_line element_text ggplot geom_line geom_point geom_ribbon 
#' labs scale_x_continuous theme ggtitle scale_y_continuous geom_vline coord_cartesian sec_axis
#' @importFrom htmltools save_html
#' @importFrom jsonlite toJSON
#' @importFrom magrittr %>%
#' @importFrom openxlsx read.xlsx write.xlsx
#' @importFrom rlang .data
#' @importFrom rstan sampling extract
#' @importFrom shinyjs alert
#' @importFrom shinyWidgets pickerInput updatePickerInput
#' @importFrom stats approx dnorm lm median quantile sd
#' @importFrom utils write.csv write.table combn packageVersion
#' @importFrom zip zipr
#' @references
#' Stan Development Team (NA). RStan: the R interface to Stan. R package version NA. http://mc-stan.org
#' 
#' 
globalVariables(".")
NULL

#' Server and UI Functions for Shiny Module
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session
#' @param id namespace id
#' @param title title of tab in tabset panel
#'
#' @name shinyModule
NULL
