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
#' @importFrom DataTools checkAnyNonNumericColumns downloadModelUI downloadModelServer 
#'  importDataUI importDataServer importOptions renameExistingNames tryCatchWithWarningsAndErrors
#' @importFrom dplyr arrange bind_cols bind_rows cur_group_id distinct do group_by mutate n select
#'  slice ungroup
#' @importFrom ggplot2 aes coord_cartesian element_line element_text 
#'  geom_line geom_point geom_ribbon geom_vline ggplot ggtitle labs    
#'  scale_colour_manual scale_fill_manual scale_shape_manual scale_size_manual 
#'  scale_x_continuous scale_y_continuous sec_axis theme
#' @importFrom htmltools save_html
#' @importFrom jsonlite toJSON
#' @importFrom magrittr %>%
#' @importFrom openxlsx write.xlsx
#' @importFrom parallel detectCores
#' @importFrom rlang .data
#' @importFrom rstan sampling extract
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyjs alert
#' @importFrom shinyTools dataExportButton dataExportServer formatPointsOfGGplot
#'  formatRangesOfGGplot formatTitlesOfGGplot plotExportButton plotExportServer
#'  plotPointsServer plotPointsUI plotRangesServer plotRangesUI plotTitlesServer plotTitlesUI
#' @importFrom shinyWidgets pickerInput updatePickerInput
#' @importFrom stats approx dnorm lm median quantile sd
#' @importFrom utils write.csv write.table combn
#' @importFrom yaml read_yaml
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
