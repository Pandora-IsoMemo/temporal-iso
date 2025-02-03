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
#' @importFrom ChangeR changePointsServer changePointsUI
#' @importFrom DataTools checkAnyNonNumericColumns downloadModelUI downloadModelServer 
#'  importDataUI importDataServer importUI importServer importOptions renameExistingNames
#' @importFrom dplyr %>% arrange bind_cols bind_rows cur_group_id distinct do group_by mutate n 
#'  select slice ungroup
#' @importFrom ggplot2 aes coord_cartesian element_line element_text 
#'  geom_line geom_point geom_ribbon geom_vline ggplot ggplot_build ggtitle labs    
#'  scale_colour_manual scale_fill_manual scale_shape_manual scale_size_manual 
#'  scale_x_continuous scale_y_continuous sec_axis theme
#' @importFrom parallel detectCores
#' @importFrom rlang .data
#' @importFrom rstan sampling extract
#' @importFrom shinycssloaders withSpinner
#' @importFrom shinyjs alert enable runjs
#' @importFrom shinyTools addCustomPointsToGGplot calculateRescalingFactors
#'  customPointsServer customPointsUI dataExportButton dataExportServer 
#'  extractTitle formatPointsOfGGplot formatScalesOfGGplot formatTitlesOfGGplot
#'  plotExportButton plotExportServer plotLegendServer plotLegendUI plotPointsServer plotPointsUI
#'  plotRangesServer plotRangesUI plotTitlesServer plotTitlesUI setLegendThemeOfGGplot
#'  shinyTryCatch
#' @importFrom shinyWidgets pickerInput updatePickerInput
#' @importFrom stats approx dnorm lm median na.omit quantile sd
#' @importFrom utils write.csv combn
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
