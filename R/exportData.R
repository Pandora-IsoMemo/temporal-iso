#' Filename of Export
#'
#' @param fileending character csv or xlsx
#' @export
exportFilename <- function(fileending){
  paste("isotopeData", fileending, sep = ".")
}

#' Export to csv
#'
#' @param file filename
#' @param dat data.frame
#' @param colseparator column seperator
#' @param decseparator decimal seperator
#' @export
exportCSV <- function(file, dat, colseparator, decseparator){
  write.table(x = dat, file = file, sep = colseparator,
              dec = decseparator, row.names = FALSE)
}

#' Export to xlsx
#'
#' @param file filename
#' @param dat data.frame
#' @export
exportXLSX <- function(file, dat){
  write.xlsx(dat, file)
}

#' Export to json
#'
#' @param file filename
#' @param dat data.frame
#' @export
exportJSON <- function(file, dat){
  json <- toJSON(dat)
  write(json, file)
}