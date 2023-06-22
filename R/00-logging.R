#' Logging
#' 
#' @inheritParams futile.logger::flog.info
#'
#' @export
logging <- function(msg, ...) {
  futile.logger::flog.info(msg, ...)
}

#' LogDebug
#' 
#' @inheritParams futile.logger::flog.debug
#'
#' @export
logDebug <- function(msg, ...) {
  futile.logger::flog.debug(msg, ...)
}

#' LogWarn
#' 
#' @inheritParams futile.logger::flog.warn
#'
#' @export
logWarn <- function(msg, ...) {
  futile.logger::flog.warn(msg, ...)
}
