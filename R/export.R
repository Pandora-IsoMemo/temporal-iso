

setGeneric(name = "export",
           def = function(fit, file, interval, ...)
           {
             standardGeneric("export")
           }
)



#' Export of TemporalIso samples as csv
#' 
#' @description Writes the interval samples of a TemporalIso-object to a .csv-File.
#' 
#' @param fit TemporalIso: An instance of class TemporalIso,
#' generated from \code{\link{estimateIntervals}}
#' 
#' @param file character: a character string specifying the path to a .csv-file.
#' @param interval numeric values which are part of the specified time intervals.
#' Defaults to all intervals being exported
#' 
#' @param ... Optional arguments to \link[utils]{write.csv}.
#' #' 
#' @seealso \link[utils]{write.csv}
#' @name export
#' @aliases export,TemporalIso-method
#' @export
setMethod("export",
          signature(fit = "TemporalIso"),
          function(fit, file, interval = NULL, ...) {
            if (substr(file, nchar(file) - 3, nchar(file)) != ".csv"){
              stop("filepath should lead to a csv-file")
            }
            stopifnot(all(interval %in% fit@time))
            if (is.null(interval)) interval <- fit@time
            dat <- as.data.frame(rstan::extract(fit)$interval)
            names(dat) <- fit@time
            out <- dat[interval]
            
            names(out) <- paste0("interval_", interval)
            write.csv(out, file = file, ...)
          })
