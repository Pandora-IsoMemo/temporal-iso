#' Clean and Split Data
#'
#' @param indVar (character) name of variable for individuals
#' @param renewalRates (matrix) matrix with renewal rates
#' @param renewalRatesUnc (matrix) matrix with renewal rates uncertainties
#' 
#' @export
cleanAndSplitData <-
  function(indVar, renewalRates, renewalRatesUnc) {
    if (is.null(indVar))
      return(NULL)

    splitVar <- extractIndividuals(matrix = renewalRates, indVar = indVar)
    
    # clean up renewalRates
    renewalRates <- renewalRates %>% 
      data.frame()
    validCols <- colSums(!is.na(renewalRates)) > 0
    renewalRates <- renewalRates %>%
      .[validCols] #%>%
    #filter(complete.cases(.))
    
    renewalRatesPerInd <- split(renewalRates, splitVar)
    validColsPerInd <- lapply(renewalRatesPerInd,
                              function(x) {
                                !apply(x, 2, function(y)
                                  all(is.na(y)))
                              })
    
    renewalRatesPerInd <- lapply(seq_along(renewalRatesPerInd),
                                 function(x) {
                                   renewalRatesPerInd[[x]][, validColsPerInd[[x]]]
                                 })
    names(renewalRatesPerInd) <- names(validColsPerInd)
    
    splitVarUnc <- extractIndividuals(matrix = renewalRatesUnc, indVar = indVar)
    
    # clean up renewalRatesUnc respectively and set NA to zero
    renewalRatesUnc <- renewalRatesUnc %>% 
      data.frame()
    renewalRatesUnc <- renewalRatesUnc %>%
      .[validCols] #%>%
    #filter(complete.cases(.))
    
    renewalRatesUncPerInd <- split(renewalRatesUnc, splitVarUnc)
    renewalRatesUncPerInd <- lapply(seq_along(renewalRatesUncPerInd),
                                    function(x) {
                                      # select valid columns
                                      res <- renewalRatesUncPerInd[[x]][, validColsPerInd[[x]]]
                                      # set NA uncertainty to 0
                                      res[is.na(res)] <- 0
                                      res
                                    })
    names(renewalRatesUncPerInd) <- names(validColsPerInd)
    
    list(renewalRatesPerInd = renewalRatesPerInd,
         renewalRatesUncPerInd = renewalRatesUncPerInd)
  }

extractIndividuals <- function(matrix, indVar) {
  if (!(indVar %in% colnames(matrix)) && !is.null(rownames(matrix)) && all(sapply(rownames(matrix), function(x) x != ""))) {
    rownames(matrix) %>%
      as.character()
  } else {
    matrix[, indVar] %>%
      unname() %>%
      as.character()
  }
}
