#' Get Example Matrix
#'
#' @param path path to example matrix
getExampleMatrix <- function(path) {
  df <- read.csv(path)
  
  df[is.na(df)] <- ""
  
  res <- df %>% as.matrix()
  colnames(res) <- NULL
  
  res
}

#' Get the comb matrix
#'
#' Get the combined matrix of segments and priors
#'
#' @param segments A matrix containing the segmented formulas
#' @param priors A matrix containing the priors
#'
#' @return A matrix containing the segmented formulas and priors
getComb <- function(segments, priors) {
  # find rows and columns in segments that are not all ""
  row_indices <- apply(segments, 1, function(x)
    any(x != ""))
  col_indices <- apply(segments, 2, function(x)
    any(x != ""))
  
  # subset segments and priors based on these indices
  segments <- segments[row_indices, col_indices, drop = FALSE]
  priors <- priors[row_indices, col_indices, drop = FALSE]
  
  # concat matrices
  concatenated_matrix <- matrix(
    paste(segments, priors, sep = "*+*"),
    nrow = nrow(segments),
    ncol = ncol(segments)
  )
  
  #SET: change number of concatenated_matrix below so that it matches the number of rows s_rows
  
  # List all possible cell combinations among the matrix considering only cells in different columns and must follow column order
  
  col_list <- lapply(1:ncol(segments), function(x)
    concatenated_matrix[, x])
  
  comb <- do.call(expand.grid, col_list)
  
  # Data conversion to avoid warnings
  
  comb[] <- lapply(comb, as.character)
  
  comb
}

#' Clean the comb matrix
#'
#' @param comb A matrix containing the segmented formulas and priors
#'
#' @return A cleaned matrix
cleanComb <- function(comb) {
  n_rows <- nrow(comb)
  
  # Check each row for '*+*' and if found replace that cell and all following cells in the row with ""
  for (i in 1:n_rows) {
    # Get the indices of the cells equal to "*+*"
    replace_indices <- which(comb[i, ] == "*+*")
    
    # if '*+*' is found in the row replace it and following elements with ""
    if (length(replace_indices) > 0) {
      comb[i, replace_indices[1]:ncol(comb)] <- ""
    }
  }
  
  
  # Remove Blank Rows
  comb <- comb[apply(comb, 1, function(x)
    any(x != "")), , drop = FALSE]
  
  # Remove duplicate rows
  comb <- unique(comb)
  
  comb
}

#' Split the comb matrix into two matrices
#'
#' @param comb A matrix containing the segmented formulas and priors
#'
#' @return A list of two matrices
splitComb <- function(comb) {
  # Create two empty matrices with the same dimensions as comb
  mat1 <- matrix(ncol = ncol(comb), nrow = nrow(comb))
  mat2 <- matrix(ncol = ncol(comb), nrow = nrow(comb))
  
  # Write a loop to iterate through each cell of comb
  for (i in seq_len(nrow(comb))) {
    for (j in seq_len(ncol(comb))) {
      # Check if "*+*" is in the cell value
      if (grepl("\\+", as.character(comb[i, j]))) {
        # Split the cell value using the separator "*+*"
        split_vals <- strsplit(as.character(comb[i, j]), split = "\\*\\+\\*")[[1]]
        
        # If "*+*" is found, split the cell value into two, assigning each part to the corresponding cell in mat1 and mat2
        mat1[i, j] <- split_vals[1]
        mat2[i, j] <- split_vals[2]
        
      } else {
        # If "*+*" is not found in the cell value, assign "" to the corresponding cells in mat1 and mat2
        mat1[i, j] <- ""
        mat2[i, j] <- ""
      }
    }
  }
  
  # Replacing NA values with empty string
  mat1[is.na(mat1)] <- ""
  mat2[is.na(mat2)] <- ""
  
  # Return the two matrices
  list(mat1 = mat1, mat2 = mat2)
}

#' Set formulas and priors
#'
#' @param splittedComb A list of matrices containing the segmented formulas and priors
#'
#' @return A list of lists containing the segmented formulas and priors
setFormulasAndPriors <- function(splittedComb) {
  mat1 <- splittedComb$mat1
  mat2 <- splittedComb$mat2
  
  # Creating lists to hold all the lists
  lists_seg <- vector("list", nrow(mat1))
  lists_prior <- vector("list", nrow(mat1))
  
  # Looping to convert each string in the matrix to a formula and adding to the respective list.
  
  for (i in 1:nrow(mat1)) {
    lists_seg[[i]] <- list()
    lists_prior[[i]] <- list()
    
    for (j in 1:ncol(mat1)) {
      if (mat1[i, j] != "") {
        lists_seg[[i]] <- append(lists_seg[[i]], as.formula(mat1[i, j]))
        
        # For priors
        if (mat2[i, j] != "") {
          # first split string by commas corresponding to different priors
          splits <- strsplit(mat2[i, j], split = ";")[[1]]
          
          for (k in 1:length(splits)) {
            # split the string by =
            split_str <- strsplit(splits[k], "=")[[1]]
            
            if (!is.na(split_str[1]) && !is.na(split_str[2])) {
              lists_prior[[i]] <- append(lists_prior[[i]], trimws(split_str[2]))
              names(lists_prior[[i]])[length(lists_prior[[i]])] <- trimws(split_str[1])
            }
          }
        }
      }
    }
    
  }
  
  list(lists_seg = lists_seg, lists_prior = lists_prior)
}

#' Run mcp model
#'
#' @param lists A list of lists containing the segmented formulas and priors
#' @param ... Additional arguments to pass to mcp
#'
#' @return A list of mcp model fits
runMcp <- function(lists, ...) {
  lists_seg <- lists$lists_seg
  lists_prior <- lists$lists_prior
  
  # Loop through each list and run model
  fit <- vector(mode = "list", length = length(lists_seg))
  
  for (i in 1:length(lists_seg)) {
    fit[[i]] <- mcp(model = lists_seg[[i]], prior = lists_prior[[i]], ...)
  }
  
  fit
}

#' Compare models using loo
#'
#' @param fit A list of mcp model fits
#'
#' @return A loo model comparison object
compareWithLoo <- function(fit) {
  #Comparing models using loo
  
  #Define list
  loo_model <- vector("list", length(fit))
  
  for (i in 1:length(fit)) {
    loo_model[[i]] <- loo(fit[[i]])
  }
  
  #Results of model comparison
  loo_compare(loo_model)
}
