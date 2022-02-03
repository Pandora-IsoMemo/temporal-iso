################################
# Script to simulate Data for temporal estimation of isotopic data


library(dplyr)
library(tibble)
library(tidyr)


# a simple dataframe
simpleData <- function(){
  out <- data.frame(
    intStart = 0:5,
    intEnd = 1:6,
    bone1 = c(100, 50, 20, 10, 5, 2),
    bone2 = c(100, 10, 5, 1, 1, 1),
    tooth1 = c(0, 100, 0, 0, 0, 0),
    tooth2 = c(0, 0, 100, 0, 0, 0)
    )
  return(out)
}

# function to calculate the relative influence of time periods from renewal rates
calcInfluence <- function(x){
  x <- x/100
  out <- rep(0, length(x)) 
  temp <- 1
  for (i in length(x):1){
    out[i] <- x[i] * temp
    temp <- temp - out[i]
  }
  return(out)
}

# function to calculate normal distribution values from (simulated) data
resultFromTrue <- function(dat = simpleData(),
                                  trueIsoMean = c(-10, -12, -13, -11, -8, -7),
                                  trueIsoSD = c(1.5, 2, 1.5, 2, 1.5, 1.5)){
  stopifnot(length(trueIsoMean) == nrow(dat) & length(trueIsoSD) == nrow(dat))

  influence <- dat %>% select(-starts_with("int")) %>% mutate_all(calcInfluence)
  y <- list(
    mean = lapply(trueIsoMean * influence, sum),
    sd = lapply(lapply(trueIsoSD ^ 2 * influence, sum), sqrt)
  )
  return(y)
}


###########################################
expPercent <- function(nRow, start = 100){
  out <- floor(100 * exp(-runif(1) * 0:(nRow - 1)))
  return(out)
}

changeRandom <- function(x, prop = 0.5, start = 100){
  out <- x
  out[sample(floor(length(x) * prop), 1)] <- start
  return(out)
}

# simulate arbitrary data in the right format
simulData <- function(nBone = 3, nTooth = 5, nRow = 10, prop = 0.5, seed = NULL){
  set.seed(seed = seed)
  out <- tibble(
    intStart = 0:(nRow-1),
    intEnd = 1:nRow
    )
  
  bone <- lapply(1:nBone, expPercent, nRow = nRow)
  names(bone) <- paste("bone", 1:nBone, sep = "")
  tooth <- as_tibble(matrix(0, ncol = nTooth, nrow = nRow))
  tooth <- mutate_all(tooth, changeRandom, prop = prop)
  names(tooth) <- paste("tooth", 1:nTooth, sep = "")
  
  out <- bind_cols(out, as_tibble(do.call(cbind, bone)), tooth)

  return(out)
}



simulData()
