testthat::test_that("setVarsForUncMatrix and cleanAndSplitData",  {
  # test example data
  testRenewalRates <- structure(
    c(1, 1, 1, 11, 11, 2, 2, 2, 2, 2, 2, 
      0, 1, 2, 4, 5, 0, 1, 2, 3, 4, 5, 
      1, 2, 3, 5, 6, 1, 2, 3, 4, 5, 6, 
      100, 50, 20, 5, 2, 100, 80, 30, 8, 15, 4, 
      100, 10, 5, 1, 1, 90, 40, 5, 1, 12, 1, 
      0, 100, 0, 0, 0, 0, 80, 20, 0, 0, 0,
      0, 0, 100, 0, 0, NA, NA, NA, NA, NA, NA), 
    dim = c(11L, 7L), 
    dimnames = list(c("", "", "", "", "", "", "", "", "", "", ""),
                    c("individual", "intStart", "intEnd", "bone1", "bone2", "tooth1", "tooth2"))
  )
  
  # test zero uncertainty
  testRenewalRatesUnc <- setVarsForUncMatrix(testRenewalRates, 
                                             timeVars = c("intStart", "intEnd"), 
                                             indVar = "individual",
                                             renewalRatesUnc = NULL)
  
  expect_equal(
    testRenewalRatesUnc,
    structure(
      c(1, 1, 1, 11, 11, 2, 2, 2, 2, 2, 2, 
        0, 1, 2, 4, 5, 0, 1, 2, 3, 4, 5,
        1, 2, 3, 5, 6, 1, 2, 3, 4, 5, 6,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
      dim = c(11L, 7L), 
      dimnames = list(c("", "", "", "", "", "", "", "", "", "", ""), 
                      c("individual", "intStart", "intEnd", "bone1", "bone2", "tooth1", "tooth2")))
  )
  
  splittedData <- cleanAndSplitData(indVar = "individual",
                                    renewalRates = testRenewalRates, 
                                    renewalRatesUnc = testRenewalRatesUnc)
  
  testthat::expect_equal(
    splittedData$renewalRatesPerInd,
    list(
      `1` = structure(
        list(
          individual = c(1, 1, 1),
          intStart = c(0,
                       1, 2),
          intEnd = c(1, 2, 3),
          bone1 = c(100, 50, 20),
          bone2 = c(100,
                    10, 5),
          tooth1 = c(0, 100, 0),
          tooth2 = c(0, 0, 100)
        ),
        class = "data.frame",
        row.names = c("X",
                      "X.1", "X.2")
      ),
      `11` = structure(
        list(
          individual = c(11,
                         11),
          intStart = c(4, 5),
          intEnd = c(5, 6),
          bone1 = c(5, 2),
          bone2 = c(1,
                    1),
          tooth1 = c(0, 0),
          tooth2 = c(0, 0)
        ),
        class = "data.frame",
        row.names = c("X.3",
                      "X.4")
      ),
      `2` = structure(
        list(
          individual = c(2, 2, 2,
                         2, 2, 2),
          intStart = c(0, 1, 2, 3, 4, 5),
          intEnd = c(1, 2, 3,
                     4, 5, 6),
          bone1 = c(100, 80, 30, 8, 15, 4),
          bone2 = c(90, 40,
                    5, 1, 12, 1),
          tooth1 = c(0, 80, 20, 0, 0, 0)
        ),
        class = "data.frame",
        row.names = c("X.5",
                      "X.6", "X.7", "X.8", "X.9", "X.10")
      )
    )
  )
  
  testthat::expect_equal(
    splittedData$renewalRatesUncPerInd,
    list(
      `1` = structure(
        list(
          individual = c(1, 1, 1),
          intStart = c(0,
                       1, 2),
          intEnd = c(1, 2, 3),
          bone1 = c(0, 0, 0),
          bone2 = c(0,
                    0, 0),
          tooth1 = c(0, 0, 0),
          tooth2 = c(0, 0, 0)
        ),
        class = "data.frame",
        row.names = c("X",
                      "X.1", "X.2")
      ),
      `11` = structure(
        list(
          individual = c(11,
                         11),
          intStart = c(4, 5),
          intEnd = c(5, 6),
          bone1 = c(0, 0),
          bone2 = c(0,
                    0),
          tooth1 = c(0, 0),
          tooth2 = c(0, 0)
        ),
        class = "data.frame",
        row.names = c("X.3",
                      "X.4")
      ),
      `2` = structure(
        list(
          individual = c(2, 2, 2,
                         2, 2, 2),
          intStart = c(0, 1, 2, 3, 4, 5),
          intEnd = c(1, 2, 3,
                     4, 5, 6),
          bone1 = c(0, 0, 0, 0, 0, 0),
          bone2 = c(0, 0, 0, 0,
                    0, 0),
          tooth1 = c(0, 0, 0, 0, 0, 0)
        ),
        class = "data.frame",
        row.names = c("X.5",
                      "X.6", "X.7", "X.8", "X.9", "X.10")
      )
    )
  )
})


testthat::test_that("with rownames: setVarsForUncMatrix and cleanAndSplitData",  {
  # test example data with rownames
  testRenewalRates <- structure(
    c(0, 1, 2, 4, 5, 0, 1, 2, 3, 4, 5, 
      1, 2, 3, 5, 6, 1, 2, 3, 4, 5, 6, 
      100, 50, 20, 5, 2, 100, 80, 30, 8, 15, 4, 
      100, 10, 5, 1, 1, 90, 40, 5, 1, 12, 1, 
      0, 100, 0, 0, 0, 0, 80, 20, 0, 0, 0,
      0, 0, 100, 0, 0, NA, NA, NA, NA, NA, NA), 
    dim = c(11L, 6L), 
    dimnames = list(as.character(c(1, 1, 1, 11, 11, 2, 2, 2, 2, 2, 2)),
                    c("intStart", "intEnd", "bone1", "bone2", "tooth1", "tooth2"))
  )
  
  testIndVar <- ""
  attr(testIndVar, "useRownames") <- TRUE
  
  testRenewalRatesUnc <- setVarsForUncMatrix(testRenewalRates, 
                                             timeVars = c("intStart", "intEnd"), 
                                             indVar = testIndVar)
  
  expect_equal(
    testRenewalRatesUnc,
    structure(
      c(0, 1, 2, 4, 5, 0, 1, 2, 3, 4, 5,
        1, 2, 3, 5, 6, 1, 2, 3, 4, 5, 6,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
      dim = c(11L, 6L), 
      dimnames = list(as.character(c(1, 1, 1, 11, 11, 2, 2, 2, 2, 2, 2)), 
                      c("intStart", "intEnd", "bone1", "bone2", "tooth1", "tooth2")))
  )
  
  
  splittedData <- cleanAndSplitData(indVar = testIndVar,
                                    renewalRates = testRenewalRates, 
                                    renewalRatesUnc = testRenewalRatesUnc)
  
  testthat::expect_equal(
    splittedData$renewalRatesPerInd,
    list(
      `1` = structure(
        list(
          intStart = c(0, 1, 2),
          intEnd = c(1, 2, 3),
          bone1 = c(100, 50, 20),
          bone2 = c(100, 10, 5),
          tooth1 = c(0, 100, 0),
          tooth2 = c(0, 0, 100)
        ),
        class = "data.frame",
        row.names = c("X1", "X1.1", "X1.2")
      ),
      `11` = structure(
        list(
          intStart = c(4, 5),
          intEnd = c(5, 6),
          bone1 = c(5, 2),
          bone2 = c(1, 1),
          tooth1 = c(0, 0),
          tooth2 = c(0, 0)
        ),
        class = "data.frame",
        row.names = c("X11",
                      "X11.1")
      ),
      `2` = structure(
        list(
          intStart = c(0, 1, 2, 3, 4, 5),
          intEnd = c(1, 2, 3, 4, 5, 6),
          bone1 = c(100, 80, 30, 8, 15, 4),
          bone2 = c(90, 40, 5, 1, 12, 1),
          tooth1 = c(0, 80, 20, 0, 0, 0)
        ),
        class = "data.frame",
        row.names = c("X2", "X2.1", "X2.2", "X2.3", "X2.4", "X2.5")
      )
    )
  )
  
  testthat::expect_equal(
    splittedData$renewalRatesUncPerInd,
    list(
      `1` = structure(
        list(
          intStart = c(0, 1, 2),
          intEnd = c(1, 2, 3),
          bone1 = c(0, 0, 0),
          bone2 = c(0, 0, 0),
          tooth1 = c(0, 0, 0),
          tooth2 = c(0, 0, 0)
        ),
        class = "data.frame",
        row.names = c("X1", "X1.1", "X1.2")
      ),
      `11` = structure(
        list(
          intStart = c(4, 5),
          intEnd = c(5, 6),
          bone1 = c(0, 0),
          bone2 = c(0, 0),
          tooth1 = c(0, 0),
          tooth2 = c(0, 0)
        ),
        class = "data.frame",
        row.names = c("X11", "X11.1")
      ),
      `2` = structure(
        list(
          intStart = c(0, 1, 2, 3, 4, 5),
          intEnd = c(1, 2, 3, 4, 5, 6),
          bone1 = c(0, 0, 0, 0, 0, 0),
          bone2 = c(0, 0, 0, 0, 0, 0),
          tooth1 = c(0, 0, 0, 0, 0, 0)
        ),
        class = "data.frame",
        row.names = c("X2", "X2.1", "X2.2", "X2.3", "X2.4", "X2.5")
      )
    )
  )
  
})


testthat::test_that("cleanAndSplitData with missing values",  {
  # test example data with missing values
  testRenewalRates <- structure(
    c(NA, 1, 1, 11, 11, 2, 2, 2, 2, 2, 2, 
      NA, 1, 2, 4, 5, 0, 1, 2, 3, 4, 5, 
      NA, 2, 3, 5, 6, 1, 2, 3, 4, 5, 6, 
      NA, 50, 20, 5, 2, 100, 80, 30, 8, 15, 4, 
      NA, 10, 5, 1, 1, 90, 40, 5, 1, 12, 1, 
      NA, 100, 0, 0, 0, 0, 80, 20, 0, 0, 0,
      NA, 0, 100, 0, 0, NA, NA, NA, NA, NA, NA), 
    dim = c(11L, 7L), 
    dimnames = list(c("", "", "", "", "", "", "", "", "", "", ""),
                    c("individual", "intStart", "intEnd", "bone1", "bone2", "tooth1", "tooth2"))
  )
  
  # test zero uncertainty
  testRenewalRatesUnc <- structure(
    c(1, 1, 1, 11, 11, 2, 2, 2, 2, 2, 2, 
      0, 1, 2, 4, 5, 0, 1, 2, 3, 4, 5,
      1, 2, 3, 5, 6, 1, 2, 3, 4, 5, 6,
      0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, NA, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
    dim = c(11L, 7L), 
    dimnames = list(c("", "", "", "", "", "", "", "", "", "", ""), 
                    c("individual", "intStart", "intEnd", "bone1", "bone2", "tooth1", "tooth2")))
  
  splittedData <- cleanAndSplitData(indVar = "individual",
                                    renewalRates = testRenewalRates, 
                                    renewalRatesUnc = testRenewalRatesUnc)
  testthat::expect_equal(
    splittedData$renewalRatesPerInd,
    list(
      `1` = structure(
        list(
          individual = c(1, 1),
          intStart = c(1,
                       2),
          intEnd = c(2, 3),
          bone1 = c(50, 20),
          bone2 = c(10, 5),
          tooth1 = c(100,
                     0),
          tooth2 = c(0, 100)
        ),
        class = "data.frame",
        row.names = c("X.1",
                      "X.2")
      ),
      `11` = structure(
        list(
          individual = c(11,
                         11),
          intStart = c(4, 5),
          intEnd = c(5, 6),
          bone1 = c(5, 2),
          bone2 = c(1,
                    1),
          tooth1 = c(0, 0),
          tooth2 = c(0, 0)
        ),
        class = "data.frame",
        row.names = c("X.3",
                      "X.4")
      ),
      `2` = structure(
        list(
          individual = c(2, 2, 2, 2, 2, 2),
          intStart = c(0, 1, 2, 3, 4, 5),
          intEnd = c(1, 2, 3, 4, 5,
                     6),
          bone1 = c(100, 80, 30, 8, 15, 4),
          bone2 = c(90, 40, 5, 1,
                    12, 1),
          tooth1 = c(0, 80, 20, 0, 0, 0)
        ),
        class = "data.frame",
        row.names = c("X.5",
                      "X.6", "X.7", "X.8", "X.9", "X.10")
      )
    )
  )
  
  testthat::expect_equal(
    splittedData$renewalRatesUncPerInd,
    list(
      `1` = structure(
        list(
          individual = c(1, 1, 1),
          intStart = c(0,
                       1, 2),
          intEnd = c(1, 2, 3),
          bone1 = c(0, 0, 0),
          bone2 = c(0,
                    0, 0),
          tooth1 = c(0, 0, 0),
          tooth2 = c(0, 0, 0)
        ),
        class = "data.frame",
        row.names = c("X",
                      "X.1", "X.2")
      ),
      `11` = structure(
        list(
          individual = c(11, 11),
          intStart = c(4, 5),
          intEnd = c(5,
                     6),
          bone1 = c(0, 0),
          bone2 = c(0, 0),
          tooth1 = c(0, 0),
          tooth2 = c(0,
                     0)
        ),
        row.names = c("X.3", "X.4"),
        class = "data.frame"
      ),
      `2` = structure(
        list(
          individual = c(2, 2, 2,
                         2, 2, 2),
          intStart = c(0, 1, 2, 3, 4, 5),
          intEnd = c(1, 2, 3,
                     4, 5, 6),
          bone1 = c(0, 0, 0, 0, 0, 0),
          bone2 = c(0, 0, 0, 0,
                    0, 0),
          tooth1 = c(0, 0, 0, 0, 0, 0)
        ),
        row.names = c("X.5", "X.6",
                      "X.7", "X.8", "X.9", "X.10"),
        class = "data.frame"
      )
    )
  )
})
