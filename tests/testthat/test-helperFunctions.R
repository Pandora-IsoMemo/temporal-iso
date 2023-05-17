testthat::test_that("cleanAndSplitData",  {
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
  
  expect_equal(
    setVarsForUncMatrix(testRenewalRates, 
                        timeVars = c("intStart", "intEnd"), 
                        indVar = "individual"),
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
})
