testthat::test_that("detectBreakPoints",  {
  # load test data
  df <- testthat::test_path("testdata/test_detectBreakPoints.csv") %>%
    read.csv()
  
  segmentsMatrix <- matrix(c(
    "d15N ~ 1 + time", "d15N ~ 1 ~ 0 + time", "d15N ~ 1 ~ 0 + time", "d15N ~ 1 ~ 0 + time", 
    "d15N ~ 1 + time", "", "", "",
    "", "", "", ""
  ), nrow = 3, ncol = 4, byrow = TRUE)
  
  priorsMatrix <- matrix(c(
    "time_1 = dunif(-4, -0.5);", "", "", "",
    "", "", "", "",
    "", "", "", ""
  ), nrow = 3, ncol = 4, byrow = TRUE)
  
  lists <- getComb(segments = segmentsMatrix, priors = priorsMatrix) %>%
    cleanComb() %>%
    splitComb() %>%
    setFormulasAndPriors()
  
  res <- runMcp(lists = lists, data = df)
  
  testthat::expect_equal(
    compareWithLoo(res) %>% colnames() %>% suppressWarnings(), 
    c("elpd_diff", "se_diff", "elpd_loo", "se_elpd_loo", "p_loo", "se_p_loo", "looic", "se_looic")
  )
})

testthat::test_that("getCellChoices",  {
  testthat::expect_equal(
    getCellChoices(nrow = 3, ncol = 2), 
    structure(1:6, names = c("(1, 1)", "(2, 1)", "(3, 1)", "(1, 2)", "(2, 2)", "(3, 2)"))
  )
})
