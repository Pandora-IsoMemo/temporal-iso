testthat::test_that("detectBreakPoints",  {
  # load test data
  df <- testthat::test_path("testdata/test_detectBreakPoints.csv") %>%
    read.csv()
  
  res <- runMcp(lists = setFormulasAndPriors(), 
                data = df)
  
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
