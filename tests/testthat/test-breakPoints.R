testthat::test_that("detectBreakPoints", {
  # load test data
  df <- testthat::test_path("testdata/test_detectBreakPoints.csv") %>%
    read.csv()
  
  segmentsMatrix <- matrix(
    c(
      "d15N ~ 1 + time",
      "d15N ~ 1 ~ 0 + time",
      "d15N ~ 1 ~ 0 + time",
      "d15N ~ 1 ~ 0 + time",
      "d15N ~ 1 + time",
      "",
      "",
      "",
      "",
      "",
      "",
      ""
    ),
    nrow = 3,
    ncol = 4,
    byrow = TRUE
  )
  
  priorsMatrix <- matrix(
    c(
      "time_1 = dunif(-4, -0.5);",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      "",
      ""
    ),
    nrow = 3,
    ncol = 4,
    byrow = TRUE
  )
  
  comb <- getComb(segments = segmentsMatrix, priors = priorsMatrix)
  
  testthat::expect_equal(comb[1, 1], "d15N ~ 1 + time*+*time_1 = dunif(-4, -0.5);")
  testthat::expect_equal(dim(comb), c(16, 4))
  testthat::expect_equal(comb %>% cleanComb() %>% dim(), c(8, 4))
  
  twoMatrices <- comb %>% cleanComb() %>% splitComb()
  
  testthat::expect_equal(twoMatrices$mat1[1, 1], "d15N ~ 1 + time")
  testthat::expect_equal(twoMatrices$mat2[1, 1], "time_1 = dunif(-4, -0.5);")
  testthat::expect_equal(twoMatrices$mat1 %>% dim(), c(8, 4))
  testthat::expect_equal(twoMatrices$mat2 %>% dim(), c(8, 4))
  
  lists <- comb %>%
    cleanComb() %>%
    splitComb() %>%
    setFormulasAndPriors()
  
  res <- runMcp(lists = lists, data = df)
  
  testthat::expect_equal(
    compareWithLoo(res) %>% colnames() %>% suppressWarnings(),
    c(
      "elpd_diff",
      "se_diff",
      "elpd_loo",
      "se_elpd_loo",
      "p_loo",
      "se_p_loo",
      "looic",
      "se_looic"
    )
  )
})

testthat::test_that("validateFormula", {
  testthat::expect_equal(validateFormula("y ~ x + 1"), "y ~ x + 1")
  testthat::expect_equal(validateFormula("~ 1 + x"), "~ 1 + x")
  testthat::expect_equal(validateFormula("~ I(x^2) + exp(x) + sin(x)"),
                         "~ I(x^2) + exp(x) + sin(x)")
  testthat::expect_equal(validateFormula("~sigma(1)"), "~sigma(1)")
  testthat::expect_equal(validateFormula("~sigma(rel(1) + I(x^2))"),
                         "~sigma(rel(1) + I(x^2))")
  testthat::expect_equal(validateFormula("~ar(1)"), "~ar(1)")
  testthat::expect_warning(validateFormula("y <- x + 1"))
})

testthat::test_that("getCellChoices", {
  testthat::expect_equal(getCellChoices(nrow = 3, ncol = 2), structure(
    1:6,
    names = c("(1, 1)", "(2, 1)", "(3, 1)", "(1, 2)", "(2, 2)", "(3, 2)")
  ))
})
