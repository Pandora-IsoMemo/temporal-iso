testthat::test_that("addPackageVersionNo", {
  testthat::expect_equal(
    substr(addPackageVersionNo("abc"), 1, 25),
    "abc\n\nOsteoBioR version 22"
  )
})
