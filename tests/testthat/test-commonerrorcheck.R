testthat::test_that("variance checks handle factor variables", {
  dataset <- data.frame(x = factor(c("a", "b", "a")))

  result <- jaspBase:::.checkVariance(dataset, target = "x", equalTo = 0)

  testthat::expect_false(result$error)
  testthat::expect_null(result$errorVars)
})
