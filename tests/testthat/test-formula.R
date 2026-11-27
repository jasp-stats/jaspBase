testthat::test_that("a formula can give the type of a variable", {
  columnNames <- c("contNormal", "contBinom", "facFive")

  testthat::expect_no_error(formulaCheckRequirements(contNormal ~ contBinom * facFive.nominal, columnNames))
  testthat::expect_no_error(formulaCheckRequirements(contNormal.scale ~ contBinom.ordinal + facFive.nominal, columnNames))
  testthat::expect_no_error(formulaCheckRequirements(~ facFive.nominal, columnNames))
})

testthat::test_that("a formula with a type still needs an existing variable and a known type", {
  columnNames <- c("contNormal", "contBinom", "facFive")

  testthat::expect_error(formulaCheckRequirements(contNormal ~ facSix.nominal, columnNames), "Cannot parse the formula")
  testthat::expect_error(formulaCheckRequirements(contNormal ~ facFive.factor, columnNames), "Cannot parse the formula")
  testthat::expect_error(formulaCheckRequirements(contNormal ~ log(facFive), columnNames), "Cannot parse the formula")
})

testthat::test_that("a column whose name ends with a type is still found", {
  testthat::expect_identical(formulaRemoveTypes(c("x.scale", "y.nominal"), c("x.scale", "y")), c("x.scale", "y"))
})
