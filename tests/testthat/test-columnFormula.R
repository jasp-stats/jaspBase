testthat::test_that("column names become one-sided or two-sided formulas", {
  formula <- columnFormula("x1", "x2", dependent = "y")
  testthat::expect_s3_class(formula, "formula")
  testthat::expect_equal(formula, y ~ x1 + x2, ignore_formula_env = TRUE)
  testthat::expect_equal(columnFormula(c("x1", "x2")), ~ x1 + x2,
                         ignore_formula_env = TRUE)
  testthat::expect_equal(columnFormula("x1", c("x2", "x3")), ~ x1 + x2 + x3,
                         ignore_formula_env = TRUE)
})

testthat::test_that("non-syntactic names remain literal through formula serialization", {
  columns <- c("HL", "3 - Average RSSI", "1-Supply Voltage", "a+b", "a`b",
               "a\\b", "log(x)", "x:y", "a\"b", "a'b", "NULL", " ")
  for (name in columns) {
    formula <- columnFormula(name, dependent = name)
    testthat::expect_identical(all.vars(formula), name)
    testthat::expect_identical(as.character(formula[[2L]]), name)
    testthat::expect_identical(as.character(formula[[3L]]), name)
    roundTrip <- stats::as.formula(paste(deparse(formula), collapse = " "))
    testthat::expect_identical(all.vars(roundTrip), name)
  }
})

testthat::test_that("formulas work with actual data and optional intercepts", {
  data <- setNames(data.frame(1:4, 5:8, 9:12), c("3 - Average RSSI", "a+b", "1-Supply Voltage"))
  formula <- columnFormula(c("3 - Average RSSI", "a+b"), dependent = "1-Supply Voltage")
  testthat::expect_equal(unname(stats::model.response(stats::model.frame(formula, data))), 9:12)
  testthat::expect_equal(unname(stats::model.matrix(formula, data)[, , drop = FALSE]),
                         unname(cbind(1, 1:4, 5:8)))
  noIntercept <- columnFormula(c("3 - Average RSSI", "a+b"), intercept = FALSE)
  testthat::expect_equal(unname(stats::model.matrix(noIntercept, data)[, , drop = FALSE]),
                         unname(cbind(1:4, 5:8)))
})

testthat::test_that("empty right-hand sides give intercept-only or zero formulas", {
  testthat::expect_equal(columnFormula(), ~ 1, ignore_formula_env = TRUE)
  testthat::expect_equal(columnFormula(character()), ~ 1, ignore_formula_env = TRUE)
  testthat::expect_equal(columnFormula(dependent = "y"), y ~ 1, ignore_formula_env = TRUE)
  testthat::expect_equal(columnFormula(intercept = FALSE), ~ 0, ignore_formula_env = TRUE)
  testthat::expect_equal(columnFormula(dependent = "y", intercept = FALSE), y ~ 0,
                         ignore_formula_env = TRUE)
})

testthat::test_that("column name expressions are evaluated in the caller", {
  measurements <- c("HL", "3 - Average RSSI", "1-Supply Voltage")
  dependent <- "result"
  formula <- columnFormula(measurements[[2L]], dependent = dependent)
  testthat::expect_identical(all.vars(formula), c(dependent, measurements[[2L]]))
  testthat::expect_identical(environment(formula), environment())
  formulas <- lapply(measurements, columnFormula)
  testthat::expect_identical(vapply(formulas, all.vars, character(1)), measurements)
})

testthat::test_that("invalid inputs produce clear errors", {
  for (columns in list(NULL, NA_character_, "", c("x", NA_character_), 1, TRUE,
                       list("x"), factor("x"), matrix("x")))
    testthat::expect_error(columnFormula(columns), "`...`", fixed = TRUE)

  for (dependent in list(character(), NA_character_, "", c("y1", "y2"), 1,
                         list("y"), matrix("y")))
    testthat::expect_error(columnFormula("x", dependent = dependent), "`dependent`", fixed = TRUE)

  for (intercept in list(NULL, NA, c(TRUE, FALSE), 1, "TRUE", matrix(TRUE)))
    testthat::expect_error(columnFormula("x", intercept = intercept), "`intercept`", fixed = TRUE)
})

testthat::test_that("a batch pairs constructed formulas with their arguments", {
  measurements <- c("HL", "3 - Average RSSI", "1-Supply Voltage")
  wrapper <- function(measurementLongFormat, toleranceValue) {
    list(module = "test", analysis = "GaugeRR",
         options = list(measurement = measurementLongFormat, tolerance = toleranceValue))
  }
  results <- batchAnalysis(wrapper,
                           measurementLongFormat = lapply(measurements, columnFormula),
                           toleranceValue = c(1, 2, 3))
  testthat::expect_length(results, 3L)
  testthat::expect_identical(vapply(results, function(x) all.vars(x$options$measurement), character(1)),
                             measurements)
  testthat::expect_identical(vapply(results, function(x) x$options$tolerance, numeric(1)), c(1, 2, 3))
})
