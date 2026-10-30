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

  for (checkColumns in list(NULL, NA, c(TRUE, FALSE), 1, "TRUE", matrix(TRUE)))
    testthat::expect_error(columnFormula("x", checkColumns = checkColumns), "`checkColumns`", fixed = TRUE)
})

testthat::test_that("errors name the offending argument and suggest a fix", {
  testthat::expect_error(columnFormula(NULL),
                         "Argument 1 in `...` is NULL, but column names must be text.", fixed = TRUE)
  testthat::expect_error(columnFormula(NULL),
                         "`data$Name` is NULL when the column does not exist.", fixed = TRUE)
  testthat::expect_error(columnFormula("x", NULL),
                         "Argument 2 in `...` is NULL", fixed = TRUE)

  testthat::expect_error(columnFormula(factor("x")),
                         "Argument 1 in `...` must be a character vector of column names, but it is a factor.",
                         fixed = TRUE)
  testthat::expect_error(columnFormula(factor("x")),
                         'as.character(data[["LL.nominal"]])', fixed = TRUE)

  testthat::expect_error(columnFormula(1),
                         "Argument 1 in `...` must be a character vector of column names, but it is the number 1.",
                         fixed = TRUE)
  testthat::expect_error(columnFormula(1:3),
                         "If these are column positions, use names(data)[...]", fixed = TRUE)

  testthat::expect_error(columnFormula(~ x),
                         "but it is a formula (~", fixed = TRUE)
  testthat::expect_error(columnFormula(~ x),
                         "This is already a formula: pass it directly instead.", fixed = TRUE)

  testthat::expect_error(columnFormula(list("x")), "but it is a list.", fixed = TRUE)
  testthat::expect_error(columnFormula(list("x")),
                         "do.call(columnFormula, names)", fixed = TRUE)
  testthat::expect_error(columnFormula(list("x")),
                         "lapply(names, columnFormula)", fixed = TRUE)

  testthat::expect_error(columnFormula(c("x", NA_character_)),
                         "Element 2 of argument 1 in `...` is NA", fixed = TRUE)
  testthat::expect_error(columnFormula(c("x", NA_character_, NA_character_)),
                         "Elements 2, 3 of argument 1 in `...` are NA", fixed = TRUE)
  testthat::expect_error(columnFormula(c("x", "")),
                         'Element 2 of argument 1 in `...` is an empty string ("")', fixed = TRUE)

  testthat::expect_error(columnFormula("x", dependent = c("y1", "y2")),
                         '`dependent` must be a single column name, but it is a character vector of length 2 ("y1", "y2").',
                         fixed = TRUE)
  testthat::expect_error(columnFormula("x", dependent = factor("y")),
                         "`dependent` must be a single column name, but it is a factor.", fixed = TRUE)
  testthat::expect_error(columnFormula("x", dependent = 3),
                         "If this is a column position, use names(data)[...]", fixed = TRUE)

  testthat::expect_error(columnFormula("x", intercept = "TRUE"),
                         '`intercept` must be TRUE or FALSE, but it is the string "TRUE".', fixed = TRUE)
  testthat::expect_error(columnFormula("x", intercept = "TRUE"),
                         "To leave out the intercept use `intercept = FALSE`.", fixed = TRUE)

  testthat::expect_error(columnFormula("x", checkColumns = NA),
                         "`checkColumns` must be TRUE or FALSE, but it is the logical value NA.", fixed = TRUE)
})

# Stubs that stand in for the engine's encoder helpers so the typo check can be tested
# outside JASP. The real `.encodeColNamesStrict()` errors for anything that is not a
# column and `.suggestColumnNames()` returns close matches from ColumnEncoder.
.stubEncodeStrict <- function(columns) {
  function(name) if (name %in% columns) paste0("encoded:", name) else stop("not a column: ", name)
}

.stubSuggest <- function(columns) {
  function(name) {
    distance <- as.vector(utils::adist(tolower(name), tolower(columns)))
    keep <- distance <= 2
    columns[keep][order(distance[keep])]
  }
}

# Install a fake dataset plus the stubs into the calling environment: columnFormula()'s
# typo check looks up `data`, `.encodeColNamesStrict()` and `.suggestColumnNames()`
# starting from its caller. test_that blocks get a fresh environment, so nothing leaks.
.stubDataset <- function(columns) {
  env <- parent.frame()
  data <- data.frame(matrix(rep(1:2, length(columns)), nrow = 2), check.names = FALSE)
  names(data) <- columns
  assign("data", data, envir = env)
  assign(".encodeColNamesStrict", .stubEncodeStrict(columns), envir = env)
  assign(".suggestColumnNames", .stubSuggest(columns), envir = env)
  invisible(NULL)
}

testthat::test_that("unknown columns get a did-you-mean warning when a dataset is visible", {
  .stubDataset(c("part length", "temperature"))

  testthat::expect_warning(columnFormula("Part length"),
                           'No column named "Part length" in the dataset. Did you mean "part length"?',
                           fixed = TRUE)
  testthat::expect_warning(columnFormula("temprature"),
                           'No column named "temprature" in the dataset. Did you mean "temperature"?',
                           fixed = TRUE)

  testthat::expect_no_warning(columnFormula("part length"))
  testthat::expect_no_warning(columnFormula("part length", dependent = "temperature"))

  testthat::expect_warning(columnFormula("part length", dependent = "Temprature"),
                           'No column named "Temprature" in the dataset. Did you mean "temperature"?',
                           fixed = TRUE)
})

testthat::test_that("already-encoded names, as names(data) gives them in JASP, are not reported as missing", {
  .stubDataset(c("part length", "temperature"))
  .decodeColNamesStrict <- function(name) {
    original <- sub("^encoded:", "", name)
    if (original != name && original %in% c("part length", "temperature")) original else stop("not encoded: ", name)
  }

  testthat::expect_no_warning(columnFormula("encoded:part length"))
  testthat::expect_warning(columnFormula("encoded:nothing"),
                           'No column named "encoded:nothing" in the dataset.', fixed = TRUE)
})

testthat::test_that("each unknown column gets its own warning", {
  .stubDataset(c("part length", "temperature"))

  warningsCaught <- testthat::capture_warnings(columnFormula("Part length", "temprature"))
  testthat::expect_length(warningsCaught, 2L)
  testthat::expect_true(any(grepl('No column named "Part length"', warningsCaught, fixed = TRUE)))
  testthat::expect_true(any(grepl('No column named "temprature"', warningsCaught, fixed = TRUE)))
})

testthat::test_that("the typo check stays silent without a dataset, and checkColumns = FALSE disables it", {
  data <- NULL
  filteredData <- NULL
  testthat::expect_no_warning(columnFormula("Part length"))

  data <- data.frame()
  .encodeColNamesStrict <- .stubEncodeStrict(character(0))
  .suggestColumnNames <- .stubSuggest(character(0))
  testthat::expect_no_warning(columnFormula("Part length"))

  testthat::expect_no_warning(columnFormula("Part length", checkColumns = FALSE))
})

testthat::test_that("without the suggest helper the warning names the column but suggests nothing", {
  data <- data.frame(1:2, check.names = FALSE)
  names(data) <- "part length"
  .encodeColNamesStrict <- .stubEncodeStrict("part length")
  .suggestColumnNames <- NULL

  testthat::expect_warning(columnFormula("Part length"),
                           'No column named "Part length" in the dataset.', fixed = TRUE)
})

testthat::test_that("filteredData is checked when data is not bound", {
  data <- NULL
  filteredData <- data.frame(1:2, check.names = FALSE)
  names(filteredData) <- "part length"
  .encodeColNamesStrict <- .stubEncodeStrict("part length")
  .suggestColumnNames <- .stubSuggest("part length")

  testthat::expect_warning(columnFormula("Part length"),
                           'No column named "Part length" in the dataset. Did you mean "part length"?',
                           fixed = TRUE)
})
