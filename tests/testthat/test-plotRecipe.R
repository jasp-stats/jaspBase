test_that("plot recipe arguments are decoded recursively", {
  seenContexts <- list()
  testthat::local_mocked_bindings(
    .decodeJaspText = function(x, decodeContext = NULL, fieldName = NULL) {
      seenContexts[[length(seenContexts) + 1L]] <<- decodeContext
      sub("^encoded_", "", x)
    },
    .package = "jaspBase"
  )
  decodeContext <- list(source = "test")

  args <- list(
    data = data.frame(
      encoded_column = factor(c("encoded_a", "encoded_b")),
      label = c("encoded_label", "encoded_other")
    ),
    nested = list(encoded_name = "encoded_value")
  )

  decoded <- jaspBase:::.decodeJaspPlotRecipeArguments(args, decodeNames = FALSE, decodeContext = decodeContext)

  expect_named(decoded, c("data", "nested"))
  expect_named(decoded$data, c("column", "label"))
  expect_equal(levels(decoded$data$column), c("a", "b"))
  expect_equal(decoded$data$label, c("label", "other"))
  expect_named(decoded$nested, "name")
  expect_equal(decoded$nested$name, "value")
  expect_true(length(seenContexts) > 0L)
  expect_true(all(vapply(seenContexts, identical, logical(1), decodeContext)))
})

test_that("plot recipe arguments reject environments", {
  expect_error(
    jaspBase:::.decodeJaspPlotRecipeArguments(list(data = new.env())),
    "cannot contain environments"
  )
})
