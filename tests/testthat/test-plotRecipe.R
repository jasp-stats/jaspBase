test_that("plot recipe arguments are decoded recursively", {
  oldDecoder <- get0(".decodeColNamesLax", envir = .GlobalEnv, inherits = FALSE)
  assign(".decodeColNamesLax", function(x) sub("^encoded_", "", x), envir = .GlobalEnv)
  on.exit({
    if (is.null(oldDecoder))
      rm(".decodeColNamesLax", envir = .GlobalEnv)
    else
      assign(".decodeColNamesLax", oldDecoder, envir = .GlobalEnv)
  })

  args <- list(
    data = data.frame(
      encoded_column = factor(c("encoded_a", "encoded_b")),
      label = c("encoded_label", "encoded_other")
    ),
    nested = list(encoded_name = "encoded_value")
  )

  decoded <- jaspBase:::.decodeJaspPlotRecipeArguments(args, decodeNames = FALSE)

  expect_named(decoded, c("data", "nested"))
  expect_named(decoded$data, c("column", "label"))
  expect_equal(levels(decoded$data$column), c("a", "b"))
  expect_equal(decoded$data$label, c("label", "other"))
  expect_named(decoded$nested, "name")
  expect_equal(decoded$nested$name, "value")
})

test_that("plot recipe arguments reject environments", {
  expect_error(
    jaspBase:::.decodeJaspPlotRecipeArguments(list(data = new.env())),
    "cannot contain environments"
  )
})
