test_that("Converting jaspScale to R types works", {
  x <- rnorm(10)
  z <- jaspScale(x)

  expect_equal(as.numeric(z),   x              )
  expect_equal(as.integer(z),   as.integer(x)  )
  expect_equal(as.double(z),    as.double(x)   )
  expect_equal(as.character(z), as.character(x))
  expect_equal(as.factor(z),    as.factor(x)   )
  expect_equal(as.ordered(z),   as.ordered(x)  )
  expect_equal(as.logical(z),   as.logical(x)  )
})

test_that("Converting R types to jaspScale works", {
  expect_vector(asJaspScale(rnorm(10)),   jaspScale(rnorm(10)))
  expect_vector(asJaspScale(integer()),   jaspScale())
  expect_vector(asJaspScale(character()), jaspScale())

  # these types should coerce to jaspScale the same way as to a double
  types <- list(
    numeric = rnorm(10),
    integer = 1L:10L,
    character = c("a", "b", 1, 2.3, NA),
    logical = c(TRUE, FALSE, NA)
  )

  for (x in types)
    expect_identical(
      suppressWarnings(x |> asJaspScale() |> as.double()),
      suppressWarnings(x |> as.double())
    )

  # these types should convert differently to jaspScale
  types <- list(
    factor = factor(c("a", "b", 1, 2)),
    ordered = ordered(c("a", "b", 1, 2))
  )

  for (x in types)
    expect_identical(
      suppressWarnings(x |> asJaspScale() |> as.double()),
      suppressWarnings(x |> as.character() |> as.double())
    )
})


test_that("Converting jaspNominal to R types works", {
  x <- c(4, 6, 2, 2, 6, NA)
  values <- c(2, 6, 4)
  labels <- c("two", "six", "four")
  nom <- jaspNominal(x, values = values, labels = labels)

  expect_equal(as.numeric(nom),   x)
  expect_equal(as.integer(nom),   x)
  expect_equal(as.double(nom),    x)
  expect_equal(as.character(nom), labels[match(x, values)])
  expect_equal(as.factor(nom),    factor(x, levels = values, labels = labels))
  expect_equal(as.ordered(nom),   ordered(x, levels = values, labels = labels))
  expect_error(as.logical(nom),   regexp = "Can't convert `x` <jaspNominal> to <logical>")
})

test_that("Converting R types to jaspScale works", {
  expect_vector(asJaspNominal(integer()),   jaspNominal(integer()))
  expect_vector(asJaspNominal(character()), jaspNominal(character()))
})


test_that("jasp2r works", {
  expect_vector(jaspScale()   |> jasp2r(), numeric())
  expect_vector(jaspOrdinal() |> jasp2r(), factor(ordered=TRUE))
  expect_vector(jaspNominal() |> jasp2r(), factor())
})

test_that("r2jasp works", {
  expect_vector(numeric()           |> r2jasp(), jaspScale())
  expect_vector(integer()           |> r2jasp(), jaspScale())
  expect_vector(double()            |> r2jasp(), jaspScale())
  expect_vector(logical()           |> r2jasp(), jaspNominal())
  expect_vector(factor()            |> r2jasp(), jaspNominal())
  expect_vector(character()         |> r2jasp(), newJaspText())
})
