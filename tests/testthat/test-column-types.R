# jaspScale ----
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
  expect_vector(asJaspScale(integer()),   jaspScale(integer()))
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

# jaspOrdinal ----
test_that("Converting jaspOrdinal to R types works", {
  x <- c(4, 6, 2, 2, 6, NA)
  values <- c(2, 6, 4)
  labels <- c("two", "six", "four")
  nom <- jaspOrdinal(x, values = values, labels = labels)

  expect_equal(as.numeric(nom),   x)
  expect_equal(as.integer(nom),   x)
  expect_equal(as.double(nom),    x)
  expect_equal(as.character(nom), labels[match(x, values)])
  expect_equal(as.factor(nom),    factor(x, levels = values, labels = labels))
  expect_equal(as.ordered(nom),   ordered(x, levels = values, labels = labels))
  expect_error(as.logical(nom),   regexp = "Can't convert `x` <jaspOrdinal> to <logical>")
})

test_that("Converting R types to jaspOrdinal works", {
  expect_vector (asJaspOrdinal(integer()),     jaspOrdinal(integer()))
  expect_error  (rnorm(10) |> asJaspOrdinal(), regexp = "Can't convert from `x` <double> to <integer> due to loss of precision.")
  expect_error  (letters   |> jaspOrdinal(),   regexp = "Can't convert `x` <character> to <integer>.")
  expect_warning(letters   |> asJaspOrdinal(), regexp = "NAs introduced by coercion")
})

# jaspNominal -----
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

test_that("Converting R types to jaspNominal works", {
  expect_vector(asJaspNominal(integer()),   jaspNominal(integer()))
  expect_vector(asJaspNominal(character()), jaspNominal(character()))
})

# auto converting ----
test_that("jasp2r works", {
  expect_vector(jaspScale()   |> jasp2r(), numeric())
  expect_vector(jaspOrdinal() |> jasp2r(), factor(ordered=TRUE))
  expect_vector(jaspNominal() |> jasp2r(), factor())
})

test_that("r2jasp works", {
  expect_vector(numeric()           |> r2jasp(), jaspScale())
  expect_vector(integer()           |> r2jasp(), jaspScale(integer()))
  expect_vector(double()            |> r2jasp(), jaspScale())
  expect_vector(logical()           |> r2jasp(), jaspNominal())
  expect_vector(factor()            |> r2jasp(), jaspNominal())
  expect_vector(character()         |> r2jasp(), newJaspText())
})

# JASP to JASP ----
test_that("from jaspScale works", {
  x <- jaspScale(rnorm(3))
  y <- jaspScale(1:3)

  expect_equal(x |> asJaspScale(), x)
  expect_equal(y |> asJaspScale(), y)

  expect_error (x |> asJaspOrdinal(), regexp = "Can't convert from `x` <double> to <integer> due to loss of precision.")
  expect_equal(y |> asJaspOrdinal(), jaspOrdinal(1:3))

  expect_equal(x |> asJaspNominal(), x |> as.double() |> jaspNominal())
  expect_equal(y |> asJaspNominal(), y |> as.integer() |> jaspNominal())
})

test_that("from jaspOrdinal works", {
  x <- c(2, 6, 4, 2, 2, 6, 4)
  values <- c(4, 6, 2)
  labels <- letters[1:3]
  ord <- jaspOrdinal(x = x, values = values, labels = labels)

  expect_equal(ord |> asJaspScale(), jaspScale(x))
  expect_equal(ord |> asJaspOrdinal(), ord)
  expect_equal(ord |> asJaspNominal(), jaspNominal(x, values, labels))
})

test_that("from jaspNominal works", {
  x <- c(2, 6, 4, 2, 2, 6, 4)
  values <- c(4, 6, 2)
  labels <- letters[1:3]
  nom <- jaspNominal(x = x, values = values, labels = labels)

  expect_equal(nom |> asJaspScale(), jaspScale(x))
  expect_equal(nom |> asJaspOrdinal(), jaspOrdinal(x, values, labels))
  expect_equal(nom |> asJaspNominal(), nom)
})

test_that("from jaspText works", {
  x <- c("a", "b", "c", "1", "2.4")
  txt <- jaspNominal(x)

  expect_warning(txt |> asJaspScale(),      regexp = "NAs introduced by coercion")
  expect_equal  (suppressWarnings(txt |> asJaspScale() |> as.double()), suppressWarnings(as.double(x)))
  expect_error  (txt |> asJaspOrdinal(),    regexp = "Can't convert `x` <jaspText> to <jaspOrdinal>.")
  expect_equal  (txt |> asJaspNominal(),    txt)
})

test_that("as.numeric2 works as expected", {
  num <- rnorm(3)
  int <- 1L:3L
  expect_vector(num |> as.numeric2(), double(),  size = 3)
  expect_vector(int |> as.numeric2(), integer(), size = 3)
})
