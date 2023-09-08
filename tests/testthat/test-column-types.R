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
