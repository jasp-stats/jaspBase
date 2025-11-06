testthat::test_that("fisher Z works", {
  r <- seq(-1, 1, by = 0.1)
  testthat::expect_equal(
    object = fishZ(r) |> invFishZ(),
    expected = r)
})

testthat::test_that("Power transforms work", {
  x <- seq(-1, 10, by = 1)
  testthat::expect_equal(
    object = BoxCox(x, lambda=2, shift=2) |> invBoxCox(lambda=2, shift=2),
    expected = x)

  testthat::expect_equal(
    object = BoxCox(x, lambda=2.5, shift=2, continuityAdjustment = FALSE) |>
      invBoxCox(lambda=2.5, shift=2, continuityAdjustment = FALSE),
    expected = x)

  # test some special cases
  testthat::expect_equal(
    object = BoxCox(x, lambda=0, shift = 2),
    expected = log(x+2)
  )

  testthat::expect_equal(
    object = BoxCox(x, lambda=0.5, shift=3, continuityAdjustment = FALSE),
    expected = sqrt(x+3)
  )

  testthat::expect_equal(
    object = BoxCox(x, lambda=-1, shift=2, continuityAdjustment = FALSE),
    expected = 1/(x+2)
  )

  testthat::expect_equal(
    object = BoxCox(x, lambda=2, shift=1.5, continuityAdjustment = FALSE),
    expected = (x+1.5)^2
  )


  # first element will be set to NA with a warning because it is on the boundary of admissible values
  testthat::expect_warning(y <- powerTransform(x, lambda=0.5, shift=1))
  testthat::expect_equal(
    object = y,
    expected = c(NA, 0, 1.83552958322202, 3.24398096966205, 4.43136041395163,
                 5.47746270444583, 6.42321146659831, 7.29291721106063, 8.10241958039567,
                 8.86272082790327, 9.58183162724212, 10.2657993899599))

  testthat::expect_equal(
    object = YeoJohnson(x, lambda = 1.32),
    expected = c(-0.885499639969477, 0, 1.13383416500244, 2.47259978867969,
                 3.96463381612424, 5.5820916507262, 7.30707786783549, 9.12694190110795,
                 11.032181180755, 13.0153492011314, 15.0704252337427, 17.1924226010993)
  )
})

testthat::test_that("Logit transform works", {
  p <- seq(0, 1, by = 0.1)
  testthat::expect_equal(
    object = logit(p) |> invLogit(),
    expected = p
  )
})

testthat::test_that("Auto transforms work", {
  set.seed(26)
  x <- rlnorm(10)

  y <- BoxCoxAuto(x)
  testthat::expect_equal(
    object = y,
    expected = c(-1.30245492213035, 1.55736867304615, -0.433487839215894,
                 1.02638011585724, -0.370139173611634, 0.154543673772499, 0.132320670855039,
                 1.16498788784776, -0.0395369680851299, 0.468237164461956),
    ignore_attr = TRUE
  )

  x2 <- invBoxCox(y, lambda = attr(y, "lambda"))

  testthat::expect_equal(x, x2, ignore_attr = TRUE)

  y <- powerTransformAuto(x)
  testthat::expect_equal(
    object = y,
    expected = c(-1.33612171066568, 1.59760265131298, -0.444690332051012,
                 1.05289802739253, -0.379704438725285, 0.158537049024274, 0.13573982165449,
                 1.19508635952894, -0.0405586301453423, 0.480335687426334),
    ignore_attr = TRUE
  )

  y <- YeoJohnsonAuto(x)
  testthat::expect_equal(
    object = y,
    expected = c(0.112741826990562, 1.49552012875178, 0.486001919733086,
                 1.2393667231361, 0.518058727940955, 0.791083156504504, 0.779410737662641,
                 1.30769401107349, 0.689219154207117, 0.955195143315781),
    ignore_attr = TRUE
  )

})
