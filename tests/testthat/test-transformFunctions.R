testthat::test_that("fisher Z works", {
  r <- seq(-1, 1, by = 0.1)
  testthat::expect_equal(
    object = fishZ(r) |> invFishZ(),
    expected = r)
})

testthat::test_that("Power transforms work", {
  x <- seq(-1, 10, by = 1)
  testthat::expect_equal(
    object = BoxCox(x, lambda = 2, shift = 2) |> invBoxCox(lambda = 2, shift = 2),
    expected = x)

  testthat::expect_equal(
    object = powerTransform(x, lambda = 0.5, shift = 1),
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
    expected = c(-0.319395836440043, 11.2605876201426, -0.250599437523637,
                 3.91717936655459, -0.231049967619291, 0.189454075774882, 0.157510562708716,
                 5.27886307986331, -0.0375454162860614, 0.869872446555355),
    ignore_attr = TRUE
  )

  x2 <- invBoxCox(y, lambda = attr(y, "lambda"))

  testthat::expect_equal(x, x2, ignore_attr = TRUE)

  y <- powerTransformAuto(x)
  testthat::expect_equal(
    object = y,
    expected = c(-1.33611036495374, 1.59761108868762, -0.444689167522521,
                 1.05290178406806, -0.37970357202767, 0.158537083208781, 0.135739837765653,
                 1.1950911816994, -0.0405586035722318, 0.480336415536656),
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
