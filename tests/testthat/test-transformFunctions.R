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
    expected = c(NA, 0, 0.37389291204479, 0.6607910340708, 0.902657339133702,
                 1.11574583155834, 1.30839255432226, 1.48554949932136, 1.65044316322328,
                 1.8053146782674, 1.95179579919584, 2.09111836903338))

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
