#' Yeo-Johnson transformation
#'
#' Transform a variable using the Yeo-Johnson transformation that extends the Box-Cox transform to unbounded variables.
#' It is possible to select the value of \code{lambda} automatically by maximizing the normal profile log likelihood.
#'
#' @param x a numeric vector to be transformed.
#' @param lambda The lambda parameter of the transform.
#' @param lower Lower limit for possible lambda values.
#' @param upper Upper limit for possible lambda values.
#' @return
#' \code{YeoJohnson} and \code{YeoJohndonAuto} return the transformed variable.
#' \code{YeoJohnsonLambda} returns the optimal value of \code{lambda} for the given data.
#'
#'
#' @references
#' Yeo, I. K., & Johnson, R. A. (2000). A new family of power transformations to improve normality or symmetry. \emph{Biometrika, 87}(4), 954-959.
#'
#' @name YeoJohnson
NULL

#' @rdname YeoJohnson
#' @export
YeoJohnson <- function(x, lambda) {
  stopifnot(length(lambda) == 1L)
  out <- numeric(length = length(x))

  pos <- !is.na(x) & x >= 0
  neg <- !is.na(x) & x < 0

  if (lambda == 0) {
    out[pos] <- log1p(x[pos])
  } else {
    out[pos] <- ((x[pos] + 1)^lambda - 1) / lambda
  }

  if (lambda == 2) {
    out[neg] <- -log1p(-x[neg])
  } else {
    num <- (-x[neg]+1)^(2-lambda) - 1
    den <- 2 - lambda
    out[neg] <- - num / den
  }

  out[is.na(x)] <- NA

  return(out)
}

#' @rdname YeoJohnson
#' @export
YeoJohnsonAuto <- function(x, lower=-5, upper=5) {
  lambda <- YeoJohnsonLambda(x, lower, upper)
  y <- YeoJohnson(x, lambda)
  attr(y, "lambda") <- lambda
  return(y)
}

#' @rdname YeoJohnson
#' @export
YeoJohnsonLambda <- function(x, lower=-5, upper=5) {
  stopifnot(lower < upper)
  lambda <- optimize(
    .yeoJohnsonLogLik,
    interval = c(lower, upper), x = x, maximum = TRUE
  )[["maximum"]]

  return(lambda)
}

.yeoJohnsonLogLik <- function(lambda, x) {
  y <- YeoJohnson(x, lambda)
  m <- mean(y)
  s2 <- mean((y - m)^2) # MLE variance: divisor n (as in paper)

  logLikNorm <- sum(stats::dnorm(y, mean = m, sd = sqrt(s2), log = TRUE))
  logDetJac <- (lambda - 1) * sum(sign(x) * log1p(abs(x)))

  return(logLikNorm + logDetJac)
}
