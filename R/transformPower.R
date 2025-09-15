#' Power transformation transformation
#'
#' Transform a variable using the power transformation (scale-invariant Box-Cox).
#' It is possible to select the value of \code{lambda} automatically by maximizing the normal profile log likelihood.
#'
#' @param x a numeric vector to be transformed.
#' @param lambda The lambda parameter of the transform.
#' @param shift The shift parameter of the transform. \code{shift=0} by default. All values of \code{x} must be larger than \code{shift}.
#' @param lower Lower limit for possible lambda values.
#' @param upper Upper limit for possible lambda values.
#' @return
#' \code{powerTransform} and \code{powerTransformAuto} return the transformed variable.
#' \code{powerTransformLambda} returns the optimal value of \code{lambda} for the given data.
#'
#' @details
#'
#' The power transform is defined as
#' \deqn{y_i =
#' \begin{cases}
#' \frac{(x_i+\text{shift})^\lambda - 1}{\lambda \mathrm{GM}(\mathbb{x+\text{shift}})^{\lambda-1}} & \mathrm{if } \lambda \neq 0 \\
#' \mathrm{GM}(\mathbb{x+\text{shift}})\log(x_i+\text{shift}) & \mathrm{if } \lambda = 0,
#' \end{cases}
#' }
#' where \eqn{\mathrm{GM}(\mathbb{x})} is the geometric mean of observations \eqn{x = x_1, \dots, x_n}.
#'
#'
#' @references
#' Box, G. E. P. and Cox, D. R. (1964) An analysis of transformations. \emph{JRSS B} \bold{26} 211--246.
#' @name powerTransform
NULL

#' @rdname powerTransform
#' @export
powerTransform <- function(x, lambda, shift = 0) {
  stopifnot(length(lambda) == 1L)
  x <- x + shift
  x[x<=0] <- NA
  if (any(x <= 0, na.rm = TRUE)) warning("Nonpositive values after shift set to NA.")


  gm <- exp(mean(log(x), na.rm = TRUE))
  if(lambda == 0) {
    result <- gm * log(x)
  } else {
    result <- (x^lambda - 1) / (lambda*gm^(lambda-1))
  }

  return(result)
}

#' @rdname powerTransform
#' @export
powerTransformAuto <- function(x, lower=-5, upper=5, shift=0) {
  lambda <- powerTransformLambda(x, lower, upper, shift)
  y <- powerTransform(x, lambda, shift)
  attr(y, "lambda") <- lambda
  return(y)
}

#' @rdname powerTransform
#' @export
powerTransformLambda <- function(x, lower=-5, upper=5, shift=0) {
  stopifnot(lower < upper)
  lambda <- optimize(
    .powerTransformLogLik,
    interval = c(lower, upper), x = x, shift = shift, maximum = TRUE
  )[["maximum"]]

  return(lambda)
}

.powerTransformLogLik <- function(lambda, x, shift) {
  y <- powerTransform(x, lambda, shift)
  m <- mean(y, na.rm=TRUE)
  s2 <- mean((y - m)^2, na.rm=TRUE) # MLE variance: divisor n

  logLikNorm <- sum(stats::dnorm(y, mean = m, sd = sqrt(s2), log = TRUE), na.rm=TRUE)
  # jacobian cancels due to the GM scaling

  return(logLikNorm)
}
