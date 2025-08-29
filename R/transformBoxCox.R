#' Box-Cox transformation
#'
#' Transform a variable using the two-parameter Box-Cox transformation.
#' It is possible to select the value of \code{lambda} automatically using one of two available methods.
#'
#' @param x a numeric vector.
#' @param lambda The lambda parameter of the transform.
#' @param shift The shift parameter of the transform. \code{shift=0} by default. All values of \code{x} must be larger than \code{shift}.
#' @param method Choose method to be used in calculating lambda. See details for explanation.
#' @param lower Lower limit for possible lambda values.
#' @param upper Upper limit for possible lambda values.
#' @return
#' \code{BoxCox} and \code{BoxCoxAuto} return the transformed variable. \code{invBoxCox} returns the untransformed variable.
#' \code{BoxCoxLambda} returns the optimal value of \code{lambda} for the given data.
#'
#'
#' @details
#'
#' If \code{method=="loglik"}, the value of lambda is chosen to maximize the
#' normal profile log likelihood of the transformed variable.
#'
#'
#' If \code{method=="minitab"}, the value of lambda is chosen to minimize
#' the sample standard deviation of the standardized, Box-Cox transformed variable.
#' This behavior mimics the auto Box-Cox transform available in the Minitab software.
#'
#'
#' @references Box, G. E. P. and Cox, D. R. (1964) An analysis of
#' transformations. \emph{JRSS B} \bold{26} 211--246.
#'
#' https://support.minitab.com/en-us/minitab/help-and-how-to/quality-and-process-improvement/control-charts/how-to/box-cox-transformation/methods-and-formulas/methods-and-formulas/
#'
#' @name BoxCox
NULL

#' @rdname BoxCox
#' @export
BoxCox <- function(x, lambda, shift=0) {
  x <- x + shift
  x[x<=0] <- NA
  if (any(x <= 0, na.rm = TRUE)) warning("Nonpositive values after shift set to NA.")

  if (lambda == 0) {
    result <- log(x)
  } else {
    result <- (x^lambda - 1)/lambda
  }

  return(result)
}

#' @rdname BoxCox
#' @export
invBoxCox <- function(x, lambda, shift=0) {
  if(lambda == 0) {
    result <- exp(x)
  } else {
    x <- x * lambda + 1
    result <- x^(1/lambda)
  }

  result <- result - shift

  return(result)
}

#' @rdname BoxCox
#' @export
BoxCoxAuto <- function(x, method=c("loglik", "minitab"), lower=-5, upper=5, shift=0) {
  lambda <- BoxCoxLambda(x, method, lower, upper, shift)
  y <- BoxCox(x, lambda, shift)
  attr(y, "lambda") <- lambda
  return(y)
}

#' @rdname BoxCox
#' @export
BoxCoxLambda <- function(x, method=c("loglik", "minitab"), lower=-5, upper=5, shift=0) {
  method <- match.arg(method)

  fn <- switch (method,
    loglik = .boxCoxLambdaLogLik,
    minitab = .boxCoxLambdaMinitab
  )

  return(fn(x, lower, upper, shift))
}

.boxCoxLambdaLogLik <- function(x, lower, upper, shift) {
  lambda <- optimise(
    .boxCoxLogLik,
    interval = c(lower, upper), x = x, shift = shift, maximum = TRUE
  )[["maximum"]]

  return(lambda)
}

.boxCoxLogLik <- function(lambda, x, shift = 0) {
  y  <- BoxCox(x, lambda, shift)
  m  <- mean(y, na.rm=TRUE)
  s2 <- mean((y - m)^2, na.rm=TRUE) # MLE variance (divisor n)

  logLikNorm <- sum(stats::dnorm(y, mean = m, sd = sqrt(s2), log = TRUE), na.rm=TRUE)

  z  <- x + 1
  z  <- z[is.finite(z) & z > 0]
  logDetJac <- (lambda - 1) * sum(log(z), na.rm=TRUE)

  return(logLikNorm + logDetJac)
}

.boxCoxLambdaMinitab <- function(x, lower, upper, shift) {
  lambda <- optimise(
    .boxCoxSd,
    interval = c(lower, upper), x = x, shift = shift, maximum = FALSE
    )[["minimum"]]

  return(lambda)
}

.boxCoxSd <- function(lambda, x, shift) {
  # for the sd to be comparable across lambdas,
  # calculated for the scale-invariant version
  z <- powerTransform(x, lambda, shift)
  return(sd(z, na.rm = TRUE))
}
