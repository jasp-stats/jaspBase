# This R script contains code for extracting the Box-Cox
# parameter, lambda, using Guerrero's method (1993).
# Original written by Leanne Chhay. The code was taken from the R package {forecast}, licensed under the GPL-3 license,
# and modified to include the minitab version of the transform, and make the coding style consistent.

#' Box-Cox transformation
#'
#' Transform a variable using the two-parameter Box-Cox transformation.
#' It is possible to select the value of \code{lambda} automatically
#' using one of three available methods.
#'
#' @param x a numeric vector or time series of class \code{ts}.
#' @param lambda The lambda parameter of the transform.
#' @param shift The shift parameter of the transform. \code{shift=0} by default. All values of \code{x} must be larger than \code{shift}.
#' @param method Choose method to be used in calculating lambda. See details for explanation.
#' @param lower Lower limit for possible lambda values. By default set to -5 for \code{method=="minitab"}, otherwise -1.
#' @param upper Upper limit for possible lambda values. By default set to 5 for \code{method="minitab"}, otherwise 2.
#' @return
#' \code{BoxCox} and \code{BoxCoxAuto} return the transformed variable. \code{invBoxCox} returns the untransformed variable.
#' \code{BoxCoxLambda} returns the optimal value of \code{lambda} for the given data.
#'
#' @author Leanne Chhay and Rob J Hyndman and Šimon Kucharský
#'
#'
#' @details
#' If \code{method=="guerrero"}, Guerrero's (1993) method is used, where lambda
#' minimizes the coefficient of variation for subseries of \code{x}.
#'
#' If \code{method=="loglik"}, the value of lambda is chosen to maximize the
#' profile log likelihood of a linear model fitted to \code{x}. For
#' non-seasonal data, a linear time trend is fitted while for seasonal data, a
#' linear time trend with seasonal dummy variables is used.
#'
#' If \code{method=="minitab"}, the value of lambda is chosen to minimize
#' the sample standard deviation of the standardized, Box-Cox transformed variable.
#' This behavior mimics the auto Box-Cox transform available in the Minitab software.
#'
#' @note
#' This functionality is partially taken from the \code{forecast} package, which itself derived some code from the
#' package \code{MASS}. See also package \code{DescTools} for the Box-Cox transform.
#'
#' @references Box, G. E. P. and Cox, D. R. (1964) An analysis of
#' transformations. \emph{JRSS B} \bold{26} 211--246.
#'
#' Guerrero, V.M. (1993) Time-series analysis supported by power
#' transformations. \emph{Journal of Forecasting}, \bold{12}, 37--48.
#'
#' https://support.minitab.com/en-us/minitab/help-and-how-to/quality-and-process-improvement/control-charts/how-to/box-cox-transformation/methods-and-formulas/methods-and-formulas/
#'
#' @name BoxCox
NULL

#' @rdname BoxCox
#' @export
BoxCox <- function(x, lambda, shift = 0) {
  x <- x + shift
  x[x<=0] <- NA

  if (lambda == 0) {
    result <- log(x)
  } else {
    result <- (x^lambda - 1)/lambda
  }

  return(result)
}

#' @rdname BoxCox
#' @export
invBoxCox <- function(x, lambda, shift = 0) {
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
BoxCoxAuto <- function(x, method=c("guerrero", "loglik", "minitab"), lower, upper, shift = 0) {
  x <- x + shift
  if (any(x <= 0)) stop("All values must be strictly positive after applying 'shift'.")
  lambda <- BoxCoxLambda(x, method, lower, upper)
  output <- BoxCox(x, lambda, shift=0)
  return(output)
}

#' @rdname BoxCox
#' @export
BoxCoxLambda <- function(x, method=c("guerrero", "loglik", "minitab"), lower, upper) {
  if (length(x) <= 2 * frequency(x)) {
    return(1)
  } # Not enough data to do much more than this

  method <- match.arg(method)

  if (missing(lower)) lower <- if (method == "minitab") -5 else -1
  if (missing(upper)) upper <- if (method == "minitab") 5 else 2
  if (lower <= upper) stop("`lower` must be smaller than `upper`.")

  fn <- switch (method,
    loglik = .boxCoxLambdaLogLik,
    guerrero = .boxCoxLambdaGuerrero,
    minitab = .boxCoxLambdaMinitab
  )

  return(fn(x, lower, upper))
}


# modified from the MASS package
.boxCoxLambdaLogLik <- function(x, lower, upper) {
  n <- length(x)
  logx <- log(na.omit(c(x)))
  xdot <- exp(mean(logx))
  if (!is.ts(x)) {
    fit <- lm(x ~ 1, data = data.frame(x = x), na.action = na.exclude)
  } else if (frequency(x) > 1) {
    fit <- tslm(x ~ trend + season, data = data.frame(x = x))
  } else {
    fit <- tslm(x ~ trend, data = data.frame(x = x))
  }
  xqr <- fit$qr
  lambda <- seq(lower, upper, by = .05)
  xl <- loglik <- as.vector(lambda)
  m <- length(xl)
  x <- na.omit(c(x))
  for (i in 1L:m)
  {
    if (abs(la <- xl[i]) > 0.02) {
      xt <- (x ^ la - 1) / la
    } else {
      xt <- logx * (1 + (la * logx) / 2 * (1 + (la * logx) / 3 * (1 + (la * logx) / 4)))
    }
    loglik[i] <- -n / 2 * log(sum(qr.resid(xqr, xt / xdot ^ (la - 1)) ^ 2))
  }
  return(xl[which.max(loglik)])
}

.boxCoxLambdaGuerrero <- function(x, lower, upper, nonseasonal.length=2) {
  lambda <- optimize(
    .boxCoxCoefficientOfVariation,
    interval = c(lower, upper), x = x, nonseasonal.length = nonseasonal.length, maximum = FALSE
    )[["minimum"]]

  return(lambda)
}

.boxCoxCoefficientOfVariation <- function(lam, x, nonseasonal.length=2) {
  period <- round(max(nonseasonal.length, frequency(x)))
  nobsf <- length(x)
  nyr <- floor(nobsf / period)
  nobst <- floor(nyr * period)
  x.mat <- matrix(x[(nobsf - nobst + 1):nobsf], period, nyr)
  x.mean <- colMeans(x.mat, na.rm = TRUE)
  x.sd <- apply(x.mat, 2, sd, na.rm = TRUE)
  x.rat <- x.sd / x.mean ^ (1 - lam)
  return(sd(x.rat, na.rm = TRUE) / mean(x.rat, na.rm = TRUE))
}

.boxCoxLambdaMinitab <- function(x, lower, upper) {
  lambda <- optimise(
    .boxCoxSd,
    interval = c(lower, upper), x = x, maximum=FALSE
    )[["minimum"]]

  return(lambda)
}

.boxCoxSd <- function(lambda, x) {
  z <- .standardizedBoxCox(x, lambda)
  return(sd(z, na.rm = TRUE))
}

.standardizedBoxCox <- function(x, lambda) {
  geomMean <- exp(mean(log(x)))
  if (lambda == 0) {
    z <- geomMean * log(x)
  } else {
    z <- (x^lambda - 1) / (lambda * geomMean^(lambda-1))
  }
  return(z)
}
