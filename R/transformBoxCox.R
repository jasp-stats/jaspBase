#' Box-Cox transformation
#'
#' Transform a variable using the two-parameter Box-Cox transformation.
#' It is possible to select the value of \code{lambda} automatically using one of two available methods.
#'
#' @param x a numeric vector.
#' @param lambda The lambda parameter of the transform.
#' @param shift The shift parameter of the transform. \code{shift=0} by default. All values of \code{x} must be larger than \code{shift}.
#' @param lower Lower limit for possible lambda values.
#' @param upper Upper limit for possible lambda values.
#' @param group Group vector indicating groups (for minitab emulation).
#' @param size If \code{group=NULL}, the number of observations per group.
#' @return
#' \code{BoxCox} and \code{BoxCoxAuto} return the transformed variable. \code{invBoxCox} returns the untransformed variable.
#' \code{BoxCoxLambda} returns the optimal value of \code{lambda} for the given data.
#'
#'
#' @details
#'
#' In \code{BoxCoxLambda} and \code{BoxCoxAuto}, the value of lambda is chosen to maximize the
#' normal profile log likelihood of the transformed variable.
#'
#' The two-parameter Box-Cox transformation is defined as
#' \deqn{y =
#' \begin{cases}
#' \frac{(x+\text{shift})^\lambda - 1}{\lambda} & \mathrm{if } \lambda \neq 0 \\
#' \log(x+\text{shift}) & \mathrm{if } \lambda = 0.
#' \end{cases}
#' }
#'
#'
#'
#' ## Minitab emulation
#' \code{BoxCoxLambdaMinitab} and \code{BoxCoxMinitab} mimic the auto Box-Cox transform available in the Minitab software.
#' The value of lambda is chosen to minimize the pooled sample standard deviation
#' of the "standardized transformed variable" (see [powerTransform]), unless the data is based on individuals (ungrouped),
#' that is, when \code{size=1} or all values in \code{group} are unique: then lambda minimizes the
#' unbiased estimate of the average moving range.
#' The data is then transformed using a simple exponential equation (neglecting the normalizing factor of the standard Box-Cox transform)
#' \deqn{y =
#' \begin{cases}
#' x^\lambda & \mathrm{if } \lambda \neq 0 \\
#' \log(x) & \mathrm{if } \lambda = 0
#' \end{cases}
#' }
#'
#'
#' @references Box, G. E. P. and Cox, D. R. (1964) An analysis of
#' transformations. \emph{JRSS B} \bold{26} 211--246.
#'
#' \href{https://support.minitab.com/en-us/minitab/help-and-how-to/quality-and-process-improvement/control-charts/how-to/box-cox-transformation/methods-and-formulas/methods-and-formulas/}{Box-Cox transformation in Minitab}
#'
#' \href{https://support.minitab.com/en-us/minitab/help-and-how-to/quality-and-process-improvement/control-charts/how-to/variables-charts-for-subgroups/i-mr-r-s-chart/methods-and-formulas/estimating-sigma-for-the-i-chart-and-the-mr-chart/#average-moving-range-method}{Unbiased average moving range in Minitab}
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
BoxCoxAuto <- function(x, lower=-5, upper=5, shift=0) {
  lambda <- BoxCoxLambda(x, lower, upper, shift)
  y <- BoxCox(x, lambda, shift)
  attr(y, "lambda") <- lambda
  return(y)
}

#' @rdname BoxCox
#' @export
BoxCoxLambda <- function(x, lower=-5, upper=5, shift=0) {
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

# Emulate minitab ----

#' @rdname BoxCox
#' @export
BoxCoxMinitab <- function(x, group=NULL, size=NULL, lower=-5, upper=5) {
  stopifnot(all(x >= 0))

  lambda <- BoxCoxLambdaMinitab(x, group, size, lower, upper)

  y <- if (lambda != 0) x^lambda else log(x)
  attr(y, "lambda") <- lambda

  return(y)
}

#' @rdname BoxCox
#' @export
BoxCoxLambdaMinitab <- function(x, group=NULL, size=NULL,lower=-5, upper=5) {
  group <- .getGroupMinitab(x, group, size)

  ns <- .lengthGrouped(x, group)
  # if individual data, then average moving range is the loss function
  # otherwise, the pooled standard deviation
  fn <- if (all(ns ==1)) .boxCoxAverageMovingRangeMinitab else .boxCoxSdMinitab

  lambda <- optimise(fn, interval = c(lower, upper), x = x, group, maximum = FALSE)[["minimum"]]

  return(lambda)
}

.boxCoxSdMinitab <- function(lambda, x, group) {
  z <- powerTransform(x, lambda, shift=0)

  # calculate variance per group
  var <- .varGrouped(z, group)
  n  <- .lengthGrouped(z, group)

  remove <- is.na(var)
  var <- var[!remove]
  n <- n[!remove]

  # calculate pooled sd
  sd <- sqrt(
    sum((n-1)*var) / sum(n-1)
  )

  return(sd)
}

.boxCoxAverageMovingRangeMinitab <- function(lambda, x, group) {
  z <- powerTransform(x, lambda, shift=0)

  sd <- .avgMovingRange(z)

  return(sd)
}

## helper functions for minitab version -----
# get groups based on the group vector, or the group 'size'
.getGroupMinitab <- function(x, group=NULL, size=NULL) {
  if (!is.null(group)) {
    stopifnot(length(x) == length(group))
    return(group)
  }

  if (is.null(size)) {
    size <- length(x)
  }

  numGroups <- length(x) %/% size
  group <- rep(seq_len(numGroups), each=size)

  lastGroupSize <- length(x) - length(group)
  group <- c(group, rep(numGroups+1, lastGroupSize))

  return(group)
}

# get variance of observations per group
.varGrouped <- function(x, group) {
  tapply(x, group, var, na.rm = TRUE, simplify = TRUE)
}

# get number of observations per group
.lengthGrouped <- function(x, group) {
  tapply(x, group, \(x) sum(!is.na(x)), simplify = TRUE)
}

.avgMovingRange <- function(x) {
  # https://support.minitab.com/en-us/minitab/help-and-how-to/quality-and-process-improvement/control-charts/how-to/variables-charts-for-subgroups/i-mr-r-s-chart/methods-and-formulas/estimating-sigma-for-the-i-chart-and-the-mr-chart/#average-moving-range-method
  w <- 2 # hard-coded lag=2 of the moving range

  # unbiasing constant from https://support.minitab.com/en-us/minitab/help-and-how-to/quality-and-process-improvement/control-charts/how-to/variables-charts-for-subgroups/r-chart/methods-and-formulas/estimating-sigma/#unbiasing-constants-d2-d3-and-d4
  d2 <- 1.128

  n <- length(x)
  stopifnot(n > w)

  mr <- 0
  for (i in w:n) mr <- mr+abs(diff(range(x[i:(i-w+1)], na.rm=TRUE)))

  mr <- mr/(n-w+1)

  return(mr / d2)
}
