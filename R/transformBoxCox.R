#' Box-Cox transformation
#'
#' Transform a variable using the two-parameter Box-Cox transformation.
#' It is possible to select the value of \code{lambda} automatically.
#'
#' @param x a numeric vector.
#' @param lambda The lambda parameter of the transform.
#' @param shift The shift parameter of the transform. \code{shift=0} by default. Values of \code{x} smaller than \code{shift} will turn \code{NA}.
#' @param continuityAdjustment Boolean (default \code{TRUE}) whether to apply the continuity adjustment term (see Details).
#' @param lower Lower limit for possible lambda values.
#' @param upper Upper limit for possible lambda values.
#' @param method Method for automatic determination of \code{lambda}. See details.
#' @param predictor Vector indicating regression variable (factor or numeric).
#' @param groupSize Integer indicating the group size (exclusive argument with \code{predictor}). Must be larger than 1. For individual data, use `method="movingRange"`.
#' @return
#' \code{BoxCox} and \code{BoxCoxAuto} return the transformed variable. \code{invBoxCox} returns the untransformed variable.
#' \code{BoxCoxLambda} returns the optimal value of \code{lambda} for the given data.
#'
#'
#' @details
#'
#' The two-parameter Box-Cox transformation is defined as
#' \deqn{y =
#' \begin{cases}
#' \frac{(x+\text{shift})^\lambda - 1}{\lambda} & \mathrm{if } \lambda \neq 0 \\
#' \log(x+\text{shift}) & \mathrm{if } \lambda = 0.
#' \end{cases}
#' }
#'
#' If \code{continuityAdjustment=FALSE},
#' \deqn{y =
#' \begin{cases}
#' (x+\text{shift})^\lambda & \mathrm{if } \lambda \neq 0 \\
#' \log(x+\text{shift}) & \mathrm{if } \lambda = 0
#' \end{cases}
#' }
#'
#' The continuity adjustment term ensures smooth convergence to the log transform as \eqn{\lambda \rightarrow 0}.
#'
#'
#' In \code{BoxCoxLambda} and \code{BoxCoxAuto}, the value of lambda is automatically selected using one of three possible methods:
#'
#' - When \code{method="loglik"}, lambda maximizes the normal log-likelihood of the transformed variable regressed on the predictor (Box & Cox, 1964, p. 215).
#' - When \code{method="sd"}, lambda minimizes the residual sums of squares of the power-transformed variable regressed on the predictor (see [powerTransform]; Box & Cox, 1964, p. 216).
#' - When \code{method="movingRange"}, lambda minimizes the estimate of variability based on the average moving range of the power-transformed variable (see Montgomerry, 2012).
#' Predictor is ignored in this case.
#'
#' The predictor is either taken from the predictor argument, or inferred from the `groupSize` argument.
#' If both arguments are \code{NULL}, the data is assumed to be in one group (and regressed only on an intercept term).
#'
#'
#'
#' @references Box, G. E. P. and Cox, D. R. (1964) An analysis of
#' transformations. \emph{JRSS B} \bold{26} 211--246.
#'
#' Montgomery, D. C. (2012). Statistical quality control (Vol. 4). New York: John Wiley & Sons.
#'
#' @name BoxCox
NULL

#' @rdname BoxCox
#' @export
BoxCox <- function(x, lambda, shift=0, continuityAdjustment=TRUE) {
  stopifnot(length(lambda) == 1L)
  x <- x + shift
  if (any(x <= 0, na.rm = TRUE)) warning("Values smaller than -shift set to NA.")
  x[x<=0] <- NA

  if (lambda == 0) {
    result <- log(x)
  } else {
    result <- x^lambda
    if (continuityAdjustment) result <- (result - 1) / lambda
  }

  return(result)
}

#' @rdname BoxCox
#' @export
invBoxCox <- function(x, lambda, shift=0, continuityAdjustment=TRUE) {
  if(lambda == 0) {
    result <- exp(x)
  } else {
    if (continuityAdjustment) x <- x * lambda + 1
    result <- x^(1/lambda)
  }

  result <- result - shift

  return(result)
}

#' @rdname BoxCox
#' @export
BoxCoxAuto <- function(x, predictor=NULL, groupSize=NULL, method="loglik", lower=-5, upper=5, shift=0, continuityAdjustment=TRUE) {
  lambda <- BoxCoxLambda(x, predictor, groupSize, method, lower, upper, shift, continuityAdjustment)
  y <- BoxCox(x, lambda, shift, continuityAdjustment)
  attr(y, "lambda") <- lambda
  return(y)
}

#' @rdname BoxCox
#' @export
BoxCoxLambda <- function(x, predictor=NULL, groupSize=NULL, method="loglik", lower=-5, upper=5, shift=0, continuityAdjustment=TRUE) {
  stopifnot(lower < upper)
  predictor <- .getPredictorBoxCox(x, predictor, groupSize)

  method <- match.arg(method, c("loglik", "sd", "movingRange"))

  fn <- switch(
    method,
    loglik = .boxCoxLogLik,
    sd = .boxCoxSd,
    movingRange = .boxCoxAverageMovingRange
  )

  lambda <- optimise(
    fn,
    interval = c(lower, upper), x = x, predictor = predictor, shift = shift, continuityAdjustment = continuityAdjustment, maximum = FALSE
  )[["minimum"]]

  return(lambda)
}

.boxCoxLogLik <- function(lambda, x, predictor, shift = 0, continuityAdjustment = TRUE) {
  # continuityAdjustment = TRUE is intentional (continuity ensures smooth optimization)
  y  <- BoxCox(x, lambda, shift, continuityAdjustment = TRUE)

  # Box & Cox (1964), p. 215
  fit <- if(!is.null(predictor)) lm(y~predictor) else lm(y~1)
  n <- nobs(fit)
  sigma2 <- deviance(fit) / n
  logLikSigma <- -n/2 * log(sigma2)
  logDetJac <- (lambda - 1) * sum(log(x+shift), na.rm=TRUE)

  # return negative log lik for minimization
  return(- logLikSigma - logDetJac)
}

.boxCoxSd <- function(lambda, x, predictor, shift = 0, ...) {
  # Box & Cox (1964), p. 216
  y <- powerTransform(x, lambda, shift=shift)

  fit <- if(!is.null(predictor)) lm(y~predictor) else lm(y~1)

  return(deviance(fit))
}

.boxCoxAverageMovingRange <- function(lambda, x, shift = 0, ...) {
  x <- powerTransform(x, lambda, shift = shift)

  w <- 2 # hard-coded lag=2 of the moving range

  d2 <- 1.128

  n <- length(x)
  stopifnot(n > w)

  mr <- 0
  for (i in w:n) mr <- mr+abs(diff(range(x[i:(i-w+1)], na.rm=TRUE)))

  mr <- mr/(n-w+1)

  # return estimate of sd
  return(mr / d2)
}


.getPredictorBoxCox <- function(x, predictor=NULL, groupSize=NULL) {
  if (!is.null(predictor) && !is.null(groupSize)) stop("Only one of `predictor`, `groupSize` may be specified")

  if (!is.null(predictor)) {
    stopifnot(length(x) == length(predictor))
    return(predictor)
  }

  if (is.null(groupSize)) return(NULL)
  stopifnot(length(groupSize) == 1L)
  stopifnot(groupSize > 1)

  numGroups <- length(x) %/% groupSize
  group <- rep(seq_len(numGroups), each=groupSize)

  lastGroupSize <- length(x) - length(group)
  group <- c(group, rep(numGroups+1, lastGroupSize))

  return(as.factor(group))
}
