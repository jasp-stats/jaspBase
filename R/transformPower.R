#' Power transformation transformation
#'
#' Transform a variable using the power transformation (scale-invariant Box-Cox).
#' It is possible to select the value of \code{lambda} automatically.
#'
#' @param x a numeric vector.
#' @param lambda The lambda parameter of the transform.
#' @param shift The shift parameter of the transform. \code{shift=0} by default. Values of \code{x} smaller than \code{shift} will turn \code{NA}.
#' @param lower Lower limit for possible lambda values.
#' @param upper Upper limit for possible lambda values.
#' @param predictor Vector indicating regression variable (factor or numeric).
#' @param groupSize Integer indicating the group size (exclusive argument with \code{predictor}). Must be larger than 1.
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
#' Automatic transform finds lambda that minimizes the residual sums of squares of the power-transformed variable
#' regressed on the predictor (Box & Cox, 1964, p. 216).
#'
#' See [BoxCox] for `predictor` and `groupSize` arguments.
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
  if (any(x <= 0, na.rm = TRUE)) warning("Values smaller than -shift set to NA.")
  x[x<=0] <- NA


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
powerTransformAuto <- function(x, predictor=NULL, groupSize=NULL, lower=-5, upper=5, shift=0) {
  lambda <- powerTransformLambda(x, predictor, groupSize, lower, upper, shift)
  y <- powerTransform(x, lambda, shift)
  attr(y, "lambda") <- lambda
  return(y)
}

#' @rdname powerTransform
#' @export
powerTransformLambda <- function(x, predictor=NULL, groupSize=NULL, lower=-5, upper=5, shift=0) {
  stopifnot(lower < upper)
  predictor <- .getPredictorBoxCox(x, predictor, groupSize)

  lambda <- optimise(
    .powerTransformLogLik,
    interval = c(lower, upper), x = x, predictor = predictor, shift = shift, maximum = FALSE
  )[["minimum"]]

  return(lambda)
}

.powerTransformLogLik <- function(lambda, x, predictor, shift) {
  # Box & Cox (1964), p. 216
  y <- powerTransform(x, lambda, shift=shift)

  fit <- if(!is.null(predictor)) lm(y~predictor) else lm(y~1)

  return(deviance(fit))
}
