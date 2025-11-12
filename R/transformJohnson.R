#' Johnson transformation
#'
#' Transform a variable using the Johnson transformation that transforms unbounded variables into a close-to-normal
#' distribution. It is computationally more expensive than [BoxCox], [powerTransform], or [YeoJohnson], but also more
#' expressive, allowing better normalization for more complicated empirical distributions.
#'
#' @param x a numeric vector to be transformed.
#' @param lower Lower limit for possible z values.
#' @param upper Upper limit for possible z values.
#'
#' @details
#'
#'
#' This transformation follows the algorithm described in Chou, et al. (1998), with the exception that instead of
#' calculating the Shapiro-Wilk test statistic W for every \eqn{z\in\{0.25, 0.26, \dots, 1.24, 1.25\}},
#' we instead use [stats::optimise] to find z that gives the largest W.
#'
#' @returns Numeric vector of the transformed values. For convenience, the transformation type and parameters are stored in the object's attributes.
#'
#' @references
#'
#' Chou, Y. M., Polansky, A. M., & Mason, R. L. (1998). Transforming non-normal data to normality in statistical process control. \emph{Journal of Quality Technology, 30}(2), 133-141.
#'
#' @name Johnson
NULL

#' @rdname Johnson
#' @export
Johnson <- function(x, lower=0.25, upper=1.25) {
  stopifnot(lower < upper)

  result <- optimise(.johnsonObjective, lower = lower, upper = upper, x=x, maximum = TRUE)
  w <- result[["objective"]]
  z <- result[["maximum"]]


  attrs <- attributes(w)
  attributes(w) <- NULL

  y <- attrs[["y"]]
  attrs[["y"]] <- NULL
  attrs[["w"]] <- w
  attrs[["z"]] <- z

  attributes(y) <- attrs

  return(y)
}

.johnsonObjective <- function(z, x) {
  q <- .johnsonQtls(x=x, z=z)

  if (q["QR"] < 1) {
    types <- c("sl", "sb")
  } else {
    types <- c("sl", "su")
  }

  results <- lapply(types, .johnson, x=x, q=q)


  result <- results[[which.max(unlist(results))]]

  return(result)
}

.johnson <- function(type=c("sl", "sb", "su"), x, q) {
  type <- match.arg(type)
  result <- switch(
    type,
    "sl" = .johnsonSl(x, q),
    "sb" = .johnsonSb(x, q),
    "su" = .johnsonSu(x, q)
  )

  if (length(result) == 1 && is.na(result)) return(-Inf)

  w <- stats::shapiro.test(result[["y"]])[["statistic"]]
  attributes(w) <- result

  return(w)
}

# from jtrans package

.johnsonSb <- function(x, q) {

  eta <- q$z / acosh(.5 * sqrt((1 + q$xm / q$xu) * (1 + q$xm / q$xl)))
  gamma <- eta * asinh((q$xm / q$xl - q$xm / q$xu) *
                         sqrt((1 + q$xm / q$xu) *
                                (1 + q$xm / q$xl) - 4) /
                         (2 * (q$xm^2 / q$xl / q$xu - 1)))
  lambda <- (q$xm * sqrt(((1 + q$xm / q$xu) *
                            (1 + q$xm / q$xl) - 2)^2 - 4) /
               (q$xm^2 / q$xl / q$xu - 1))
  epsilon <- .5 * (q$x2 + q$x3 - lambda +
                     q$xm * (q$xm / q$xl - q$xm / q$xu) /
                     (q$xm^2 / q$xl / q$xu - 1))

  if (is.nan(gamma) | is.nan(epsilon) | eta <= 0 | lambda <= 0)
    return(NA)

  if (all(x > epsilon) & all(x < epsilon + lambda)) {
    return(list(y  = gamma + eta *  log((x - epsilon) /
                                          (lambda + epsilon - x)),
                params = list(eta=unname(eta), gamma=unname(gamma), lambda=unname(lambda), epsilon=unname(epsilon), z=q$z),
                type="sb"))
  } else return(NA)
}


.johnsonSu <- function (x, q) {

  eta <- 2 * q$z / acosh(.5 * (q$xu / q$xm + q$xl / q$xm))
  gamma <- eta * asinh((q$xl / q$xm - q$xu / q$xm) /
                         (2 * sqrt(q$xu * q$xl / q$xm^2 - 1)))
  lambda <- (2 * q$xm * sqrt(q$xu * q$xl / q$xm^2 - 1) /
               (q$xu / q$xm + q$xl / q$xm - 2) /
               sqrt(q$xu / q$xm + q$xl / q$xm + 2))
  epsilon <- .5 * (q$x2 + q$x3 + q$xm * (q$xl / q$xm - q$xu / q$xm) /
                     (q$xu / q$xm + q$xl / q$xm - 2))

  if (is.nan(gamma) | is.nan(epsilon) | eta <= 0 | lambda <= 0)
    return(NA)

  return(list(y  = gamma + eta * asinh((x - epsilon) / lambda),
              params = list(eta=unname(eta), gamma=unname(gamma), lambda=unname(lambda), epsilon=unname(epsilon), z=q$z),
              type="su"))
}

.johnsonSl <- function(x, q) {

  if (q$xu / q$xm <= 1) return(NA)

  eta <- 2 * q$z / log(q$xu / q$xm)
  gamma <- eta * log((q$xu / q$xm - 1) / sqrt(q$xu * q$xm))
  epsilon <- .5 * (q$x2 + q$x3 - q$xm * (q$xu / q$xm + 1) /
                     (q$xu / q$xm - 1))

  if (is.nan(gamma) | is.nan(epsilon) | eta <= 0) return(NA)

  if (all(x > epsilon)) {
    return(list(y  = gamma + eta * log(x - epsilon),
                params = list(eta=unname(eta), gamma=unname(gamma), lambda=NA, epsilon=unname(epsilon), z=q$z),
                type="sl"))
  } else return(NA)
}

.johnsonQtls <- function(x, z) {
  qtls <- quantile(x, probs=pnorm(c(-3 * z, -z, z, 3 * z)))
  q <- list(xl = qtls[2] - qtls[1],
            xm = qtls[3] - qtls[2],
            xu = qtls[4] - qtls[3],
            QR = (qtls[4] - qtls[3]) * (qtls[2] - qtls[1]) /
              (qtls[3] - qtls[2])^2,
            z  = z,
            x2 = qtls[2],
            x3 = qtls[3])
  return(q)
}
