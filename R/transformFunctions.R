#some customised transform functions for compute column and filter

#' @export
fishZ        <- function(x)              { return(atanh(x))                   }
#' @export
invFishZ     <- function(x)              { return(tanh(x))                    }

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

#' @export
powerTransform <- function(x, lambda, shift = 0) {
  x <- x + shift
  x[x<=0] <- NA

  gm <- exp(mean(log(x), na.rm = TRUE))
  if(lambda == 0) {
    result <- gm * log(x)
  } else {
    result <- (abs(x)^lambda - 1) / lambda*gm^(lambda-1)
  }

  return(result)
}

#' @export
YeoJohnson <- function(x, lambda) {
  result <- mapply(function(xx, ll) {
    if(xx >= 0) {
      xp1 <- xx + 1
      if(ll == 0) {
        return(log(xp1))
      } else {
        return((xp1^ll - 1)/ll)
      }
    } else {
      mxp1 <- -xx+1
      if(ll == 2) {
        return(-log(mxp1))
      } else {
        tml  <- 2-ll
        num <- (mxp1^tml) - 1
        return(-num/tml)
      }
    }
  }, xx = x, ll = lambda)

  return(result)
}

#' @export
logit <- function(x) { stats::qlogis(x) }

#' @export
invLogit <- function(x) { stats::plogis(x) }
