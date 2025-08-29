#some customised transform functions for compute column and filter

#' @export
fishZ        <- function(x)              { return(atanh(x))                   }
#' @export
invFishZ     <- function(x)              { return(tanh(x))                    }

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
logit <- function(x) { stats::qlogis(x) }

#' @export
invLogit <- function(x) { stats::plogis(x) }
