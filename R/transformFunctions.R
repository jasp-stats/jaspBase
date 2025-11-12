#some customised transform functions for compute column and filter

#' @export
fishZ        <- function(x)              { return(atanh(x))                   }
#' @export
invFishZ     <- function(x)              { return(tanh(x))                    }

#' @export
logit <- function(x) { stats::qlogis(x) }

#' @export
invLogit <- function(x) { stats::plogis(x) }
