#some pretty samplers that know how many rows you would like to see in your computed column!
#having compact aligned code makes it easier to see mistakes

#' @export
normalDist   <- function(mean, sd)          { return( stats::rnorm(   n=.dataSetRowCount(), mean=mean, sd=sd))             }
#' @export
expDist      <- function(rate)              { return( stats::rexp(    n=.dataSetRowCount(), rate=rate))                    }
#' @export
betaDist     <- function(alpha, beta)       { return( stats::rbeta(   n=.dataSetRowCount(), shape1=alpha, shape2=beta))    }
#' @export
gammaDist    <- function(shape, scale)      { return( stats::rgamma(  n=.dataSetRowCount(), shape=shape, scale=scale))     }
#' @export
unifDist     <- function(min, max)          { return( stats::runif(   n=.dataSetRowCount(), min=min, max=max))             }
#' @export
tDist        <- function(df, ncp)           { return( stats::rt(      n=.dataSetRowCount(), df=df, ncp=ncp))               }
#' @export
chiSqDist    <- function(df, ncp)           { return( stats::rchisq(  n=.dataSetRowCount(), df=df, ncp=ncp))               }
#' @export
binomDist    <- function(trials, prob)      { return( stats::rbinom(  n=.dataSetRowCount(), size=trials, prob=prob))       }
#' @export
poisDist     <- function(lambda)            { return( stats::rpois(   n=.dataSetRowCount(), lambda=lambda))                }
#' @export
geomDist     <- function(prob)              { return( stats::rgeom(   n=.dataSetRowCount(), prob=prob))                    }
#' @export
fDist        <- function(df1, df2, ncp)     { return( stats::rf(      n=.dataSetRowCount(), df1=df1, df2=df2, ncp=ncp))    }
#' @export
negBinomDist <- function(targetTrial, prob) { return( stats::rnbinom( n=.dataSetRowCount(), size=targetTrial, prob=prob))  }
#' @export
logNormDist  <- function(meanLog, sdLog)    { return( stats::rlnorm(  n=.dataSetRowCount(), meanlog=meanLog, sdlog=sdLog)) }
#' @export
weibullDist  <- function(shape, scale)      { return( stats::rweibull(n=.dataSetRowCount(), shape=shape, scale=scale))     }

# TODO(Alexander/Joris): default arguments for function calls
#' @export
integerDist <- function(categories, replace=TRUE, prob=NULL) {
  return(sample.int(n=categories, size=.dataSetRowCount(), replace=replace, prob=prob))
}
