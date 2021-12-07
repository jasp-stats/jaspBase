# these functions should be deprecated but they are still used in the modules

#' @export
.vf <- function(formula) {

  in.pieces <- .decompose(formula)
  ved <- .jrapply(in.pieces, .v)
  .compose(ved)
}

#' @export
.unvf <- function(formula) {

  in.pieces <- .decompose(formula)
  unved <- .jrapply(in.pieces, .unv)

  interaction.symbol <- "\u2009\u273B\u2009"
  base::Encoding(interaction.symbol) <- "UTF-8"

  .compose(unved, interaction.symbol)
}

.decompose <- function(formulas) {

  lapply(as.list(formulas), function(formula) {

    sides <- strsplit(formula, "~", fixed=TRUE)[[1]]

    lapply(sides, function(formula) {

      terms <- strsplit(formula, "+", fixed=TRUE)

      lapply(terms, function(term) {
      components <- strsplit(term, ":")
      components <- sapply(components, trimws, simplify=FALSE)

      })[[1]]
    })
  })
}

.compose <- function(formulas, i.symbol=":") {

  sapply(formulas, function(formula) {

    formula <- sapply(formula, function(side) {

      side <- sapply(side, function(term) {

        term <- sapply(term, function(component) { base::Encoding(component) <- "UTF-8" ; component })

        paste(term, collapse=i.symbol)
      })

      paste(side, collapse=" + ")
    })

    paste(formula, collapse=" ~ ")
  })
}

.jrapply <- function(X, FUN) {

  if (is.list(X) && length(X) > 0) {

    for (i in 1:length(X)) {
      X[[i]] <- .jrapply(X[[i]], FUN)
    }
  }
  else {
    X <- FUN(X)
  }

  X
}

.shouldContinue <- function(value) {

  base::identical(value, 0) || base::identical(value, as.integer(0)) || (is.list(value) && value$status == "ok")
}

#' @export
.clean <- function(value) {
    # Clean function value so it can be reported in json/html

  if (is.list(value)) {
      if (is.null(names(value))) {
          for (i in length(value)) {
          value[[i]] <- .clean(value[[i]])
      }
    } else {
        for (name in names(value)) {
          value[[name]] <- .clean(value[[name]])
      }
    }
    return(value)
  }

  if (is.null(value)) {
      return ("")
  }

  if (is.character(value)) {
      return(value)
  }

  if (is.finite(value)) {
      return(value)
  }

  if (is.na(value)) {
      return("NaN")
  }

    if (identical(value, numeric(0))) {
        return("")
    }

  if (value == Inf) {
      return("\u221E")
  }

  if (value == -Inf) {
      return("-\u221E")
  }

  stop("could not clean value")
}

