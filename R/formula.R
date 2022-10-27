#' @title JASP Formulas
#'
#' @description This function provides support to [stats::formula] in R syntax. It is used to internally parse formula objects.
#' This function is not intended for direct use.
#'
#' @param formula A formula object.
#' @param data A data frame.
#' @returns A list of class "jaspFormula" is returned, with the following elements:
#' \itemize{
#'   \item{\code{formula}}{ The original formula object.}
#'   \item{\code{lhs}}{ A vector of column names included on the left hand-side of the formula.}
#'   \item{\code{rhs}}{ A List of \code{fixed} and \code{random} terms that appear on the right hand-side of the formula.}
#' }
#' The elements of the \code{fixed} terms are:
#' \itemize{
#'   \item{\code{vars}}{ A character vector of model terms.}
#'   \item{\code{intercept}}{ Logical. If TRUE, intercept is included, if FALSE, intercept it not included.}
#' }
#' The \code{random} is itself a list of length equal to the number of random factors.
#' Each element is a list that contains the same elements as \code{fixed}, plus the following elements:
#' \itemize{
#'   \item{\code{correlated}}{ Logical. Are the terms correlated?}
#'   \item{\code{group}}{ The name of the random group factor.}
#' }
#'
#' @details The formulas in JASP follow the same rules as in [stats::formula], but have some functionality removed.
#' Specifically, it is not possible to use [stats::offset] in a formula. Analyses that allow including [stats::offset] in the model have specialized argument for that purpose.
#' It is also not possible to use variable transformations in a formula. Thus, instead of transforming variables using formulas, transform the variables before entering them in the analysis.
#'
#' For specification of the random effects, [lme4::lme4-package] syntax is used. There is a difference in how JASP parses whether or not are random effects correlated:
#' Under each random grouping factor, all terms can be either correlated or uncorrelated, but it is not possible to correlate some random effects but hold some random effects uncorrelated.
#' @example inst/examples/formula.R
#' @rdname jaspFormula
#' @export
jaspFormula <- function(formula, data) {
  formulaCheckRequirements(formula, data)

  result <- list(
    formula = formula,
    lhs = formulaGetLhs(formula),
    rhs = formulaGetRhs(formula)
  )

  class(result) <- c("jaspFormula")
  return(result)
}

formulaCheckRequirements <- function(formula, data) {
  if (!inherits(formula, "formula")) {
    stop("`formula` argument must be object of class `formula`.")
  }

  attr <- attributes(terms(formula))

  if (!is.null(attr[["offset"]])) {
    stop("JASP formulas do not understand `offset` terms. Analyses that allow the `offset` terms have a special `offset` argument.")
  }

  lhs <- all.names(formulaExtractLhs(formula))
  anyLhsTransformed <- !all(lhs %in% c(colnames(data), "cbind", "(", ")"))

  rhs <- all.names(formulaExtractRhs(formula))
  anyRhsTransformed <- !all(rhs %in% c(colnames(data), "+", "-", ":", "*", "1", "0", "(", ")", "|", "||"))

  if (anyLhsTransformed || anyRhsTransformed) {
    stop("JASP formulas do not allow variable transformations. Please transform your variables before running the analysis.")
  }
}

formulaGetLhs <- function(formula) {
  lhs <- list(
    vars = all.vars(formulaExtractLhs(formula))
  )
  return(lhs)
}

formulaGetRhs <- function(formula) {
  rhs <- list(
    fixed  = formulaFixedRhs (formula),
    random = formulaRandomRhs(formula)
  )

  return(rhs)
}

formulaFixedRhs <- function(formula) {

  if(formulaContainsRandomEffects(formula)) {
    re      <- formulaGetRandomEffects(formula)
    re      <- paste0("(", re, ")")
    upd     <- paste(c("~ . ", re), collapse = "-")
    formula <- update.formula(formula, as.formula(upd))
  }

  rhs <- list(
    vars      = attr(terms(formula), "term.labels"),
    intercept = attr(terms(formula), "intercept") != 0
  )

  return(rhs)
}

formulaRandomRhs <- function(formula) {
  if(!formulaContainsRandomEffects(formula)) {
    return(NULL)
  }

  re <- formulaGetRandomEffects(formula)
  correlated <- vapply(re, function(r) r[[1]] == as.name("|"), logical(1))

  groupings <- lapply(re, formulaExtractRhs)

  results <- list()
  for(i in seq_along(re)) {
    form      <- ~ 0
    form[[2]] <- formulaExtractLhs(re[[i]])
    res       <- formulaFixedRhs(form)
    res[["correlated"]] <- correlated[[i]]
    res[["group"]]      <- groupings[[i]]

    results[[i]] <- res
  }

  output <- list()
  for(group in unique(groupings)) {
    belongsToGroup <- group == groupings
    if(sum(belongsToGroup) == 1) {
      output[[group]] <- results[[which(belongsToGroup)]]
    } else {
      if(any(correlated[belongsToGroup]))
        warning("JASP formulas cannot mix uncorrelated and correlated terms under a random factor. Coercing all random effects under ", group, " to be uncorrelated.")

      res <- results[belongsToGroup]
      output[[group]] <- list(
        vars       = unlist(lapply(res, "[[", "vars")),
        intercept  = any(unlist(lapply(res, "[[", "intercept"))),
        correlated = FALSE,
        group      = group
      )
    }
  }

  return(output)
}


formulaContainsRandomEffects <- function(formula) {
  any(c('|','||') %in% all.names(formula))
}


formulaGetRandomEffects <- function(formula) {
  result <- c()
  for(i in seq_len(length(formula))) {
    term <- formula[[i]]
    if(term == as.name("|") || term == as.name("||")) {
      return(formula)
    } else if(length(term) > 1) {
      result <- c(result, getRandomEffects(term))
    }
  }

  return(result)
}

formulaExtractLhs <- function(formula) {
  if(length(formula) <= 2) { # only rhs specified
    return(NULL)
  } else {
    return(formula[[2]])
  }
}

formulaExtractRhs <- function(formula) {
  return(formula[[length(formula)]])
}

