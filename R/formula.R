#' @title JASP Formulas
#'
#' @description These functions provide support to [stats::formula] in R syntax. They are used to internally parse formula objects.
#' These functions are **not** intended for direct use.
#'
#' [jaspFormula] is used to parse R formulas. [makeJaspFormula] is a convenience function that is used for generating R formulas from list objects.
#' [jaspFormulaRhs] is another convenience function that is used in tandem with [makeJaspFormula].
#'
#' @param formula A formula object.
#' @param data A data frame.
#' @param ... Terms added to the rhs of the formula. Use [jaspFormulaRhs] to create the terms.
#' @param response A character giving the names of response variables (on the lhs of the formula).
#' @param terms A character giving the names or terms on the rhs of the formula.
#' @param group A character giving the name of the grouping variable for the random effects.
#' @param intercept Logical. Should intercept be included?
#' @param correlated Logical. Should random effects be correlated?
#' @param x Object of class "jaspFormula".
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
#'   \item{\code{correlated}}{ Logical. Are the terms correlated? Can contain an attribute named "correlations" that holds the entire correlation structure in case a mixture of correlated and uncorrelated terms is used.}
#'   \item{\code{group}}{ The name of the random group factor.}
#' }
#'
#' @details The formulas in JASP follow the same rules as in [stats::formula], but have some functionality removed.
#' Specifically, it is not possible to use [stats::offset] in a formula. Analyses that allow including [stats::offset] in the model have specialized argument for that purpose.
#' It is also not possible to use variable transformations in a formula. Thus, instead of transforming variables using formulas, transform the variables before entering them in the analysis.
#'
#' For specification of the random effects, [lme4::lme4-package] syntax is used. There is a difference in how JASP parses whether or not are random effects correlated:
#' Under each random grouping factor, if some but not all terms are correlated, the output `correlated` is still set to \code{TRUE}. The "correlations" attribute contains the full correlation structure.
#' @example inst/examples/ex-formula.R
#' @rdname jaspFormula
#' @keywords internal
#' @export
jaspFormula <- function(formula, data) {
  formulaEncoded <- formulaEncode(formula)
  data    <- formulaCheckOrReadData(data)
  formulaCheckRequirements(formulaEncoded, data)

  # TODO: We should use the encoded formula hier, but if a column has a type ('col.scale'), then it is decoded by 'col'
  result <- list(
    formula = paste(deparse(formula), collapse = ""),
    lhs = formulaGetLhs(formula),
    rhs = formulaGetRhs(formula)
  )

  class(result) <- c("jaspFormula", class(result))
  return(result)
}

#
#' @rdname jaspFormula
#' @export
makeJaspFormula <- function(..., response=NULL, data) {
  data <- formulaCheckOrReadData(data)

  if(!is.null(response) && !is.character(response)) {
    stop("`response` must be a character.", domain = NA)
  }

  dots <- list(...)
  if(!is.jaspRhs(dots))
    stop("All terms on the rhs must be specified as a `jaspFormulaRhs` object.", domain = NA)

  if(length(response) > 1) {
    response <- paste0("cbind(", paste0(response, collapse = ","), ")")
  }


  rhs <- vapply(dots, makeJaspFormulaRhs, character(1), data = data)

  formula <- stats::reformulate(rhs, response)
  return(jaspFormula(formula, data))
}


#
#' @rdname jaspFormula
#' @export
jaspFormulaRhs <- function(terms = NULL, group = NULL, intercept = TRUE, correlated = TRUE) {
  result <- list(terms = terms, group = group, intercept = intercept, correlated = correlated)
  class(result) <- c("jaspFormulaRhs", class(result))
  return(result)
}

makeJaspFormulaRhs <- function(rhs, data) {
  allVarNames <- colnames(data)

  result <- paste(rhs[["terms"]], collapse = "+")
  if (is.null(rhs[["terms"]])) {
    result <- if(rhs[["intercept"]]) "1" else "NULL"
  } else if(!rhs[["intercept"]]) {
    result <- paste("0", result, sep = "+")
  }

  if(!is.null(rhs[["group"]])) {
    sep <- if(rhs[["correlated"]]) "|" else "||"
    result <- sprintf("(%s %s %s)", result, sep, rhs[["group"]])
  }
  return(result)
}

is.jaspFormulaRhs <- function(x) {
  inherits(x, "jaspFormulaRhs")
}

is.jaspRhs <- function(x) {
  is.jaspFormulaRhs(x) || (is.list(x) && all(vapply(x, is.jaspRhs, logical(1))))
}

formulaEncode <- function(formula) {
  # formula encoding should happen in R as well
  # but for now we do it only in JASP as the dataset passed in by the R user is not getting encoded yet.
  if(jaspBase::jaspResultsCalledFromJasp()) {
    formula <- deparse(formula)
    formula <- jaspBase::encodeColNames(formula)
    formula <- stats::as.formula(formula)
  }

  return(formula)
}

formulaCheckOrReadData <- function(data) {
  # If we are in JASP and no data are supplied explicitly, we simply read the dataset from JASP.
  if(jaspBase::jaspResultsCalledFromJasp()) # && (missing(data) || is.null(data)))
    data <- jaspBase::readDataSetToEnd(all.columns = TRUE)

  if(missing(data) || is.null(data) || !is.data.frame(data))
    stop("`data` must be a data frame.", domain = NA)

  return(data)
}

formulaCheckRequirements <- function(formula, data) {
  if (!inherits(formula, "formula")) {
    stop("`formula` argument must be object of class `formula`.", domain = NA)
  }

  attr <- attributes(stats::terms(formula))

  if (!is.null(attr[["offset"]])) {
    stop("JASP formulas do not understand `offset` terms. Analyses that allow the `offset` terms have a special `offset` argument.", domain = NA)
  }

  columnNames <- decodeColNames(colnames(data))
  lhs <- decodeColNames(all.names(formulaExtractLhs(formula)))
  anyLhsTransformed <- !all(lhs %in% c(columnNames, "cbind", "(", ")"))

  rhs <- decodeColNames(all.names(formulaExtractRhs(formula)))
  anyRhsTransformed <- !all(rhs %in% c(columnNames, "+", "-", ":", "*", "^", "1", "0", "(", ")", "|", "||"))

  if (anyLhsTransformed || anyRhsTransformed) {
    stop(paste0("Cannot parse the formula `", deparse(formula), "`: maybe a wrong variable name is used. Note that variable transformation is not allowed: in this case, please transform your variables before running the analysis."), domain = NA)
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
    formula <- stats::update.formula(formula, stats::as.formula(upd))
  }

  rhs <- list(
    vars      = attr(stats::terms(formula), "term.labels"),
    intercept = attr(stats::terms(formula), "intercept") != 0
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
  groupings <- lapply(groupings, as.character)

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
      res <- results[belongsToGroup]

      if (any(correlated[belongsToGroup])) {
        vars      <- unlist(lapply(res, "[[", "vars"))
        intercept <- any(unlist(lapply(res, "[[", "intercept")))
        if(intercept) allVars <- c("intercept", vars) else allVars <- vars

        correlations <- expand.grid(var1 = allVars, var2 = allVars)
        correlations <- subset(correlations, var1 != var2)
        correlations$index <- 0

        counter <- 0
        for (r in res) {
          if (r$correlated) {
            if (r$intercept) vars <- c("intercept", r$vars) else vars <- r$vars
            if (length(vars) > 1) {
              counter <- counter + 1
              correlations[correlations$var1 %in% vars & correlations$var2 %in% vars, "index"] <- counter
            }
          }
        }

        if(all(correlations$index == 0)) {
          correlated <- FALSE
        } else {
          warning("A mixture of correlated and uncorrelated terms was detected under `", group, "`, are you sure the formula is correctly specified?", domain = NA)
          correlated <- TRUE
          attr(correlated, "correlations") <- correlations
        }

        output[[group]] <- list(
          vars         = vars,
          intercept    = intercept,
          correlated   = correlated,
          group        = group
        )

      } else {
        output[[group]] <- list(
          vars       = unlist(lapply(res, "[[", "vars")),
          intercept  = any(unlist(lapply(res, "[[", "intercept"))),
          correlated = FALSE,
          group      = group
        )
      }

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
      result <- c(result, formulaGetRandomEffects(term))
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

