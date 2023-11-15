#' @export
replaceNA <- function(column, replaceWith) {
  UseMethod("replaceNA", column)
}

#' @export
replaceNA.numeric <- function(column, replaceWith) {
  result <- ifelse(is.na(column), replaceWith, column)
  return(result)
}

#' @export
replaceNA.character <- function(column, replaceWith) {
  charReplaceWith <- as.character(replaceWith)
  charColumn <- as.character(column)
  result <- ifelse(is.na(charColumn), charReplaceWith, charColumn)
  return(result)
}

#' @export
replaceNA.factor <- function(column, replaceWith) {
  result <- as.factor(replaceNA.character(column, replaceWith))
  return(result)
}

#' @export
replaceNA.ordered <- function(column, replaceWith) {
  result <- reorderFactor(replaceNA.factor(column, replaceWith))
  return(result)
}

#' @export
ifElse <- function(test, yes, no) {
  useType <- findDominatingClass(yes, no)
  UseMethod("ifElse", useType)
}

#' @export
ifElse.integer <- function(test, yes, no) {
  result <- ifElse.numeric(test, yes, no)
  return(result)
}

#' @export
ifElse.numeric <- function(test, yes, no) {
  result <- ifelse(test, yes, no)
  return(result)
}

#' @export
ifElse.character <- function(test, yes, no) {
  yesChar <- as.character(yes)
  noChar <- as.character(no)
  result <- ifelse(test, yesChar, noChar)
  return(result)
}

#' @export
ifElse.factor <- function(test, yes, no) {
  result <- as.factor(ifElse.character(test, yes, no))
  return(result)
}

#' @export
ifElse.ordered <- function(test, yes, no) {
  result <- reorderFactor(as.ordered(ifElse.factor(test, yes, no)))
  return(result)
}

findDominatingClass <- function(...) {
  args <- list(...)

  allTypes <- sapply(args, class)
  maxType <- max(ordered(allTypes, levels=c("integer", "numeric", "ordered",
                                            "factor", "character"))
  )

  if (maxType=="numeric" || maxType=="integer") {
    return(as.numeric(1))
  } else if (maxType=="ordered") {
    return(as.ordered(1))
  } else if (maxType=="factor") {
    return(as.factor(1))
  } else if (maxType=="character") {
    return(as.character(1))
  } else {
    return(NaN)
  }
}

reorderFactor <- function(someFactor) {
  stopifnot(is.factor(someFactor))

  theLevels <- levels(someFactor)
  numericalOrder <- suppressWarnings(as.numeric(theLevels))
  numericalOrderWorks <- all(!is.na(numericalOrder))

  if (isTRUE(numericalOrderWorks)) {
    # basically numerics
    orderedLevels <- numericalOrder[ordered(numericalOrder)]
    result <- ordered(someFactor, levels=orderedLevels)
    return(result)
  } else {
    result <- ordered(someFactor, levels=theLevels[order(theLevels)])
    return(result)
  }
}

#' @export
zScores <- function(x) {
  UseMethod("zScores", x)
}

#' @export
zScores.numeric <- function(x) {
  scale(x)
}

#' @export
zScores.factor <- function(x) {
  allLevelsAreNumeric <- !anyNA(suppressWarnings(numericLevels <- as.numeric(levels(x))))
  if (allLevelsAreNumeric)
    return(scale(numericLevels[x]))
  else # assume this is really what the user wants
    return(scale(x))
}

#' @export
hasSubstring <- function(string, substring) {
  return(grepl(pattern = substring, x = string, fixed = TRUE))
}

# Operator to perform an expression by group / conditionally on a factor / given a condition. (author: EJvK):
# Usage: (abs(mtcars$mpg - mean(mtcars$mpg)) > var(mtcars$mpg)) %|% mtcars$cyl
#' @export
`%|%` <- function(expr, group) {

  group       <- as.factor(group)
  expr        <- as.list(match.call())$expr
  nams        <- codetools::findGlobals(as.function(list(expr)), FALSE)$variables
  vars        <- lapply(nams, get)
  names(vars) <- nams
  v           <- logical(length(group))

  for (i in levels(group)) {
    env           <- list2env(lapply(vars, function(x) subset(x, group == i)))
    v[group == i] <- eval(expr = expr, envir = env)
  }

  return(v)
}

#' @export
rowMean <- function(...) {
  apply(data.frame(...), 1, mean, na.rm=FALSE)
}

#' @export
rowMeanNaRm <- function(...) {
  apply(data.frame(...), 1, mean, na.rm=TRUE)
}

#' @export
rowSum <- function(...) {
  apply(data.frame(...), 1, sum, na.rm=FALSE)
}

#' @export
rowSumNaRm <- function(...) {
  apply(data.frame(...), 1, sum, na.rm=TRUE)
}

#' @export
rowSD <- function(...) {
  apply(data.frame(...), 1, sd, na.rm=FALSE)
}

#' @export
rowSDNaRm <- function(...) {
  apply(data.frame(...), 1, sd, na.rm=TRUE)
}

#' @export
rowVariance <- function(...) {
  apply(data.frame(...), 1, var, na.rm=FALSE)
}

#' @export
rowVarianceNaRm <- function(...) {
  apply(data.frame(...), 1, var, na.rm=TRUE)
}

#' @export
rowCovariance <- function(...) {
  apply(data.frame(...), 1, cov, na.rm=FALSE)
}

#' @export
rowCovarianceNaRm <- function(...) {
  apply(data.frame(...), 1, cov, na.rm=TRUE)
}

#' @export
rowCorrelation <- function(...) {
  apply(data.frame(...), 1, cor, na.rm=FALSE)
}

#' @export
rowCorrelationNaRm <- function(...) {
  apply(data.frame(...), 1, cor, na.rm=TRUE)
}

#' @export
rowMedian <- function(...) {
  apply(data.frame(...), 1, median, na.rm=FALSE)
}

#' @export
rowMedianNaRm <- function(...) {
  apply(data.frame(...), 1, median, na.rm=TRUE)
}

#' @export
rowMin <- function(...) {
  apply(data.frame(...), 1, min, na.rm=FALSE)
}

#' @export
rowMinNaRm <- function(...) {
  apply(data.frame(...), 1, min, na.rm=TRUE)
}


#' @export
rowMax <- function(...) {
  apply(data.frame(...), 1, max, na.rm=FALSE)
}

#' @export
rowMaxNaRm <- function(...) {
  apply(data.frame(...), 1, max, na.rm=TRUE)
}