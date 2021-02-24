replaceNA <- function(column, replaceWith) {
  UseMethod("replaceNA", column)
}

replaceNA.numeric <- function(column, replaceWith) {
  result <- ifelse(is.na(column), replaceWith, column)
  return(result)
}

replaceNA.character <- function(column, replaceWith) {
  charReplaceWith <- as.character(replaceWith)
  charColumn <- as.character(column)
  result <- ifelse(is.na(charColumn), charReplaceWith, charColumn)
  return(result)
}

replaceNA.factor <- function(column, replaceWith) {
  result <- as.factor(replaceNA.character(column, replaceWith))
  return(result)
}

replaceNA.ordered <- function(column, replaceWith) {
  result <- reorderFactor(replaceNA.factor(column, replaceWith))
  return(result)
}

ifElse <- function(test, yes, no) {
  useType <- findDominatingClass(yes, no)
  UseMethod("ifElse", useType)
}

ifElse.integer <- function(test, yes, no) {
  result <- ifElse.numeric(test, yes, no)
  return(result)
}

ifElse.numeric <- function(test, yes, no) {
  result <- ifelse(test, yes, no)
  return(result)
}

ifElse.character <- function(test, yes, no) {
  yesChar <- as.character(yes)
  noChar <- as.character(no)
  result <- ifelse(test, yesChar, noChar)
  return(result)
}

ifElse.factor <- function(test, yes, no) {
  result <- as.factor(ifElse.character(test, yes, no))
  return(result)
}

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

zScores <- function(x) {
  UseMethod("zScores", x)
}

zScores.numeric <- function(x) {
  scale(x)
}

zScores.factor <- function(x) {
  allLevelsAreNumeric <- !anyNA(suppressWarnings(numericLevels <- as.numeric(levels(x))))
  if (allLevelsAreNumeric)
    return(scale(numericLevels[x]))
  else # assume this is really what the user wants
    return(scale(x))
}

hasSubstring <- function(string, substring) {
  return(grepl(pattern = substring, x = string, fixed = TRUE))
}

# Operator to perform an expression by group / conditionally on a factor / given a condition. (author: EJvK):
# Usage: (abs(mtcars$mpg - mean(mtcars$mpg)) > var(mtcars$mpg)) %|% mtcars$cyl
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
