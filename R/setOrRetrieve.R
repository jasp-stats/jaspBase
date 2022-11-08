.internal <- list2env(list(
  recomputedHashtab = hashtab(type = "address", NULL), # unclear if "address" is the correct choice here
  lastRecomputed    = TRUE
), parent = emptyenv())

saveHashOfJaspObject <- function(x) {
  .internal[["recomputedHashtab"]][[x]] <- 0L
}

setRecomputed <- function(x) {
  .internal[["lastRecomputed"]] <- x
}

#' Test if a jaspObject was recomputed of not.
#' @param x a jaspObject, or missing in which case the last created or retrieved jaspObject is considered.
#'
#' @details Note that this function only works when \code{\link{\%setOrRetrieve\%}} is used to set or retrieve a jaspObject.
#'
#' @export
isRecomputed <- function(x) {

  if (missing(x))
    return(.internal[["lastRecomputed"]])

  if (!is.jaspObjR(x))
    stop("isRecomputed should only be called with a jaspObject!", domain = NA)

  return(is.null(.internal[["recomputedHashtab"]][[x]]))

}

#' Set or retrieve a jaspObject
#' @param lhs an assignment into a jaspObject, e.g., `container[["table"]]`.
#' @param rhs a function call that creates a jaspObject.
#'
#' @details This function exists as a shorthand for the following very common pattern:
#' \preformatted{
#' if (is.null(jaspContainer[[key]])) { # was this subelement already computed?
#'   subContainer <- createJaspContainer(..., dependencies = ...) # no, so recreate it
#'   jaspContainer[[key]] <- subContainer # store it in this position
#' } else {
#'   subContainer <- jaspContainer[[key]] # it was recomputed, retrieve it from the state.
#' }
#' }
#' The code above duplicates the phrase `jaspContainer[[key]]` quite a bit.
#' When this is a string literal, this introduces a lot of room for copy-paste errors.
#' with `%setOrRetrieve%`, this becomes
#' \preformatted{
#' subContainer <- jaspContainer[[key]] \%setOrRetrieve\% createJaspContainer(..., dependencies = ...)
#' }
#'
#' The same pattern can also be used to set and retrieve state objects.
#' Consider the following code
#' \preformatted{
#' if (is.null(jaspContainer[[key]])) {
#'   object <- expensiveComputeFunction()
#'   state <- createJaspState(object, dependencies = ...)
#'   jaspContainer[[key]] <- state
#' } else {
#'   object <- jaspContainer[[key]]$object
#' }
#' }
#' with `%setOrRetrieve%`, this becomes
#' \preformatted{
#' object <- jaspContainer[[key]] \%setOrRetrieve\% (
#'   expensiveComputeFunction() |>
#'   createJaspState(dependencies = ...)
#' )
#' }
#' Note that if the `rhs` passed to `%setOrRetrieve%` returns an object of class `jaspStateR` then `%setOrRetrieve%` returns the object it contains, rather than the jaspObject itself.
#' In all other cases the jaspObject is returned.
#' If the `rhs` does not return a jaspObject, an error is thrown.
#'
#' @rdname setOrRetrieve
#' @example inst/examples/ex-setOrRetrieve.R
#' @export
`%setOrRetrieve%` <- function(lhs, rhs) {

  exprLhs <- substitute(lhs) # need to do this before evaluating lhs
  if (!is.null(lhs)) {

    saveHashOfJaspObject(lhs)
    setRecomputed(FALSE) # will be a global value inside jaspBase (without <<-)

    if (is.jaspStateR(lhs))
      return(lhs$object)

    return(lhs)

  }

  setRecomputed(TRUE)

  result <- force(rhs)
  if (!is.jaspObjR(result))
    stop("The right hand side of `%setOrRetrieve%` should evaluate to a jaspObject but it got an object of class ",
         paste(class(result), collapse = ""), domain = NA)

  expr <- call("<-", exprLhs, as.name("result"))
  eval(expr)

  if (is.jaspStateR(result))
    return(result$object)

  return(result)

}

