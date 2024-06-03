.internal <- list2env(list(
  # It's not 100% clear if "address" is the best choice here, but it should be a little bit faster than constructing hashes using identical.
  #  See also https://github.com/wch/r-source/blob/trunk/src/library/utils/src/hashtab.c
  recomputedHashtab = hashtab(type = "address", NULL),
  lastRecomputed    = TRUE,
  dataset           = NULL
), parent = emptyenv())

saveHashOfJaspObject <- function(x) {
  .internal[["recomputedHashtab"]][[x]] <- 0L
}

setRecomputed <- function(x) {
  .internal[["lastRecomputed"]] <- x
}

emptyRecomputed <- function() {
  # remove all stored hashes
  utils::clrhash(.internal[["recomputedHashtab"]])
  # set lastRecomputed to TRUE
  setRecomputed(TRUE)
}

#' Set or retrieve a jaspObject
#' @description `%setOrRetrieve%` is a useful shorthand for a common pattern.
#' @param lhs an assignment into a jaspObject, e.g., `container[["table"]]`.
#' @param rhs a function call that creates a jaspObject.
#'
#' @details `%setOrRetrieve%` exists as a shorthand for the following very common pattern:
#' ```
#' if (is.null(jaspContainer[[key]])) { # was this subelement already computed?
#'   subContainer <- createJaspContainer(..., dependencies = ...) # no, so recreate it
#'   jaspContainer[[key]] <- subContainer # store it with this key
#' } else {
#'   subContainer <- jaspContainer[[key]] # it was recomputed, retrieve it from the state.
#' }
#' ```
#' The code above duplicates the phrase `jaspContainer[[key]]` quite a bit.
#' When this is a string literal, this introduces a lot of room for copy-paste errors.
#' with `%setOrRetrieve%`, this becomes
#' ```
#' subContainer <- jaspContainer[[key]] \%setOrRetrieve\% createJaspContainer(..., dependencies = ...)
#' ```
#'
#' The same pattern can also be used to set and retrieve state objects.
#' Consider the following code
#' ```
#' if (is.null(jaspContainer[[key]])) {
#'   object <- expensiveComputeFunction()
#'   state <- createJaspState(object, dependencies = ...)
#'   jaspContainer[[key]] <- state
#' } else {
#'   object <- jaspContainer[[key]]$object
#' }
#' ```
#' with `%setOrRetrieve%`, this becomes
#' ```
#' object <- jaspContainer[[key]] \%setOrRetrieve\% (
#'   expensiveComputeFunction() |>
#'   createJaspState(dependencies = ...)
#' )
#' ```
#' If the `rhs` passed to `%setOrRetrieve%` returns an object of class `jaspStateR` then `%setOrRetrieve%` returns the object it contains, rather than the jaspObject itself.
#' In all other cases the jaspObject is returned.
#' If the `rhs` does not return a jaspObject, an error is thrown.
#'
#' @rdname setOrRetrieve
#' @example inst/examples/ex-setOrRetrieve.R
#' @export
`%setOrRetrieve%` <- function(lhs, rhs) {

  exprLhs <- substitute(lhs) # need to do this before evaluating lhs
  if (!is.null(lhs)) {

    if (!is.jaspObjR(lhs))
      stop("The left-hand side of %setOrRetrieve% did not return a jaspObject!", domain = NA)

    saveHashOfJaspObject(lhs)
    setRecomputed(FALSE)

    if (is.jaspStateR(lhs))
      return(lhs$object)

    return(lhs)

  }

  if (length(exprLhs) != 3L || !identical(as.character(exprLhs[[1L]]), "[["))
    stop("The parent of the left-hand side of %setOrRetrieve% is not indexing with `[[` in a jaspObject!", domain = NA)

  # for nested objects, e.g., `container[["a"]][["b"]]` this will retrieve `container[["a"]]`
  # requires `envir = parent.frame(2L)` because the parent jaspObject does not exist in this functions environment
  parentObjectLhs <- eval(exprLhs[[2L]], envir = parent.frame(1L))
  if (!is.jaspObjR(parentObjectLhs))
    stop("The parent of the left-hand side of %setOrRetrieve% (", as.character(exprLhs[[2L]]), ") did not return a jaspObject!", domain = NA)

  setRecomputed(TRUE)

  result <- force(rhs)
  if (!is.jaspObjR(result))
    stop("The right hand side of `%setOrRetrieve%` should evaluate to a jaspObject but it got an object of class ",
         paste(class(result), collapse = ""), domain = NA)

  expr <- call("<-", exprLhs, result) # the literal result
  # assign the result in the calling environment because otherwise the parent jaspObject cannot be found.
  eval(expr, envir = parent.frame(1L))

  if (is.jaspStateR(result))
    return(result$object)

  return(result)

}

#' @description [isRecomputed] tests if a jaspObject was recomputed or not, if it was created with [%setOrRetrieve%].
#' @param x a jaspObject, or missing in which case the last created or retrieved jaspObject is considered.
#'
#' @details `isRecomputed` only works when \code{\link{\%setOrRetrieve\%}} is used to set or retrieve a jaspObject.
#' @rdname setOrRetrieve
#' @export
isRecomputed <- function(x) {

  if (missing(x))
    return(.internal[["lastRecomputed"]])

  if (!is.jaspObjR(x))
    stop("isRecomputed should only be called with a jaspObject!", domain = NA)

  return(is.null(.internal[["recomputedHashtab"]][[x]]))

}
