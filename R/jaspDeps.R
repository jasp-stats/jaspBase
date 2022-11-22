#' jaspDeps - An object to store jaspDependencies
#'
#' @param options passed to `$dependOn`.
#' @param optionsFromObject passed to `$dependOn`.
#' @param optionContainsValue passed to `$dependOn`.
#' @param nestedOptions passed to `$dependOn`.
#' @param nestedOptionsContainsValue passed to `$dependOn`.
#'
#' @details The function is a convenience wrapper to set dependencies in the constructor of jaspObjects.
#' For example, this code
#' \preformatted{
#' container <- createJaspContainer(title = "Title")
#' container$dependOn(optionContainsValue = list(variables = "contNormal"))
#' }
#' is identical to
#' \preformatted{
#' container <- createJaspContainer(
#'   title = "Title",
#'   dependencies = jaspDeps(optionContainsValue = list(variables = "contNormal"))
#' )
#' }
#'
#'
#' @export
jaspDeps <- function(options = NULL, optionsFromObject = NULL, optionContainsValue = NULL, nestedOptions = NULL, nestedOptionsContainsValue = NULL) {
  lst <- list(
    options                    = options,
    optionsFromObject          = optionsFromObject,
    optionContainsValue        = optionContainsValue,
    nestedOptions              = nestedOptions,
    nestedOptionsContainsValue = nestedOptionsContainsValue
  )
  class(lst) <- "jaspDeps"
  return(lst)
}

is.jaspDeps <- function(x) {
  inherits(x, "jaspDeps")
}

setJaspDeps <- function(jaspObj, jaspDeps) {
  jaspObj$dependOn(
    options                    = jaspDeps[["options"]],
    optionsFromObject          = jaspDeps[["optionsFromObject"]],
    optionContainsValue        = jaspDeps[["optionContainsValue"]],
    nestedOptions              = jaspDeps[["nestedOptions"]],
    nestedOptionsContainsValue = jaspDeps[["nestedOptionsContainsValue"]]
  )
  return(jaspObj)
}
