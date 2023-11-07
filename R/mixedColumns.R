validateCellTypes <- function(types) {
  permittedTypes <- c("integer", "number", "pvalue", "string", "separator")
  if (!all(types %in% permittedTypes)) {
    badTypes <- unique(setdiff(types, permittedTypes))
    stop(sprintf("Valid types are `%s` but got: %s",
                 paste(permittedTypes, collapse = ", "),
                 paste(badTypes,       collapse = ", ")))
  }
}

validateCellFormats <- function(formats, types, lengthOfValues) {
  if (is.null(formats)) {
    formats <- vector("list", length(types))
    if (any(types %in% c("number", "pvalue"))) {

      formats[types == "number"] <- list("sf:4;dp:3")
      formats[types == "pvalue"] <- list("dp:3;p:.001")

    }
  } else {
    stopifnot(length(types) == length(formats))
  }
  stopifnot(lengthOfValues == length(types))
  return(formats)
}

#' @export
createMixedColumn <- function(values, types, formats = NULL) {

  validateCellTypes(types)
  formats <- validateCellFormats(formats, types, length(values))

  # NOTE: names are not actually used
  result <- Map(\(v, t, f) list(value = v, type = t, format = f), values, types, formats)
  class(result) <- c("mixed", "column")
  return(result)

}

#' @export
createMixedRow <- function(value, type, format = NULL) {
  # TODO: support multiple values and types?
  validateCellTypes(type)
  format <- validateCellFormats(format, type, length(value))
  result <- list(value = value, type = type, format = format[[1L]])
  class(result) <- c("mixed", "row")
  return(result)
}

#' @export
# So we don't need I(...) when putting mixed in a data.frame
as.data.frame.mixed <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame.AsIs(x, row.names = row.names, optional = optional, ...)
}
