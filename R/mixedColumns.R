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
  data <- Map(\(v, t, f) list(value = v, type = t, format = f), values, types, formats)

  result <- vctrs::new_vctr(data, column = TRUE, class = "mixed")

  return(result)

}

#' @export
createMixedRow <- function(value, type, format = NULL) {
  # TODO: support multiple values and types?
  validateCellTypes(type)
  format <- validateCellFormats(format, type, length(value))
  data <- list(value = value, type = type, format = format[[1L]])
  result <- vctrs::new_vctr(list(data), column = FALSE, class = "mixed")
  return(result)
}

isMixedColumn <- function(x) {
  isTRUE(attr(x, "column"))
}


formatMixedHelper <- function(x, showFormat = FALSE, shortTypes = TRUE, usePillar = FALSE) {

  # TODO: formatting with Pillar is way nicer, but maybe not always compatible with all terminals...
  # it also would require overwriting the print function, which I'm not very keen about

  mixedAbbreviations <- c(
    "pvalue"  = "pval",
    "number"  = "num",
    "string"  = "str",
    "integer" = "int"
  )

  value  <- x[["value"]]
  type   <- x[["type"]]
  format <- x[["format"]]

  if (usePillar) {
    style_num    <- pillar::style_num
    style_subtle <- pillar::style_subtle
  } else {
    style_num <- style_subtle <- function(x, ...) x
  }

  paste0(
    if (type == "string") value else style_num(value, negative = value < 0),
    style_subtle(paste0(if (showFormat || !usePillar) "<" else "", if (shortTypes) mixedAbbreviations[type] else type)),
    if (showFormat) style_subtle(paste0("|", format, ">")) else if (!usePillar) ">"
  )
}

#' @exportS3Method
format.mixed <- function(x, showFormat = FALSE, shortTypes = TRUE, usePillar = FALSE, ...) {

  data <- vctrs::vec_data(x)
  return(vapply(X = data, FUN = formatMixedHelper, FUN.VALUE = character(1L), showFormat = showFormat, shortTypes = shortTypes, usePillar = usePillar))

}

#' @export
# So we don't need I(...) when putting mixed in a data.frame
as.data.frame.mixed <- function(x, row.names = NULL, optional = FALSE, ...) {
  as.data.frame.AsIs(x, row.names = row.names, optional = optional, ...)
}
