.jaspDecodeContext <- function(columnEncoderContext = NULL, factors = list(),
                               source = "manual") {
  factors <- .normalizeJaspFactorMappings(factors, columnEncoderContext)

  list(
    version = 1L,
    columnEncoderContext = columnEncoderContext,
    factors = factors,
    source = source,
    warningState = new.env(parent = emptyenv())
  )
}

.serializableJaspDecodeContext <- function(decodeContext) {
  decodeContext <- .normalizeJaspDecodeContext(decodeContext)
  list(
    version = decodeContext[["version"]],
    columnEncoderContext = decodeContext[["columnEncoderContext"]],
    factors = decodeContext[["factors"]],
    source = decodeContext[["source"]]
  )
}

.withJaspDecodeContextDecoder <- function(decodeContext, expr) {
  if (is.null(decodeContext))
    return(eval.parent(substitute(expr)))

  decodeContext <- .normalizeJaspDecodeContext(decodeContext)

  oldStrict <- .globalBinding(".decodeColNamesStrict")
  oldLax    <- .globalBinding(".decodeColNamesLax")
  on.exit({
    .restoreGlobalBinding(".decodeColNamesStrict", oldStrict)
    .restoreGlobalBinding(".decodeColNamesLax", oldLax)
  }, add = TRUE)

  assign(".decodeColNamesStrict", .decodeJaspColumnsStrict(decodeContext), envir = .GlobalEnv)
  assign(".decodeColNamesLax",    .decodeJaspColumnsLax(decodeContext),    envir = .GlobalEnv)

  eval.parent(substitute(expr))
}

.globalBinding <- function(name) {
  exists <- exists(name, envir = .GlobalEnv, inherits = FALSE)
  list(
    exists = exists,
    value = if (exists) get(name, envir = .GlobalEnv, inherits = FALSE) else NULL
  )
}

.restoreGlobalBinding <- function(name, binding) {
  if (isTRUE(binding[["exists"]])) {
    assign(name, binding[["value"]], envir = .GlobalEnv)
  } else if (exists(name, envir = .GlobalEnv, inherits = FALSE)) {
    rm(list = name, envir = .GlobalEnv)
  }
  invisible(NULL)
}

.decodeJaspColumnsStrict <- function(decodeContext) {
  force(decodeContext)
  function(x) {
    if (!is.character(x) || length(x) == 0L)
      return(x)

    .decodeJaspColumnText(x, decodeContext)
  }
}

.decodeJaspColumnsLax <- function(decodeContext) {
  force(decodeContext)
  function(x) .decodeJaspColumnText(x, decodeContext)
}

.currentJaspDecodeContext <- function() {
  columnEncoderContext <- NULL
  requestedDataset <- NULL

  if (requireNamespace("jaspSyntax", quietly = TRUE)) {
    requestedDataset <- tryCatch(
      getExportedValue("jaspSyntax", "readRequestedDataset")(decode = FALSE, normalize = FALSE),
      error = function(e) NULL
    )
    columnEncoderContext <- tryCatch(
      getExportedValue("jaspSyntax", "columnEncoderContext")(),
      error = function(e) NULL
    )
  }

  .jaspDecodeContext(
    columnEncoderContext = columnEncoderContext,
    factors = .jaspFactorMappingsFromDataset(requestedDataset, columnEncoderContext),
    source = "jaspSyntax"
  )
}

.normalizeJaspDecodeContext <- function(decodeContext = NULL) {
  if (is.null(decodeContext))
    return(.currentJaspDecodeContext())

  decodeContext[["factors"]] <- .normalizeJaspFactorMappings(
    decodeContext[["factors"]],
    decodeContext[["columnEncoderContext"]]
  )
  if (is.null(decodeContext[["version"]]))
    decodeContext[["version"]] <- 1L
  if (is.null(decodeContext[["source"]]))
    decodeContext[["source"]] <- "unknown"
  if (!is.environment(decodeContext[["warningState"]]))
    decodeContext[["warningState"]] <- new.env(parent = emptyenv())

  decodeContext
}

.normalizeJaspFactorMappings <- function(factorMappings = NULL, columnEncoderContext = NULL) {
  if (is.null(factorMappings) || length(factorMappings) == 0L)
    return(list())

  normalized <- list()
  for (fieldName in names(factorMappings)) {
    if (!.isNonEmptyString(fieldName))
      next

    valueMap <- factorMappings[[fieldName]]
    if (is.list(valueMap) && !is.null(valueMap[["levels"]]))
      valueMap <- valueMap[["levels"]]
    if (!is.character(valueMap) || is.null(names(valueMap)))
      next

    valid <- !is.na(valueMap) & !is.na(names(valueMap)) & nzchar(names(valueMap))
    valueMap <- valueMap[valid]
    valueMap <- .decodeJaspColumnText(valueMap, columnEncoderContext)
    if (length(valueMap) == 0L)
      next

    aliases <- unique(c(
      fieldName,
      .decodeJaspColumnText(fieldName, columnEncoderContext)
    ))
    aliases <- aliases[!is.na(aliases) & nzchar(aliases)]
    for (alias in aliases)
      normalized[[alias]] <- valueMap
  }

  normalized
}

.jaspFactorMappingsFromDataset <- function(requestedDataset, columnEncoderContext = NULL) {
  if (!is.data.frame(requestedDataset))
    return(list())

  factorMappings <- list()
  for (columnName in names(requestedDataset)) {
    column <- requestedDataset[[columnName]]
    if (!is.factor(column))
      next

    factorMappings[[columnName]] <- stats::setNames(
      as.character(levels(column)),
      as.character(seq_along(levels(column)))
    )
  }

  .normalizeJaspFactorMappings(factorMappings, columnEncoderContext)
}

.jaspEncodedColumnTokenPattern <- function() "(JaspColumn_[[:alnum:]_]+_Encoded|JaspExtraOptions_[[:alnum:]_]+_Encoded|jaspColumn[0-9]+)"

.containsJaspEncodedTokens <- function(x) {
  if (!is.character(x) || length(x) == 0L)
    return(FALSE)
  any(grepl(.jaspEncodedColumnTokenPattern(), x, perl = TRUE), na.rm = TRUE)
}

.decodeJaspColumnText <- function(x, columnEncoderContext = NULL) {
  if (!is.character(x) || length(x) == 0L)
    return(x)
  if (!.containsJaspEncodedTokens(x))
    return(x)

  if (is.list(columnEncoderContext) && "columnEncoderContext" %in% names(columnEncoderContext))
    columnEncoderContext <- columnEncoderContext[["columnEncoderContext"]]
  if (is.null(columnEncoderContext))
    return(x)

  decoded <- tryCatch(
    getExportedValue("jaspSyntax", "decodeColumnText")(x, columnEncoderContext),
    error = function(e) {
      stop(
        "jaspBase result decoding requires a working native jaspSyntax column decoder: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )
  if (is.character(decoded) && length(decoded) == length(x)) {
    names(decoded) <- names(x)
    decoded
  } else {
    stop("Native jaspSyntax column decoder returned an invalid result.", call. = FALSE)
  }
}

.decodeJaspFactorValues <- function(x, fieldName = NULL, decodeContext) {
  if (is.null(fieldName) || length(fieldName) != 1L || is.na(fieldName) || !nzchar(fieldName))
    return(x)

  candidateFields <- unique(c(
    fieldName,
    .decodeJaspColumnText(fieldName, decodeContext)
  ))
  candidateFields <- candidateFields[!is.na(candidateFields) & nzchar(candidateFields)]

  valueMap <- NULL
  for (candidateField in candidateFields) {
    valueMap <- decodeContext[["factors"]][[candidateField]]
    if (!is.null(valueMap))
      break
  }
  if (is.null(valueMap))
    return(x)

  keys <- as.character(x)
  matched <- !is.na(keys) & keys %in% names(valueMap)
  if (!any(matched))
    return(x)

  out <- as.character(x)
  out[matched] <- unname(valueMap[keys[matched]])
  out
}

.warnIfMissingJaspDecodeContext <- function(x, decodeContext) {
  if (!.containsJaspEncodedTokens(x))
    return(invisible(NULL))
  if (isTRUE(decodeContext[["warningState"]][["missingContext"]]))
    return(invisible(NULL))

  decodeContext[["warningState"]][["missingContext"]] <- TRUE
  warning(
    "JASP result output still contains encoded column tokens, but no analysis decode context was available. ",
    "This should only happen for legacy or externally constructed state.",
    call. = FALSE
  )
  invisible(NULL)
}

.decodeJaspText <- function(x, decodeContext = NULL, fieldName = NULL) {
  if (!is.character(x) || length(x) == 0L)
    return(x)

  decodeContext <- .normalizeJaspDecodeContext(decodeContext)
  x <- .decodeJaspFactorValues(x, fieldName = fieldName, decodeContext = decodeContext)
  x <- .decodeJaspColumnText(x, decodeContext)

  .warnIfMissingJaspDecodeContext(x, decodeContext)
  x
}

.isJaspDecodedPlotObject <- function(x) {
  isTRUE(attr(x, "jaspDecodedResultObject", exact = TRUE))
}

.markJaspDecodedPlotObject <- function(x) {
  attr(x, "jaspDecodedResultObject") <- TRUE
  x
}

.decodeJaspResultState <- function(state, decodeContext = NULL) {
  decodeContext <- .normalizeJaspDecodeContext(decodeContext)
  if (!is.list(state) || is.null(state[["figures"]]))
    return(.decodeJaspRObject(state, decodeContext = decodeContext))

  for (figureIndex in seq_along(state[["figures"]])) {
    figure <- state[["figures"]][[figureIndex]]
    if (is.list(figure)) {
      if (!is.null(figure[["obj"]]))
        figure[["obj"]] <- .decodeJaspPlotObject(figure[["obj"]], returnGrob = FALSE, decodeContext = decodeContext)
      otherFields <- setdiff(names(figure), "obj")
      for (field in otherFields)
        figure[[field]] <- .decodeJaspRObject(figure[[field]], fieldName = field, decodeContext = decodeContext)
      names(figure) <- .decodeJaspText(names(figure), decodeContext = decodeContext)
      state[["figures"]][[figureIndex]] <- figure
    }
  }

  # `other` contains jaspState payloads: arbitrary analysis-owned R objects
  # restored into the next run. Keep those objects opaque. Display/replay state
  # that JASP owns lives under `figures` and is decoded above.

  otherFields <- setdiff(names(state), c("figures", "other"))
  for (field in otherFields)
    state[[field]] <- .decodeJaspRObject(state[[field]], fieldName = field, decodeContext = decodeContext)

  names(state) <- .decodeJaspText(names(state), decodeContext = decodeContext)
  state
}

.decodeJaspPlotObject <- function(plot, returnGrob = FALSE, decodeContext = NULL) {
  if (!isTRUE(returnGrob) && .isJaspDecodedPlotObject(plot))
    return(plot)

  decodeContext <- .normalizeJaspDecodeContext(decodeContext)
  decoded <- decodeplot(plot, returnGrob = returnGrob, decodeContext = decodeContext)

  if (!isTRUE(returnGrob))
    decoded <- .markJaspDecodedPlotObject(decoded)

  decoded
}

.decodeJaspRObjectFromCpp <- function(jaspObject, decodeContext = NULL) {
  .withJaspDecodeContextDecoder(decodeContext, {
    .decodeJaspRObject(jaspObject$toRObject(), decodeContext = decodeContext)
  })
}
