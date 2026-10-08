#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

#' @importFrom stats na.omit

fromJSON <- function(x) jsonlite::fromJSON(x, TRUE, FALSE, FALSE)
toJSON   <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA, null="null")

# This is a temporary fix
# TODO: remove it when R will solve this problem!
gettextf <- function(fmt, ..., domain = NULL)  {
  return(sprintf(gettext(fmt, domain = domain), ...))
}

loadJaspResults <- function(name) {
  create_cpp_jaspResults(name, .retrieveState())
}

finishJaspResults <- function(jaspResultsCPP, calledFromAnalysis = TRUE) {

  jaspResultsCPP$prepareForWriting()

  newState <- list(
    figures = jaspResultsCPP$getPlotObjectsForState(),
    other   = jaspResultsCPP$getOtherObjectsForState()
  )

  jaspResultsCPP$relativePathKeep <- .saveState(newState)$relativePath

  returnThis <- NULL
  if (calledFromAnalysis) {
    returnThis <- list(keep = jaspResultsCPP$getKeepList()) #To keep the old keep-code functional we return it like this

    jaspResultsCPP$complete() #sends last results to desktop, changes status to complete and saves results to json in tempfiles

  } else {

    jaspResultsCPP$saveResults()
    jaspResultsCPP$finishWriting()

  }

  return(returnThis)
}


sendFatalErrorMessage <- function(name, title, msg)
{
  jaspResultsCPP        <- loadJaspResults(name)
  jaspResultsCPP$title  <- title

  jaspResultsCPP$setErrorMessage(msg, "fatalError")
  jaspResultsCPP$send()
}


#' @export
runJaspResults <- function(name, title, dataKey, options, stateKey, functionCall = name, preloadData=FALSE, multiDataSetJson = NULL, datasets = NULL) {
  # resets jaspGraphs::graphOptions & options after this function finishes
  setOptionsCleanupHook()

  # let's disable this for now
  # if (identical(.Platform$OS.type, "windows"))
  #   compiler::enableJIT(0)

  setLegacyRng()

  jaspResultsCPP        <- loadJaspResults(name)
  jaspResultsCPP$title  <- title
  jaspResults           <- jaspResultsR$new(jaspResultsCPP)

  jaspResultsCPP$setOptions(options)

  dataKey     <- fromJSON(dataKey)
  options     <- fromJSON(options)
  stateKey    <- fromJSON(stateKey)

  if (base::exists(".requestStateFileNameNative")) {
    location              <- .fromRCPP(".requestStateFileNameNative")
    oldwd                 <- getwd()
    setwd(location$root)
    withr::defer(setwd(oldwd))
  }

  if (! jaspResultsCalledFromJasp()) {
    .numDecimals        <- 3
    .fixedDecimals      <- FALSE
    .normalizedNotation <- TRUE
    .exactPValues       <- FALSE
  }

  analysis    <- eval(parse(text=functionCall))
  dataset     <- NULL
  # datasets comes in either from the caller (R wrapper handout) or from the queued engine
  # reads below; do NOT null the parameter here.

  multiDataSet <- !is.null(datasets) || .isMultiDataSetJson(multiDataSetJson)

  if (!is.null(datasets)) {
    # R-side handout (jaspSyntax wrappers / jaspTools): the user delivered the datasets as a
    # named list themselves, which already carries all the information multiDataSetJson would:
    # the names are the dataset ids (a title attribute is optional), so everything is derived
    # from here and no queued engine reads are involved.
    names(datasets) <- as.character(names(datasets))
    if (is.null(attr(datasets, "dataSetNames")))
      attr(datasets, "dataSetNames") <- as.list(names(datasets))
    attr(datasets, "dataSetIds") <- as.list(names(datasets))  # handout keys are dataset ids

    .multiDataSetMode(TRUE)
    on.exit(.multiDataSetMode(FALSE), add = TRUE)

  } else if (multiDataSet) {
    # Multi-dataset aware run: the engine queued every dataset this analysis references (see
    # Engine::runAnalysis), one read per queued call below. Keyed by dataset id, with the user facing
    # titles attached as an attribute; the datasets arrive as a parameter, readDataSet* is broken here.
    dsInfo <- fromJSON(multiDataSetJson)
    ids    <- as.character(dsInfo$ids)   # slice keys: the resolved FILTER id of each slice

    .multiDataSetMode(TRUE)
    on.exit(.multiDataSetMode(FALSE), add = TRUE)

    # The slices arrive with their column names ENCODED by the same per-dataset encoder that encoded
    # the options (rbridge_readDataSetRequested), so an option value indexes its column in
    # datasets[[key]] as-is - the same encoded namespace a classic single-dataset run lives in.
    # Keys are filter ids (per-form selections are filter selections), one slice per distinct
    # (dataset, filter) the options reference: a form's selection option indexes ITS slice
    # directly with datasets[[as.character(as.integer(options$dataSetA))]]. attr "dataSetIds"
    # maps every slice key back to its dataset id. Result strings are decoded on their way back
    # to the GUI by Engine::sendString.
    datasets <- list()

    for (id in ids)
      datasets[[id]] <- .fromRCPP(".readDataSetRequestedNative")

    attr(datasets, "dataSetNames") <- dsInfo$names
    attr(datasets, "dataSetIds")   <- dsInfo$dataSetIds

  } else if (preloadData)
    dataset <- .fromRCPP(".readDataSetRequestedNative")

  # ensure an analysis always starts with a clean hashtable of computed jasp Objects
  emptyRecomputed()

  analysisResult <-
    tryCatch(
      expr=withCallingHandlers(expr=if (multiDataSet)
                                       analysis(jaspResults=jaspResults, dataset=NULL, options=options, datasets=datasets)
                                     else
                                       analysis(jaspResults=jaspResults, dataset=dataset, options=options), error=.addStackTrace),
      error=function(e) e,
      jaspAnalysisAbort=function(e) e
    )

  if (!jaspResultsCalledFromJasp()) {

    if (inherits(analysisResult, "error")) {

      if (inherits(analysisResult, "validationError")) {
        errorStatus  <- "validationError"
        errorMessage <- analysisResult$message
      } else {
        errorStatus  <- "fatalError"
        error        <- .sanitizeForJson(analysisResult)
        stackTrace   <- .sanitizeForJson(analysisResult$stackTrace)
        stackTrace   <- paste(stackTrace, collapse="<br><br>")
        errorMessage <- .generateErrorMessage(type=errorStatus, error=error, stackTrace=stackTrace)
      }

      jaspResultsCPP$setErrorMessage(errorMessage, errorStatus)
      jaspResultsCPP$send()

    }

    finishJaspResults(jaspResultsCPP)
    return(jaspResults)
  }

  if (inherits(analysisResult, "jaspAnalysisAbort")) {
    jaspResultsCPP$send()
    return("null")
  } else if (inherits(analysisResult, "error")) {

    if (inherits(analysisResult, "validationError")) {
      errorStatus  <- "validationError"
      errorMessage <- analysisResult$message
    } else {
      errorStatus  <- "fatalError"
      error        <- .sanitizeForJson(analysisResult)
      stackTrace   <- .sanitizeForJson(analysisResult$stackTrace)
      stackTrace   <- paste(stackTrace, collapse="<br><br>")
      errorMessage <- .generateErrorMessage(type=errorStatus, error=error, stackTrace=stackTrace)
    }

    jaspResultsCPP$setErrorMessage(errorMessage, errorStatus)
    jaspResultsCPP$send()

    return(paste0("{ \"status\" : \"", errorStatus, "\", \"results\" : { \"title\" : \"error\", \"error\" : 1, \"errorMessage\" : \"", errorMessage, "\" } }", sep=""))
  } else {

    returnThis <- finishJaspResults(jaspResultsCPP)

    json <- try({ toJSON(returnThis) })
    if (isTryError(json))
      return(paste("{ \"status\" : \"error\", \"results\" : { \"error\" : 1, \"errorMessage\" : \"", "Unable to jsonify", "\" } }", sep=""))
    else
      return(json)
  }
}

registerFonts <- function() {
  # This gets called by JASPEngine when settings changes and on `initEnvironment`

  if (requireNamespace("ragg") && requireNamespace("systemfonts")) {

    # To register custom font files shipped with JASP we need the path to the font file.
    # Next the font could be loaded like this:
    #
    # fontName <- "FreeSansJASP"
    # fontFile <- "~/github/jasp-desktop/Desktop/resources/fonts/FreeSans.ttf"
    # systemfonts::register_font(fontName, normalizePath(fontFile))
    # jaspGraphs::setGraphOption("family", fontName)

    if (exists(".resultFont"))
      jaspGraphs::setGraphOption("family", .resultFont)
    else
      warning("registerFonts was called but resultFont does not exist!")

  } else {
    print("R packages 'ragg' and/ or 'systemfonts' are unavailable, falling back to R's default fonts.")
  }
}

#' @export
initEnvironment <- function() {
  packages <- c("BayesFactor") # Add any package that needs pre-loading

  if (identical(.Platform$OS.type, "windows"))
    assignFunctionInPackage(fakeGrDevicesPdf, "pdf", "grDevices") # this fixes the problem that grDevices::pdf() does not work within JASP (https://github.com/jasp-stats/INTERNAL-jasp/issues/682)

  for (package in packages)
    if (base::isNamespaceLoaded(package) == FALSE)
      try(base::loadNamespace(package), silent=TRUE)

  registerFonts()

  if (base::exists(".requestTempRootNameNative")) {
    paths <- .fromRCPP(".requestTempRootNameNative")
    setwd(paths$root)
  } else
    print("Could not set the working directory!")
}

checkPackages <- function() {
  toJSON(.checkPackages())
}

.sanitizeForJson <- function(obj) {
  # Removes elements that are not translatable to json
  #
  # Args:
  # - obj: character string or obj coercible to string (e.g. a try-error)
  #
  # Return:
  # - character string ready to be put into toJSON
  #
  str <- as.character(obj)
  str <- gsub("\"", "'", str, fixed=TRUE)
  str <- gsub("\\n", "<br>", str)
  str <- gsub("\\\\", "", str)
  return(str)
}

#' @export
isTryError <- function(obj){
  if (is.list(obj)){
    return(any(sapply(obj, function(obj) {
      inherits(obj, "try-error")
    }))
    )
  } else {
    return(any(sapply(list(obj), function(obj){
      inherits(obj, "try-error")
    })))
  }
}

.readDataSetCleanNAs <- function(cols) {
  cols <- cols[!is.na(cols)]

  if(length(cols) == 0)
    return(NULL);
  return(cols);
}

# ---------------------------------------------------------------------------
# Multi-dataset aware runs
#
# A multiDataSetAware analysis gets every dataset it needs as the `datasets` parameter of
# runJaspResults (and thus of the analysis function), instead of reading "the" dataset through
# the readDataSet* functions below. While such a run is active those read functions stop: the
# very notion of one current dataset is meaningless when an analysis runs on several.
# ---------------------------------------------------------------------------

.multiDataSetState <- new.env(parent = emptyenv())

.multiDataSetMode <- function(set = NULL) {
  if (!is.null(set)) .multiDataSetState$active <- set
  isTRUE(.multiDataSetState$active)
}

.stopIfMultiDataSetMode <- function(what) {
  if (.multiDataSetMode())
    stop(sprintf(paste0("%s() is not available in multi-dataset aware analyses; those get the datasets they ",
                        "need as the `datasets` argument (a named list, keyed by dataset id; ",
                        "attr(datasets, \"dataSetNames\") maps those ids to the dataset titles)."), what),
         call. = FALSE)
}

.isMultiDataSetJson <- function(multiDataSetJson) {
  !is.null(multiDataSetJson) && !identical(multiDataSetJson, "") && !identical(multiDataSetJson, "null")
}

# ---- dataset-aware routing of encoded column names ---------------------------------------------
#
# Options and datasets meet in the ENCODED namespace: the engine encodes every variable option
# against the encoder of the dataset it was selected from, and rbridge_readDataSetRequested encodes
# the slice's column names against that very same encoder. Both sides are therefore the same
# strings - "JASPColumn_<dataSetId>_<counter>_Encoded" (DataSet::setupEncoderPrefix embeds the
# dataset id, the "_Encoded" postfix is the encoder default) - and an analysis indexes its data
# with the option value as-is: datasets[[id]][[value]]. Results are decoded on the way back to the
# GUI (Engine::sendString runs them past the encoder of every dataset the analysis references), so
# nothing in R ever has to decode; the only thing worth recovering here is the dataset id, which
# tells you WHICH element of `datasets` a value belongs to. Legacy names ("JaspColumn_<counter>",
# encoded before ids were embedded) and plain syntax-mode names carry no id and belong to the
# primary dataset.

.dataSetIdFromEncodedOne <- function(encoded) {
  if (!is.character(encoded) || length(encoded) != 1L || is.na(encoded))
    return(NA_integer_)

  # The real form is "JASPColumn_<dataSetId>_<counter>_Encoded"; a type suffix
  # ("...scale/.ordinal/.nominal") and the encoder's temporary "_For_Replacement" postfix are
  # tolerated, though neither survives into an option value.
  name <- sub("_(Encoded|For_Replacement)$", "", sub("\\.(scale|ordinal|nominal)$", "", encoded))

  match <- regmatches(name, regexec("^JASPColumn_([0-9]+)_([0-9]+)$", name))[[1]]
  if (length(match) == 3L)
    return(as.integer(match[2]))

  NA_integer_
}

# Find the slice key of `datasets` that carries the given dataset id: the id itself when the
# list is keyed by dataset (jaspTools-style handout), else the first filter-keyed slice of that
# dataset (engine queue branch: one slice per distinct dataset+filter). NULL when absent.
.sliceKeyForDataSet <- function(datasets, idStr) {
  idStr <- as.character(idStr)
  if (idStr %in% names(datasets))
    return(idStr)

  dsIds <- attr(datasets, "dataSetIds")

  if (!is.null(dsIds)) {
    hit <- which(vapply(dsIds, function(x) as.character(x) == idStr, logical(1)))
    if (length(hit) > 0)
      return(names(dsIds)[[hit[[1]]]])
  }

  NULL
}

#' @title dataSetIdFromEncoded
#'
#' @description Recover the dataset id from an encoded column name.
#'
#' @param encoded character vector, encoded column names like they show up in the options of a
#'   multiDataSetAware analysis.
#'
#' @details
#' Encoded names embed the id of the dataset they belong to
#' ("JASPColumn_<dataSetId>_<counter>_Encoded"), so this routes any option value to its dataset:
#' `datasets[[as.character(dataSetIdFromEncoded(x))]]`. Names without an embedded id (legacy
#' "JaspColumn_<counter>" or plain column names) yield NA; use [getDataSetFor()] if you want those
#' to fall back to the primary dataset.
#'
#' @return integer vector, the dataset ids (NA where no id is embedded).
#'
#' @export
dataSetIdFromEncoded <- function(encoded) vapply(encoded, .dataSetIdFromEncodedOne, integer(1L), USE.NAMES = FALSE)

#' @title dataSetNameFromEncoded
#'
#' @description Recover the title of the dataset an encoded column name belongs to.
#'
#' @param encoded character vector, encoded column names.
#' @param datasets the `datasets` list of a multiDataSetAware analysis (ids as names, titles in
#'   `attr(datasets, "dataSetNames")`).
#'
#' @details
#' Looks up `dataSetIdFromEncoded(encoded)` in `attr(datasets, "dataSetNames")`. Without an embedded
#' id (legacy or plain names) the title of the primary (first) dataset is returned, which is what
#' those names belong to.
#'
#' @return character vector with the dataset titles (NA when the id is not in `datasets`).
#'
#' @export
dataSetNameFromEncoded <- function(encoded, datasets) {
  titles <- attr(datasets, "dataSetNames")
  ids    <- dataSetIdFromEncoded(encoded)

  vapply(seq_along(encoded), function(i) {
    id <- ids[[i]]
    key <- if (is.na(id)) names(datasets)[[1]] else .sliceKeyForDataSet(datasets, as.character(id))
    if (!is.null(titles) && !is.null(key) && key %in% names(titles)) as.character(titles[[key]]) else NA_character_
  }, character(1L), USE.NAMES = FALSE)
}

#' @title getDataSetFor
#'
#' @description The dataset a particular (encoded) column name came from.
#'
#' @param encoded character, one encoded column name.
#' @param datasets the `datasets` list of a multiDataSetAware analysis.
#' @param default dataset to return when `encoded` carries no dataset id and no dataset has such a
#'   column; by default the primary (first) dataset itself.
#'
#' @details
#' `datasets[[as.character(dataSetIdFromEncoded(encoded))]]` when the embedded id is one of the
#' datasets handed over. Without an id (or with a dangling one) the primary dataset wins when it
#' has such a column, else the first dataset that does (syntax-mode handovers pass plain names and
#' cannot encode per dataset), else `default`.
#'
#' @return the data.frame of the dataset this column belongs to.
#'
#' @export
getDataSetFor <- function(encoded, datasets, default = datasets[[1]]) {
  id <- dataSetIdFromEncoded(encoded)

  if (!is.na(id)) {
    key <- .sliceKeyForDataSet(datasets, as.character(id))
    if (!is.null(key) && key %in% names(datasets))   # [[ on a named list ERRORS for absent names, guard properly
      return(datasets[[key]])
  }

  name <- as.character(encoded)

  primary <- datasets[[1]]
  if (!is.null(primary) && name %in% names(primary))
    return(primary)

  for (candidate in datasets)
    if (name %in% names(candidate))
      return(candidate)

  default
}

#' @title getDataSetColumn
#'
#' @description The column an option value refers to, straight out of the right dataset.
#'
#' @param encoded character, one encoded column name from the options of a multiDataSetAware
#'   analysis.
#' @param datasets the `datasets` list of that analysis.
#'
#' @details Short for `getDataSetFor(encoded, datasets)[[encoded]]`: options and column names share
#' the encoded namespace, so no decoding is involved. When the routed dataset does not have the
#' column (deleted after the options were bound, say), every other dataset is scanned before
#' giving up with NULL.
#'
#' @return the column (vector), or NULL.
#'
#' @export
getDataSetColumn <- function(encoded, datasets) {
  name <- as.character(encoded)

  dataSet <- getDataSetFor(encoded, datasets)
  if (!is.null(dataSet) && name %in% names(dataSet))
    return(dataSet[[name]])

  for (candidate in datasets)
    if (name %in% names(candidate))
      return(candidate[[name]])

  NULL
}

#' @title readDataSetByVariableTypes
#'
#' @param options options from QML.
#' @param keys character, option name(s) that contain variables in the dataset.
#' @param exclude.na.listwise character, column names for which any missing values will cause that row to be excluded.
#'
#' @details
#' `readDataSetByVariableTypes` automatically removes keys that are empty lists or empty strings, unlike `.readDataSetToEnd` which would throw an error.
#'
#' @export
readDataSetByVariableTypes <- function(options, keys, exclude.na.listwise = NULL) {

  .stopIfMultiDataSetMode("readDataSetByVariableTypes")

  if (!is.list(options))
    stop(".readDataSetByVariableTypes received `options` that are not a list.")

  if (!is.character(keys))
    stop(".readDataSetByVariableTypes received `keys` that are not a character vector")

  # TODO: use the values below for unit tests!
  # test error 1: missing <key>.types
  # options <- list(
  #   variables       = c("contNormal", "contGamma"),
  #   variables.types = c("scale", "scale"),
  #   covariate       = "contBinom",
  #   factor          = "contBinom"
  # )
  # keys <- c("variables", "covariate", "factor")

  # test error 2: repeated variable names
  # options <- list(
  #   variables       = c("contNormal", "contGamma", "debString"),
  #   variables.types = c("scale", "scale", "nominal"),
  #   covariate       = "contNormal",
  #   covariate.types = c("ordinal"),
  #   covariate2       = "contGamma",
  #   covariate2.types = c("ordinal")
  # )
  # keys <- c("variables", "covariate", "covariate2")

  # test 3: this one should not error
  # options <- list(
  #   variables       = c("contNormal", "contGamma", "debString"),
  #   variables.types = c("scale", "scale", "nominal"),
  #   covariate       = "contExpon",
  #   covariate.types = c("ordinal"),
  #   covariate2       = "contBinom",
  #   covariate2.types = c("ordinal")
  # )
  # keys <- c("variables", "covariate", "covariate2")

  # automatically remove keys that are empty lists or empty strings
  validKeys <- vapply(keys, \(key) !identical(options[[key]], "") && !identical(options[[key]], list()), FUN.VALUE = logical(1L))
  if (!any(validKeys))
    return(data.frame())

  keys <- keys[validKeys]

  variableNames <- options[keys]
  variableTypes <- options[paste0(keys, ".types")]

  lengthsVars  <- lengths(variableNames)
  lengthsTypes <- lengths(variableTypes)

  # are any keys missing the element key.types in options?
  mismatch <- which(lengthsVars != lengthsTypes)
  if (length(mismatch) > 0L)
    stop(".readDataSetByVariableTypes received the following key(s) which are missing the types:\n\n", paste0("\"", names(mismatch), "\"", collapse = ", "))

  variableNamesVec <- unlist(variableNames, use.names = FALSE)

  if (!all(is.character(variableNamesVec)))     stop(".readDataSetByVariableTypes received key(s) for which the type in options[[\"{key}\"]] was not a character vector")
  if (anyNA(variableNamesVec))                  stop(".readDataSetByVariableTypes received key(s) for which the type in options[[\"{key}\"]] was NA")

  # are any keys containing the same variable twice?
  if (anyDuplicated(variableNamesVec)) {

    names(variableNamesVec) <- rep(keys, lengthsVars)

    duplicatedVariables <- unique(variableNamesVec[duplicated(variableNamesVec)])

    # the width here will probably be overkill because it looks at encoded columns, but it'll look pretty nopntheless
    desiredWidth <- max(nchar(duplicatedVariables), 8L) # 8 == nchar("variable") from the header below
    rows <- vapply(duplicatedVariables, \(var) {
      paste0(formatC(var, width = desiredWidth), " | ", paste(sort(names(variableNamesVec[variableNamesVec == var])), collapse = ", "))
    }, FUN.VALUE = character(1L))
    header <- paste0(formatC("variable", width = desiredWidth), " | ", "key(s)\n")
    separator <- paste0(strrep("-", desiredWidth), "-|-")
    separator <- paste0(separator, strrep("-", max(nchar(rows)) - nchar(separator)), "\n")

    stop(".readDataSetByVariableTypes received the same variable(s) more than once.\nEnsure this cannot happen or call .readDataSetByVariableTypes multiple times. \n\n",
         header, separator, paste(rows, collapse = "\n"))

  }

  variableTypesVec <- unlist(variableTypes, use.names = FALSE)

  allowedTypes <- c("scale", "ordinal", "nominal")
  if (!all(is.character(variableTypesVec)))     stop(".readDataSetByVariableTypes received key(s) for which the type in options[[\"{key}.types\"]] was not a character vector")
  if (anyNA(variableTypesVec))                  stop(".readDataSetByVariableTypes received key(s) for which the type in options[[\"{key}.types\"]] was NA")
  if (!all(variableTypesVec %in% allowedTypes)) stop(".readDataSetByVariableTypes received key(s) for which the type in options[[\"{key}.types\"]] was not one of \"scale\", \"ordinal\", or \"nominal\"")

  variablesSplitByType <- split(variableNamesVec, variableTypesVec)

  return(.readDataSetToEnd(
    columns.as.numeric  = variablesSplitByType[["scale"]],
    columns.as.ordinal  = variablesSplitByType[["ordinal"]],
    columns.as.factor   = variablesSplitByType[["nominal"]],
    exclude.na.listwise = exclude.na.listwise
  ))

}

#' @export
.readDataSetToEnd <- function(columns=NULL, columns.as.numeric=NULL, columns.as.ordinal=NULL, columns.as.factor=NULL, all.columns=FALSE, exclude.na.listwise=NULL, ...) {

  .stopIfMultiDataSetMode("readDataSet")

  columns              <- .readDataSetCleanNAs(columns)
  columns.as.numeric   <- .readDataSetCleanNAs(columns.as.numeric)
  columns.as.ordinal   <- .readDataSetCleanNAs(columns.as.ordinal)
  columns.as.factor    <- .readDataSetCleanNAs(columns.as.factor)
  exclude.na.listwise  <- .readDataSetCleanNAs(exclude.na.listwise)

  if (all.columns == FALSE && is.null(columns) && is.null(columns.as.numeric) && is.null(columns.as.ordinal) && is.null(columns.as.factor))
    return (data.frame())

  dataset <- .fromRCPP(".readDatasetToEndNative", unlist(columns), unlist(columns.as.numeric), unlist(columns.as.ordinal), unlist(columns.as.factor), all.columns != FALSE)
  dataset <- .excludeNaListwise(dataset, exclude.na.listwise)

  dataset
}

#' @export
.readFullDataset <- function(exclude.na.listwise=NULL, ...) {

  .stopIfMultiDataSetMode("readFullDataset")

  exclude.na.listwise  <- .readDataSetCleanNAs(exclude.na.listwise)

  dataset <- .fromRCPP(".readFullDatasetToEnd")
  dataset <- .excludeNaListwise(dataset, exclude.na.listwise)

  dataset
}

#' @export
.readDataSetHeader <- function(columns=NULL, columns.as.numeric=NULL, columns.as.ordinal=NULL, columns.as.factor=NULL, all.columns=FALSE, ...) {

  .stopIfMultiDataSetMode("readDataSetHeader")

  columns              <- .readDataSetCleanNAs(columns)
  columns.as.numeric   <- .readDataSetCleanNAs(columns.as.numeric)
  columns.as.ordinal   <- .readDataSetCleanNAs(columns.as.ordinal)
  columns.as.factor    <- .readDataSetCleanNAs(columns.as.factor)

  if (all.columns == FALSE && is.null(columns) && is.null(columns.as.numeric) && is.null(columns.as.ordinal) && is.null(columns.as.factor))
    return (data.frame())

  dataset <- .fromRCPP(".readDataSetHeaderNative", unlist(columns), unlist(columns.as.numeric), unlist(columns.as.ordinal), unlist(columns.as.factor), all.columns != FALSE)

  dataset
}

#' @export
.vdf <- function(df, columns=NULL, columns.as.numeric=NULL, columns.as.ordinal=NULL, columns.as.factor=NULL, all.columns=FALSE, exclude.na.listwise=NULL, ...) {
  new.df <- NULL
  namez <- NULL

  for (column.name in columns) {

    column <- df[[column.name]]

    if (is.null(new.df)) {
      new.df <- data.frame(column)
    } else {
      new.df <- data.frame(new.df, column)
    }

    namez <- c(namez, column.name)
  }

  for (column.name in columns.as.ordinal) {

    column <- as.ordered(df[[column.name]])

    if (length(column) == 0) {
      .quitAnalysis("Error: no data! Check for missing values.")
    }
    if (is.null(new.df)) {
      new.df <- data.frame(column)
    } else {
      new.df <- data.frame(new.df, column)
    }

    namez <- c(namez, column.name)
  }

  for (column.name in columns.as.factor) {

    column <- as.factor(df[[column.name]])

    if (length(column) == 0) {
      .quitAnalysis("Error: no data! Check for missing values.")
    }
    if (is.null(new.df)) {
      new.df <- data.frame(column)
    } else {
      new.df <- data.frame(new.df, column)
    }

    namez <- c(namez, column.name)
  }

  for (column.name in columns.as.numeric) {

    column <- as.numeric(as.character(df[[column.name]]))

    if (length(column) == 0) {
      .quitAnalysis("Error: no data! Check for missing values.")
    }
    if (is.null(new.df)) {
      new.df <- data.frame(column)
    } else {
      new.df <- data.frame(new.df, column)
    }

    namez <- c(namez, column.name)
  }

  if (is.null(new.df))
    return (data.frame())

  names(new.df) <- namez

  new.df <- .excludeNaListwise(new.df, exclude.na.listwise)

  new.df
}

#' Exclude rows with missing values (listwise deletion)
#'
#' @param dataset dataframe containing the dataset.
#' @param columns a character vector with column names, or NULL to remove all rows with missing values.
#'
#' @return a dataframe with rows that contain missing values removed
#' @export
excludeNaListwise <- function(dataset, columns = NULL) {

  if (length(dataset) == 0 || nrow(dataset) == 0)
    return(dataset)

  if (!is.data.frame(dataset))
    stop("excludeNaListwise: the `dataset` argument must be a dataframe.")

  if (is.null(columns))
    return(dataset[stats::complete.cases(dataset), , drop = FALSE])

  if (!is.character(columns))
    stop("excludeNaListwise: the `columns` argument must be a character vector.")

  if (!all(columns %in% colnames(dataset)))
    stop("excludeNaListwise: the following columns did not appear in the dataset:", setdiff(columns, colnames(dataset)))

  return(dataset[stats::complete.cases(dataset[columns]), , drop = FALSE])
}

.excludeNaListwise <- function(dataset, exclude.na.listwise) {

  if ( ! is.null(exclude.na.listwise)) {

    rows.to.exclude <- c()

    for (col in exclude.na.listwise)
      rows.to.exclude <- c(rows.to.exclude, which(is.na(dataset[[col]])))

    rows.to.exclude <- unique(rows.to.exclude)

    rows.to.keep <- 1:dim(dataset)[1]
    rows.to.keep <- rows.to.keep[ ! rows.to.keep %in% rows.to.exclude]

    new.dataset <- dataset[rows.to.keep,]

    if (!is.data.frame(new.dataset)) {   # HACK! if only one column, R turns it into a factor (because it's stupid)

      dataset <- na.omit(dataset)

    } else {

      dataset <- new.dataset
    }
  }

  dataset
}

#' @export
.shortToLong <- function(dataset, rm.factors, rm.vars, bt.vars, dependentName = "dependent", subjectName = "subject") {

  f  <- rm.factors[[length(rm.factors)]]
  df <- data.frame(factor(unlist(f$levels), unlist(f$levels)))

  names(df) <- f$name

  row.count <- dim(df)[1]

  i <- length(rm.factors) - 1
  while (i > 0) {

    f <- rm.factors[[i]]

    new.df <- df

    j <- 2
    while (j <= length(f$levels)) {

      new.df <- rbind(new.df, df)
      j <- j + 1
    }

    df <- new.df

    row.count <- dim(df)[1]

    cells <- rep(unlist(f$levels), each=row.count / length(f$levels))
    cells <- factor(cells, unlist(f$levels))

    df <- cbind(cells, df)
    names(df)[[1]] <- f$name

    i <- i - 1
  }

  ds <- subset(dataset, select=rm.vars)
  ds <- t(as.matrix(ds))

  dependentDf <- data.frame(x = as.numeric(c(ds)))
  colnames(dependentDf) <- dependentName
  df <- cbind(df, dependentDf)

  for (bt.var in bt.vars) {

    cells <- rep(dataset[[bt.var]], each=row.count)
    new.col <- list()
    new.col[[bt.var]] <- cells

    df <- cbind(df, new.col)
  }

  subjects <- 1:(dim(dataset)[1])
  subjects <- as.factor(rep(subjects, each=row.count))

  subjectDf <- data.frame(x = subjects)
  colnames(subjectDf) <- subjectName
  df <- cbind(df, subjectDf)

  df
}

jaspResultsStrings <- function() {
  # jaspResults does not exist as an R package within JASP, so we cannot use its po folder
  # and we add the strings that need to be translated here.
  gettext("<em>Note.</em>")
}

#' @export
.fromRCPP <- function(x, ...) {

  if (length(x) != 1 || ! is.character(x)) {
    stop("Invalid type supplied to .fromRCPP, expected character")
  }

  collection <- c(
    ".requestTempFileNameNative",
    ".requestTempRootNameNative",
    ".readDatasetToEndNative",
    ".readDataSetHeaderNative",
    ".readDataSetRequestedNative",
    ".requestStateFileNameNative",
    ".baseCitation",
    ".ppi",
    ".imageBackground")

  if (! x %in% collection) {
    stop("Unknown RCPP object")
  }

  if (exists(x)) {
    obj <- eval(parse(text = x))
  } else {
    location <- utils::getAnywhere(x)
    if (length(location[["objs"]]) == 0) {
      stop(paste0("Could not locate ",x," in environment (.fromRCPP)"))
    }
    obj <- location[["objs"]][[1]]
  }

  if (is.function(obj)) {
    args <- list(...)
    do.call(obj, args)
  } else {
    return(obj)
  }

}

.saveState <- function(state) {
  location <- .fromRCPP(".requestStateFileNameNative")
  relativePath <- location$relativePath

  # when run through jaspTools do not save the state, but store it internally
  if ("jaspTools" %in% loadedNamespaces()) {
    # fool renv so it does not try to install jaspTools
    .setInternal <- utils::getFromNamespace(".setInternal", asNamespace("jaspTools"))
    .setInternal("state", state)
    return(list(relativePath = relativePath))
  }

  try(suppressWarnings(base::save(state, file=relativePath, compress=FALSE)), silent = FALSE)

  return(list(relativePath = relativePath))
}

.retrieveState <- function() {

  state <- NULL

  if (base::exists(".requestStateFileNameNative")) {

    location <- .fromRCPP(".requestStateFileNameNative")

    base::tryCatch(
      base::load(location$relativePath),
      error=function(e) e
      #,warning=function(w) w #Commented out because if there *is* a warning, which there of course shouldnt be, the state wont be loaded *at all*.
    )
  }

  state
}

#' @export
.extractErrorMessage <- function(error) {
  stopifnot(length(error) == 1)

  if (isTryError(error)) {
    msg <- attr(error, "condition")$message
    return(trimws(msg))
  } else if (is.character(error)){
    split <- base::strsplit(error, ":")[[1]]
    last <- split[[length(split)]]
    return(trimws(last))
  } else {
    stop("Do not know what to do with an object of class `", class(error)[1], "`; The class of the `error` object should be `try-error` or `character`!", domain = NA)
  }
}

#' @export
.recodeBFtype <- function(bfOld, newBFtype = c("BF10", "BF01", "LogBF10"), oldBFtype = c("BF10", "BF01", "LogBF10")) {

  # Arguments:
  # bfOld: the current value of the Bayes factor
  # newBFtype: the new type of Bayes factor, e.g., BF10, BF01,
  # oldBFtype: the current type of the Bayes factor, e.g., BF10, BF01,

  newBFtype <- match.arg(newBFtype)
  oldBFtype <- match.arg(oldBFtype)

  if (oldBFtype == newBFtype)
    return(bfOld)

  if      (oldBFtype == "BF10") { if (newBFtype == "BF01") { return(1 / bfOld);  } else { return(log(bfOld));     } }
  else if (oldBFtype == "BF01") {	if (newBFtype == "BF10") { return(1 / bfOld);  } else { return(log(1 / bfOld)); } }
  else                          {	if (newBFtype == "BF10") { return(exp(bfOld)); } else { return(1 / exp(bfOld));	} } # log(BF10)
}

#' @export
.parseAndStoreFormulaOptions <- function(jaspResults, options, names) {
  for (i in seq_along(names)) {
    name <- names[[i]]
    options[[paste0(name, "Unparsed")]] = options[[name]]

    if (is.null(jaspResults[[name]])) {
      parsedOption <- .parseRCodeInOptions(options[[name]])
      jaspResults[[name]] <- createJaspState(parsedOption, name)
    }

    options[[name]] <- jaspResults[[name]]$object
  }

  return(options)
}

#' @export
.parseRCodeInOptions <- function(option) {
  if (.RCodeInOptionsIsOk(option)) {
    if (length(option) > 1L)
      return(eval(parse(text = option[[1L]])))
    else
      return(eval(parse(text = option)))
  }
  else
    return(NA)
}

#' @export
.RCodeInOptionsIsOk <- function(option) UseMethod(".RCodeInOptionsIsOk", option)

#' @export
.RCodeInOptionsIsOk.default <- function(option)
  return (length(option) == 1L) || (length(option) > 1L && identical(option[[2L]], "T"))

#' @export
.RCodeInOptionsIsOk.list <- function(option) {
  for (i in seq_along(option))
    if (!.RCodeInOptionsIsOk(option[[i]]))
      return(FALSE)
  return(TRUE)
}

#' @export
.setSeedJASP <- function(options) {

  if (is.list(options) && all(c("setSeed", "seed") %in% names(options))) {
    if (isTRUE(options[["setSeed"]]))
      set.seed(options[["seed"]])
  } else {
    # some analysis (t-test) have common functions for computations, however, only some of the offer seed in the interface - therefore, this error message is disabled for the moment
    # stop(paste(".setSeedJASP was called with an incorrect argument.",
    #            "The argument options should be the options list from QML.",
    #            "Ensure that the SetSeed{} QML component is present in the QML file for this analysis."))
  }
}

#' @export
.getSeedJASP <- function(options) {

  if (is.list(options) && all(c("setSeed", "seed") %in% names(options))) {
    if (isTRUE(options[["setSeed"]]))
      return(options[["seed"]])
  } else {
    stop(paste(".getSeedJASP was called with an incorrect argument.",
               "The argument options should be the options list from QML.",
               "Ensure that the SetSeed{} QML component is present in the QML file for this analysis."))
  }
}

# PLOT RELATED FUNCTION ----
#' @export
.suppressGrDevice <- function(plotFunc) {
  plotFunc <- substitute(plotFunc)
  tmpFile <- tempfile()
  grDevices::png(tmpFile)
  on.exit({
    grDevices::dev.off()
    if (file.exists(tmpFile))
      file.remove(tmpFile)
  })
  eval(plotFunc, parent.frame())
}

# not .saveImage() because RInside (interface to CPP) cannot handle that
saveImage <- function(plotName, format, height, width)
{
  state           <- .retrieveState()     # Retrieve plot object from state
  plt             <- state[["figures"]][[plotName]][["obj"]]

  plt             <- decodeplot(plt);

  location        <- .fromRCPP(".requestTempFileNameNative", "png") # create file location string to extract the root location
  backgroundColor <- .fromRCPP(".imageBackground")

  # create file location string
  location <- .fromRCPP(".requestTempFileNameNative", "png") # to extract the root location
  relativePath <- paste0("temp.", format)

  if (format == "pptx") {

    error <- try(.saveImageAsPPTX(plt, relativePath))

  } else {

    error <- try({

      # Get file size in inches by creating a mock file and closing it
      pngMultip <- .fromRCPP(".ppi") / 96
      grDevices::png(
        filename = "dpi.png",
        width = width * pngMultip,
        height = height * pngMultip,
        res = 72 * pngMultip
      )
      insize <- grDevices::dev.size("in")
      grDevices::dev.off()

      # Where available use the cairo devices, because:
      # - On Windows the standard devices use a wrong R_HOME causing encoding/font errors (INTERNAL-jasp/issues/682)
      # - On MacOS the standard pdf device can't deal with custom fonts (jasp-test-release/issues/1370) -- historically cairo could not display the default font well (INTERNAL-jasp/issues/186), but that seems fixed
      if (capabilities("cairo"))
        type <- "cairo"
      else if (capabilities("aqua"))
        type <- "quartz"
      else
        type <- "Xlib"

      # Open correct graphics device
      if (format == "eps") {

        if (type == "cairo")
          device <- grDevices::cairo_ps
        else
          device <- grDevices::postscript

        device(
          relativePath,
          width = insize[1],
          height = insize[2],
          bg = backgroundColor
        )

      } else if (format == "tiff") {

        hiResMultip <- 300 / 72
        ragg::agg_tiff(
          filename    = relativePath,
          width       = width * hiResMultip,
          height      = height * hiResMultip,
          res         = 300,
          background  = backgroundColor,
          compression = "lzw"
        )

      } else if (format == "pdf") {

        if (type == "cairo")
          device <- grDevices::cairo_pdf
        else
          device <- grDevices::pdf

        device(
          relativePath,
          width = insize[1],
          height = insize[2],
          bg = "transparent"
        )

      } else if (format == "png") {

        # Open graphics device and plot
        ragg::agg_png(
          filename   = relativePath,
          width      = width * pngMultip,
          height     = height * pngMultip,
          background = backgroundColor,
          res        = 72 * pngMultip
        )

      } else if (format == "svg") {

        # convert width & height from pixels to inches. ppi = pixels per inch. 72 is a magic number inherited from the past.
        # originally, this number was 96 but svglite scales this by (72/96 = 0.75). 0.75 * 96 = 72.
        # for reference see https://cran.r-project.org/web/packages/svglite/vignettes/scaling.html
        width  <- width  / 72
        height <- height / 72
        svglite::svglite(file = relativePath, width = width, height = height)

      } else { # add optional other formats here in "else if"-statements

        stop("Unknown image format '", format, "'", domain = NA)

      }

      # Plot and close graphics device
      if (inherits(plt, "recordedplot")) {
        .redrawPlot(plt)
      } else if (inherits(plt, c("gtable", "ggMatrixplot", "jaspGraphs"))) {
        gridExtra::grid.arrange(plt)
      } else if (inherits(plt, "gTree")) {
        grid::grid.draw(plt)
      } else {
        plot(plt)
      }
      grDevices::dev.off()

    })

  }
  # Create output for interpretation by JASP front-end and return it
  output <- list(status = "imageSaved",
                 results = list(name  = relativePath,
                                error = FALSE))
  if (isTryError(error)) {
    output[["results"]][["error"]] <- TRUE
    output[["results"]][["errorMessage"]] <-
      .extractErrorMessage(error)
  }

  return(toJSON(output))
}

.saveImageAsPPTX <- function(plt, relativePath) {
  # adapted from https://github.com/dreamRs/esquisse/blob/626cbe584f43a6a13a6d5cce3192fcf912e08cb0/R/ggplot_to_ppt.R#L64
  ppt <- officer::read_pptx()
  ppt <- officer::add_slide(ppt, layout = "Title and Content", master = "Office Theme")

  value <- if (inherits(plt, "jaspGraphsPlot") && ("newpage" %in% methods::formalArgs(plt$plotFunction))) {
    # fixes https://github.com/jasp-stats/jasp-issues/issues/1910, officer cannot handle `newpage = true` (which is necessary for other plot types)
    rvg::dml(code = plot(plt, newpage = FALSE))
  } else {
    rvg::dml(code = plot(plt))
  }

  ppt <- officer::ph_with(ppt, value, location = officer::ph_location_type(type = "body"))
  print(ppt, target = relativePath) # officer:::print.rpptx
}

# Source: https://github.com/Rapporter/pander/blob/master/R/evals.R#L1389
# THANK YOU FOR THIS FUNCTION!
.redrawPlot <- function(rec_plot)
{
  if (getRversion() < '3.0.0')
  {
    #@jeroenooms
    for (i in 1:length(rec_plot[[1]]))
      if ('NativeSymbolInfo' %in% class(rec_plot[[1]][[i]][[2]][[1]]))
        rec_plot[[1]][[i]][[2]][[1]] <- getNativeSymbolInfo(rec_plot[[1]][[i]][[2]][[1]]$name)
  } else
    #@jjallaire
    for (i in 1:length(rec_plot[[1]]))
    {
      symbol <- rec_plot[[1]][[i]][[2]][[1]]
      if ('NativeSymbolInfo' %in% class(symbol))
      {
        if (!is.null(symbol$package)) name <- symbol$package[['name']]
        else                          name <- symbol$dll[['name']]

        pkg_dll       <- getLoadedDLLs()[[name]]
        native_symbol <- getNativeSymbolInfo(name = symbol$name, PACKAGE = pkg_dll, withRegistrationInfo = TRUE)
        rec_plot[[1]][[i]][[2]][[1]] <- native_symbol
      }
    }

  if (is.null(attr(rec_plot, 'pid')) || attr(rec_plot, 'pid') != Sys.getpid()) {
    warning('Loading plot snapshot from a different session with possible side effects or errors.')
    attr(rec_plot, 'pid') <- Sys.getpid()
  }

  suppressWarnings(grDevices::replayPlot(rec_plot))
}

rewriteImages <- function(name, ppi, imageBackground) {

  jaspResultsCPP <- loadJaspResults(name)
  on.exit({
    jaspResultsCPP$status <- "imagesRewritten" # analysisResultStatus::imagesRewritten!
    jaspResultsCPP$send()
    finishJaspResults(jaspResultsCPP, calledFromAnalysis = FALSE)
  })

  oldPlots <- jaspResultsCPP$getPlotObjectsForState()


  for (i in seq_along(oldPlots)) {
    try({

      uniqueName <- oldPlots[[i]][["getUnique"]]

      jaspPlotCPP         <- jaspResultsCPP$findObjectWithUniqueNestedName(uniqueName)
      if (is.null(jaspPlotCPP))
        stop("no jasp plot found")

      jaspPlotCPP$editing <- TRUE

      plot <- jaspPlotCPP$plotObject

      # here we can modify general things for all plots (theme, font, etc.).
      # ppi and imageBackground are automatically updated in writeImageJaspResults through .Rcpp magic

      thm <- ggplot2::theme(text = ggplot2::element_text(family = jaspGraphs::getGraphOption("family")))
      if (ggplot2::is.ggplot(plot)) {
        plot <- plot + thm
      } else if (jaspGraphs:::is.jaspGraphsPlot(plot)) {
        for (i in seq_along(plot)) {
          plot[[i]] <- plot[[i]] + thm
        }
      }

      jaspPlotCPP$plotObject <- plot

      jaspPlotCPP$editing <- FALSE

    })
  }

  return(NULL)
}

# not .editImage() because RInside (interface to CPP) cannot handle that
editImage <- function(name, optionsJson) {

  optionsList <- fromJSON(optionsJson)
  plotName    <- optionsList[["data"]]
  type        <- optionsList[["type"]]
  width       <- optionsList[["width"]]
  height      <- optionsList[["height"]]
  uniqueName  <- optionsList[["name"]]

  plot     <- NULL
  revision <- -1

  # uncomment to profile (and make sure that profvis is installed)
  # profvis::profvis(prof_output = "~/jaspDeletable/robjects/profileEditImage", expr = {

  results <- try({

    jaspResultsCPP <- loadJaspResults(name)

    jaspPlotCPP         <- jaspResultsCPP$findObjectWithUniqueNestedName(uniqueName)
    if (is.null(jaspPlotCPP))
      stop("no jasp plot found")

    jaspPlotCPP$editing <- TRUE
    on.exit({jaspPlotCPP$editing <- FALSE}) # this should not persist!

    plot <- jaspPlotCPP$plotObject
    if (is.null(plot))
      stop("no plot object found")

    #We should get the extra special editing options out here and do something funky (https://www.youtube.com/watch?v=roQuEqxjDx4) with them ^^

    if (type == "resize") {

      oldWidth  <- jaspPlotCPP$width
      oldHeight <- jaspPlotCPP$height

      jaspPlotCPP$width      <- width
      jaspPlotCPP$height     <- height
      jaspPlotCPP$plotObject <- plot

      # this may fail for base graphics (e.g., "figure margins too small")
      if (jaspPlotCPP$getError()) {
        jaspPlotCPP$width      <- oldWidth
        jaspPlotCPP$height     <- oldHeight
        jaspPlotCPP$plotObject <- plot

        # ensures the JSON response matches the plot
        width  <- oldWidth
        height <- oldHeight
      } else {
        jaspPlotCPP$resizedByUser <- TRUE
      }

    } else if (type == "interactive") {


      # copy plot and check if we edit it
      if (ggplot2::is_ggplot(plot)) {
        newPlot <- ggplot2:::plot_clone(plot)
      } else {
        # jaspGraphsPlot(plot) of length 1 is only supported, but this should work
        # for any length
        newPlot <- plot$clone()
        for (i in seq_along(newPlot))
          newPlot[[i]] <- ggplot2:::plot_clone(newPlot[[i]])
      }

      newOpts       <- optionsList[["editOptions"]]
      oldOpts       <- jaspGraphs::plotEditingOptions(plot)
      newOpts$xAxis <- list(type = oldOpts$xAxis$type, settings = newOpts$xAxis$settings[names(newOpts$xAxis$settings) != "type"])
      newOpts$yAxis <- list(type = oldOpts$yAxis$type, settings = newOpts$yAxis$settings[names(newOpts$yAxis$settings) != "type"])
      newPlot       <- jaspGraphs::plotEditing(newPlot, newOpts)

      # plot editing did nothing or was canceled
      isDifferent <- if (ggplot2::is_ggplot(plot)) {
        !identical(plot, newPlot)
      } else {
        !all(vapply(seq_along(plot), \(i) identical(plot[[i]], newPlot[[i]]), FUN.VALUE = logical(1L)))
      }
      if (isDifferent) {
        jaspPlotCPP$plotObject <- newPlot
      }

    }
    interactiveJsonData <- jaspPlotCPP$interactiveJsonData
    revision <- jaspPlotCPP$revision

    finishJaspResults(jaspResultsCPP, calledFromAnalysis = FALSE)

  })

  # end of profiling
  # })

  response <- list(
    status  = "imageEdited",
    results = list(
      name     = plotName,
      resized  = type == "resize",
      width    = width,
      height   = height,
      revision = revision,
      error    = FALSE,
      editOptions         = jaspGraphs::plotEditingOptions(plot),
      interactiveJsonData = interactiveJsonData
    )
  )

  if (isTryError(results)) {

    errorMessage <- if (is.null(plot)) gettext("no plot object was found") else .extractErrorMessage(results)

    response[["results"]][["error"]]        <- TRUE
    response[["results"]][["errorMessage"]] <- errorMessage

  }

  return(toJSON(response))
}

#' @export
storeDataSet <- function(dataset) {
  jaspSyntax::loadDataSet(dataset)
}

#' @title storeDataSets
#'
#' @description Store several datasets in the JASP syntax bridge at once, for a
#'   multiDataSetAware analysis run from R (the wrapper form of
#'   `for (ds in datasets) jaspSyntax::loadDataSet(ds)`).
#'
#' @param datasets named list of dataframes, keyed by dataset id.
#'
#' @export
storeDataSets <- function(datasets) {
  # Load every dataset into the syntax bridge's workspace (jaspSyntax::loadDataSets): each list
  # name becomes a DataSet title and every dataset gets a real id and column encoder, so the
  # VariablesForms can select them by name and the bridge can encode per dataset.
  # (jaspTools runs hand their datasets to runJaspResults directly and never come here.)
  jaspSyntax::loadDataSets(datasets)
  invisible(NULL)
}

#' @export
runWrappedAnalysis <- function(moduleName, analysisName, qmlFileName, options, version, preloadData, datasets = NULL) {
  if (jaspResultsCalledFromJasp()) {
    # In this case, it is JASP Desktop that called the wrapper. This was done to parse the R code, and to get the arguments
    # in a structured way. In this way the Desktop can then set the options to the QML controls of the form, and this will run the analysis.
    # So here, just give back the parsed options.
    return(toJSON(list("options" = options, "module" = moduleName, "analysis" = analysisName, "version" = version)))

  } else {
    # The wrapper is called inside an R environment (R Studio probably).
    # The options must be parsed and checked by the QML form, and then the real analysis can be called.
    qmlFile <- file.path(find.package(moduleName), "qml", qmlFileName)

    multiDataSetJson <- NULL

    if (!is.null(datasets)) {
      # MultiDataSetAware wrapper: load every dataset into the bridge workspace (names become
      # DataSet titles), let the QML forms select them through their dataSetSelectionOption
      # (depends-ordered, so the column options bind against the selected dataset and the
      # controls stamp the .meta provenance), and have the bridge encode per dataset and queue
      # the slices - the exact preparation a desktop run gets from Engine::runAnalysis (shared
      # through DataBridge::prepareMultiDataSetRun). runJaspResults then reads the slices from
      # the queue: encoded columns matching the encoded options, keyed by slice (filter) id.
      storeDataSets(datasets)

      status           <- jaspSyntax::loadQmlAndParseOptionsStatus(moduleName, analysisName, qmlFile,
                                                                   as.character(toJSON(options)), version, preloadData)
      options          <- status$options
      multiDataSetJson <- if (nzchar(status$multiDataSetJson)) status$multiDataSetJson else NULL

      if (!length(options) || !nzchar(options))
        stop("Error when parsing the options")
    } else {
      options <- jaspSyntax::loadQmlAndParseOptions(moduleName, analysisName, qmlFile, as.character(toJSON(options)), version, preloadData)

      if (options == "")
        stop("Error when parsing the options")
    }

     internalAnalysisName <- paste0(moduleName, "::", analysisName, "Internal")

     return(runJaspResults(name=internalAnalysisName, title=analysisName, dataKey="{}", options=options, stateKey="{}", functionCall=internalAnalysisName, preloadData=preloadData, multiDataSetJson = multiDataSetJson))
  }
}



