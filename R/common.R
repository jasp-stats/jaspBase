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
runJaspResults <- function(name, title, dataKey, options, stateKey, functionCall = name) {

  # let's disable this for now
  # if (identical(.Platform$OS.type, "windows"))
  #   compiler::enableJIT(0)

  if(!isFALSE(.Options[["jaspLegacyRngKind"]])) {
    rngKind <- RNGkind()
    RNGkind(sample.kind = "Rounding")  # R 3.6.0 changed its rng; this ensures that for the time being the results do not change
    on.exit(RNGkind(sample.kind = rngKind[[3]]), add = TRUE)
  }

  jaspResultsCPP        <- loadJaspResults(name)
  jaspResultsCPP$title  <- title
  jaspResults           <- jaspResultsR$new(jaspResultsCPP)

  jaspResultsCPP$setOptions(options)

  dataKey     <- rjson::fromJSON(dataKey)
  options     <- rjson::fromJSON(options)
  stateKey    <- rjson::fromJSON(stateKey)

  if (base::exists(".requestStateFileNameNative")) {
    location              <- .fromRCPP(".requestStateFileNameNative")
    oldwd                 <- getwd()
    setwd(location$root)
    on.exit(setwd(oldwd))
  }

  analysis    <- eval(parse(text=functionCall))

  dataset <- NULL
  if (! is.null(dataKey)) {
    cols    <- .getDataSetCols(dataKey, options)
    dataset <- do.call(.readDataSetToEnd, cols)
  }

  registerFonts()

  # resets jaspGraphs::graphOptions & options after this function finishes
  setOptionsCleanupHook()

  analysisResult <-
    tryCatch(
      expr=withCallingHandlers(expr=analysis(jaspResults=jaspResults, dataset=dataset, options=options), error=.addStackTrace),
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
    if (class(json) == "try-error")
      return(paste("{ \"status\" : \"error\", \"results\" : { \"error\" : 1, \"errorMessage\" : \"", "Unable to jsonify", "\" } }", sep=""))
    else
      return(json)
  }
}

registerFonts <- function() {

  if (requireNamespace("ragg") && requireNamespace("systemfonts")) {

    # To register custom font files shipped with JASP we need the path to the font file.
    # Next the font could be loaded like this:
    #
    # fontName <- "FreeSansJASP"
    # fontFile <- "~/github/jasp-desktop/Desktop/resources/fonts/FreeSans.ttf"
    # systemfonts::register_font(fontName, normalizePath(fontFile))
    # jaspGraphs::setGraphOption("family", fontName)

    if (exists(".resultsFont"))
      jaspGraphs::setGraphOption("family", .resultsFont)
    else
      warning("registerFonts was called but resultsFont does not exist!")

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

#' @export
.readDataSetToEnd <- function(columns=NULL, columns.as.numeric=NULL, columns.as.ordinal=NULL, columns.as.factor=NULL, all.columns=FALSE, exclude.na.listwise=NULL, ...) {

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
.readDataSetHeader <- function(columns=NULL, columns.as.numeric=NULL, columns.as.ordinal=NULL, columns.as.factor=NULL, all.columns=FALSE, ...) {

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

  names(new.df) <- .v(namez)

  new.df <- .excludeNaListwise(new.df, exclude.na.listwise)

  new.df
}

.excludeNaListwise <- function(dataset, exclude.na.listwise) {

  if ( ! is.null(exclude.na.listwise)) {

    rows.to.exclude <- c()

    for (col in .v(exclude.na.listwise))
      rows.to.exclude <- c(rows.to.exclude, which(is.na(dataset[[col]])))

    rows.to.exclude <- unique(rows.to.exclude)

    rows.to.keep <- 1:dim(dataset)[1]
    rows.to.keep <- rows.to.keep[ ! rows.to.keep %in% rows.to.exclude]

    new.dataset <- dataset[rows.to.keep,]

    if (class(new.dataset) != "data.frame") {   # HACK! if only one column, R turns it into a factor (because it's stupid)

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

  names(df) <- .v(f$name)

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
    names(df)[[1]] <- .v(f$name)

    i <- i - 1
  }

  ds <- subset(dataset, select=.v(rm.vars))
  ds <- t(as.matrix(ds))

  dependentDf <- data.frame(x = as.numeric(c(ds)))
  colnames(dependentDf) <- dependentName
  df <- cbind(df, dependentDf)

  for (bt.var in bt.vars) {

    cells <- rep(dataset[[.v(bt.var)]], each=row.count)
    new.col <- list()
    new.col[[.v(bt.var)]] <- cells

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
    ".callbackNative",
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
    location <- getAnywhere(x)
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
    jaspTools:::.setInternal("state", state)
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
      error=function(e) e,
      warning=function(w) w
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

  if (is.list(options) && c("setSeed", "seed") %in% names(options)) {
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
  png(tmpFile)
  on.exit({
    dev.off()
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
      png(
        filename = "dpi.png",
        width = width * pngMultip,
        height = height * pngMultip,
        res = 72 * pngMultip
      )
      insize <- dev.size("in")
      dev.off()

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

        # The call to capabilities("X11") may throw an error. This error has for effect that ragg::agg_tiff cannot be called anymore wthout crashing the enigne.
        # So the call to capabilities("X11") must be done only when the eps (or pdf) type is set.
        if (type == "cairo" && try(capabilities("X11")))
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

        if (type == "cairo" && try(capabilities("X11")))
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
      } else {
        plot(plt)
      }
      dev.off()

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
  # plot.ggplot == print.ggplot but print.qgraph doesn't plot anything whereas plot.qgraph does so we use plot
  ppt <- officer::ph_with(ppt, rvg::dml(code = plot(plt)), location = officer::ph_location_type(type = "body"))
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
  registerFonts()

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

    } else if (type == "interactive" && ggplot2::is.ggplot(plot)) {


      # copy plot and check if we edit it
      newPlot <- ggplot2:::plot_clone(plot)

      newOpts       <- optionsList[["editOptions"]]
      oldOpts       <- jaspGraphs::plotEditingOptions(plot)
      newOpts$xAxis <- list(type = oldOpts$xAxis$type, settings = newOpts$xAxis$settings[names(newOpts$xAxis$settings) != "type"])
      newOpts$yAxis <- list(type = oldOpts$yAxis$type, settings = newOpts$yAxis$settings[names(newOpts$yAxis$settings) != "type"])
      newPlot       <- jaspGraphs::plotEditing(newPlot, newOpts)

      # plot editing did nothing or was canceled
      if (!identical(plot, newPlot))
        jaspPlotCPP$plotObject <- newPlot

    }
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
      editOptions = jaspGraphs::plotEditingOptions(plot)
    )
  )

  if (isTryError(results)) {

    errorMessage <- if (is.null(plot)) gettext("no plot object was found") else .extractErrorMessage(results)

    response[["results"]][["error"]]        <- TRUE
    response[["results"]][["errorMessage"]] <- errorMessage

  }

  return(toJSON(response))
}

