#Originally in common.R in package JASP, extracted here and changed for standalone use in jaspResults/jaspTools

tryToWriteImageJaspResults <- function(...) {
  tryCatch(
    suppressWarnings(return(writeImageJaspResults(...))),
    error	= function(e) { return(list(error = e$message)) }
  )
}

getImageLocation <- function() {
  root <- file.path(tempdir(), "jaspResults", "plots")
  if (! dir.exists(root))
    dir.create(root, recursive=TRUE)
  numPlots <- length(list.files(root))
  list(
    root = root,
    relativePath = paste(numPlots + 1, "png", sep=".")
  )
}

openGrDevice <- function(...) {
  #if (jaspResultsCalledFromJasp())
  #  svglite::svglite(...)
  #else
  # grDevices::png(..., type = ifelse(Sys.info()["sysname"] == "Darwin", "quartz", "cairo"))
  ragg::agg_png(...)
}

writeImageJaspResults <- function(plot, width = 320, height = 320, obj = TRUE, relativePathpng = NULL, ppi = 300, backgroundColor = "white",
                                  location = getImageLocation(), oldPlotInfo = list()) {
  # Set values from JASP'S Rcpp when available
  if (exists(".fromRCPP")) {
    location        <- .fromRCPP(".requestTempFileNameNative", "png")
    backgroundColor <- .fromRCPP(".imageBackground")
    ppi             <- .fromRCPP(".ppi")
  }

  # TRUE if called from analysis, FALSE if called from editImage
  if (is.null(relativePathpng))
    relativePathpng <- location$relativePath

  image                           <- list()
  fullPathpng                     <- paste(location$root, relativePathpng, sep="/")
  plotEditingOptions              <- NULL
  root                            <- location$root
  oldwd                           <- getwd()
  setwd(root)
  on.exit(setwd(oldwd))

  if (length(oldPlotInfo) != 0L && !is.null(oldPlotInfo[["editOptions"]]) && ggplot2::is.ggplot(plot)) {

    # uncommenting this applies the edits previously done with plot editing to an older figure to the new figure.
    # see https://github.com/jasp-stats/INTERNAL-jasp/issues/1257 for discussion on what needs to be done before we can do this.

    # e <- try({
    #   # same construction as in editImage
    #   newPlot <- ggplot2:::plot_clone(plot)
    #
    #   newOpts       <- jaspBase::fromJSON(oldPlotInfo[["editOptions"]])
    #   oldOpts       <- jaspGraphs::plotEditingOptions(plot)
    #   newOpts$xAxis <- list(type = oldOpts$xAxis$type, settings = newOpts$xAxis$settings[names(newOpts$xAxis$settings) != "type"])
    #   newOpts$yAxis <- list(type = oldOpts$yAxis$type, settings = newOpts$yAxis$settings[names(newOpts$yAxis$settings) != "type"])
    #
    #   newPlot <- jaspGraphs::plotEditing(newPlot, newOpts)
    # })
    #
    # if (!inherits(e, "try-error"))
    #   plot <- newPlot

  }

  # IN CASE WE SWITCH TO SVG:
  # # convert width & height from pixels to inches. ppi = pixels per inch. 72 is a magic number inherited from the past.
  # # originally, this number was 96 but svglite scales this by (72/96 = 0.75). 0.75 * 96 = 72.
  # # for reference see https://cran.r-project.org/web/packages/svglite/vignettes/scaling.html
  # width  <- width / 72
  # height <- height / 72

  width  <- width  * (ppi / 96)
  height <- height * (ppi / 96)

  plot2draw <- decodeplot(plot)

  openGrDevice(file = relativePathpng, width = width, height = height, res = 72 * (ppi / 96), background = backgroundColor)#, dpi = ppi)
  on.exit(grDevices::dev.off(), add = TRUE)

  if (ggplot2::is.ggplot(plot2draw) || inherits(plot2draw, c("gtable", "gTree"))) {

    # inherited from ggplot2::ggsave
    grid::grid.draw(plot2draw)

  } else {

    isRecordedPlot <- inherits(plot2draw, "recordedplot")

    if (is.function(plot2draw) && !isRecordedPlot) {

      if (obj) grDevices::dev.control('enable') # enable plot recording
      eval(plot())
      if (obj) plot2draw <- grDevices::recordPlot() # save plot to R object

    } else if (isRecordedPlot) { # function was called from editImage to resize the plot

      .redrawPlot(plot2draw) #(see below)
    } else if (inherits(plot2draw, "qgraph")) {

      plot(plot2draw)
      # qgraph:::plot.qgraph(plot2draw)

    } else {
      plot(plot2draw)
    }

  }

  # Save path & plot object to output
  image[["png"]] <- relativePathpng

  if (obj) {
    image[["obj"]]         <- plot2draw
  }

  image[["editOptions"]] <- jaspGraphs::plotEditingOptions(plot, asJSON = TRUE)

print("Trying to do the interactive thing now")
  image[["interactive"]] <- ggplot2::is.ggplot(plot)
  if (image[["interactive"]] ) {

    jsonOrTryError <- jaspGraphs::convertGgplotToPlotly(plot)

    if (exists(".fromRCPP")) {
        
        if(inherits(jsonOrTryError, "try-error")) {
          image[["interactiveConvertError"]] = gettextf("The following error occured while converting a ggplot to plotly: %s", jsonOrTryError$message)
        }
        else {
          locationPlotly  <- .fromRCPP(".requestTempFileNameNative", "json")
          fullPathPlotly  <- paste(locationPlotly$root, locationPlotly$relativePath, sep="/")
          plotlyJsonFile  <- file(fullPathPlotly)
          writeLines(jsonOrTryError, plotlyJsonFile)
          close(plotlyJsonFile)

          image[["interactiveJsonData"]] <- locationPlotly$relativePath
        }
    }

    
  }

  return(image)
}

# intentionally not exported
decodeplot <- function(x, ...) {
  UseMethod("decodeplot", x)
}

# S3 methods must be registered (done by @export) so that jaspGraphs can call jaspBase:::decodeplot
#' @export
decodeplot.jaspGraphsPlot <- function(x, ...) {
  for (i in seq_along(x$subplots))
    x$subplots[[i]] <- decodeplot(x$subplots[[i]], returnGrob = FALSE)

  return(x)
}


#' @export
decodeplot.gg <- function(x, returnGrob = TRUE, ...) {
  # TODO: do not return a grid object!
  # we can do this by automatically replacing the scales and geoms, although this is quite a lot of work.
  # alternatively, those edge cases will need to be handled by the developer.
  labels <- x$labels # x[["labels"]] needs to be subsetted by `$`, not `[[`, as patchwork objects would fail if subsetting with `[[`
  for (i in seq_along(labels))
    if (!is.null(labels[[i]]))
      labels[[i]] <- decodeColNames(labels[[i]])

  x$labels <- labels
  if (returnGrob) {
    grDevices::png(f <- tempfile())
    on.exit({
      grDevices::dev.off()
      if (file.exists(f))
        file.remove(f)
    })
    return(decodeplot.gTree(ggplot2::ggplotGrob(x)))
  } else {
    return(x)
  }
}

#' @export
decodeplot.patchwork <- function(x, ...) {
  # the last plot in a patchwork is the "active" plot
  # and is essentially a ggplot (with some extras),
  # so we can decode it as such
  x <- decodeplot.gg(x, returnGrob = FALSE)
  # but it also contains annotations, which need to be decoded in addition to the standard gg stuff
  x$patches$annotation$title    <- decodeColNames(x$patches$annotation$title   )
  x$patches$annotation$subtitle <- decodeColNames(x$patches$annotation$subtitle)
  x$patches$annotation$caption  <- decodeColNames(x$patches$annotation$caption )

  # each subplot can be either a patchwork or a ggplot object
  x$patches$plots <-  lapply(x$patches$plots, decodeplot, returnGrob = FALSE)

  return(x)
}

#' @export
decodeplot.recordedplot <- function(x, ...) {
  decodeplot.gTree(grid::grid.grabExpr(gridGraphics::grid.echo(x)))
}

#' @export
decodeplot.gtable <- function(x, ...) rapply(x, f = decodeColNames, classes = "character", how = "replace")
#' @export
decodeplot.grob   <- function(x, ...) rapply(x, f = decodeColNames, classes = "character", how = "replace")
#' @export
decodeplot.gTree  <- function(x, ...) rapply(x, f = decodeColNames, classes = "character", how = "replace")
#' @export
decodeplot.gDesc  <- function(x, ...) rapply(x, f = decodeColNames, classes = "character", how = "replace")

#' @export
decodeplot.qgraph <- function(x, ...) {
  labels <- x[["graphAttributes"]][["Nodes"]][["labels"]]
  names  <- x[["graphAttributes"]][["Nodes"]][["names"]]
  labels <- decodeColNames(labels)
  names  <- decodeColNames(names)
  x[["graphAttributes"]][["Nodes"]][["labels"]] <- labels
  x[["graphAttributes"]][["Nodes"]][["names"]]  <- names
  return(x)
}

#' @export
decodeplot.function <- function(x, ...) {

  f <- tempfile()
  on.exit({
    grDevices::dev.off()
    if (file.exists(f))
      file.remove(f)
  })

  grDevices::png(f)
  grDevices::dev.control('enable') # enable plot recording

  eval(x())
  out <- grDevices::recordPlot()

  return(decodeplot.recordedplot(out))
}

# Some functions that act as a bridge between R and JASP. If JASP isn't running then all columnNames are expected to not be encoded

# Two convenience functions to encode/decode jasp column names. A custom encoder/decoder function may be supplied, otherwise a default is used.
# The strict parameter affects the default; if TRUE then every value of x must be an exact column name, otherwise other values may be mixed in and pattern matching is performed.
#' @export
encodeColNames <- function(x, strict = FALSE, fun = NULL, ...) {
  if (!is.function(fun))
    fun <- .getDefaultEnDeCoderFun("encode", strict)
  return(.applyEnDeCoder(x, fun, ...))
}
#' @export
decodeColNames <- function(x, strict = FALSE, fun = NULL, ...) {
  if (!is.function(fun))
    fun <- .getDefaultEnDeCoderFun("decode", strict)
  return(.applyEnDeCoder(x, fun, ...))
}

.getDefaultEnDeCoderFun <- function(type, strict) {

  # TODO: this function would benefit if jasp assigns the functions into an environment
  # that way they can be looked up directly instead of using findFun (jaspTools can also do that).

  defaults <- list(encode = list(strict = ".encodeColNamesStrict", lax = ".encodeColNamesLax"),
                   decode = list(strict = ".decodeColNamesStrict", lax = ".decodeColNamesLax"))

  method <- if (strict) "strict" else "lax"

  fun <- .findFun(defaults[[type]][[method]])

  if (!is.function(fun))
    return(function(inIsOut){return(inIsOut)}) # Instead of complaining we just give it a dummy function

  return(fun)
}

# This ensures that functions can also be found in jasptools (it needs to search in the package namespace)
.findFun <- function(name) {
  obj <- NULL
  if (exists(name))
    obj <- eval(parse(text = name))

  if (!exists(name) || !is.function(obj)) {

    if ("jasptools" %in% loadedNamespaces())
      return(utils::getFromNamespace(name, asNamespace("jasptools")))

    if(exists(name, .GlobalEnv))
      return(get(name, .GlobalEnv)) # works for both JASP and jaspTools
  }

  if (!is.function(obj))
    return(NULL)

  return(obj)
}

# Internal function that applies a decoding or encoding function (or actually any function) to R objects
# as long as they are character
.applyEnDeCoder <- function(x, fun, ...) {
  UseMethod(".applyEnDeCoder", x)
}

# Default acts as a fallback for model objects which have overwritten the list class
.applyEnDeCoder.default <- function(x, fun, ...) {
  if (!"list" %in% class(x) && is.list(x))
    x <- .applyEnDeCoder.list(x, fun, ...)

  return(x)
}

.applyEnDeCoder.character <- function(x, fun, ...) {
  for (i in seq_along(x))
    x[i] <- fun(x[i])
  return(x)
}

.applyEnDeCoder.factor <- function(x, fun, ...) {
  levels(x) <- .applyEnDeCoder.character(levels(x), fun)
  return(x)
}

.applyEnDeCoder.list <- function(x, fun, recursive = TRUE, ...) {
  if (recursive) {
    return(rapply(x, f = .applyEnDeCoder, how = "replace", fun = fun, ...))
  } else {
    for (i in seq_along(x))
      if (is.character(x[[i]]))
        x[[i]] <- .applyEnDeCoder.character(x[[i]], fun, ...)

      return(x)
  }
}

.applyEnDeCoder.matrix <- function(x, fun, ...) {
  return(.applyEnDeCoder.data.frame(x, fun, ...))
}

.applyEnDeCoder.data.frame <- function(x, fun, ...) {
  for (i in seq_along(dimnames(x)))
    dimnames(x)[[i]] <- .applyEnDeCoder.character(dimnames(x)[[i]], fun, ...)
  return(x)
}

.applyEnDeCoder.call <- function(x, fun, ...) {
  return(.modify_lang(x, fun, ...))
}

.modify_lang <- function(x, f, ...) {
  # adapted from pryr::modify_lang
  # changed modify_lang to .modify_lang and added an explicit is.character check before doing f

  # @param x a call object
  # @param f a function to apply to each leaf

  recurse <- function(y) {
    # if (!is.null(names(y))) names(y) <- f2(names(y))
    lapply(y, .modify_lang, f = f, ...)
  }

  if (is.atomic(x) || is.name(x)) {
    # Leaf
    # modified bit:
    if (is.character(x))
      f(x, ...)
    else
      x
    # end of modified bit
  } else if (is.call(x)) {
    as.call(recurse(x))
  } else if (is.function(x)) {
    formals(x) <- .modify_lang(formals(x), f, ...)
    body(x) <- .modify_lang(body(x), f, ...)
    x
  } else if (is.pairlist(x)) {
    # Formal argument lists (when creating functions)
    as.pairlist(recurse(x))
  } else if (is.expression(x)) {
    # shouldn't occur inside tree, but might be useful top-level
    as.expression(recurse(x))
  } else if (is.list(x)) {
    # shouldn't occur inside tree, but might be useful top-level
    recurse(x)
  } else {
    stop(".modify_lang encountered an unknown language class: ", paste(class(x), collapse = "/"), call. = FALSE, domain = NA)
  }
}
