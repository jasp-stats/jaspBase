assignFunctionInPackage <- function(fun, name, package) {
  #'@title Overwrite functions inside R packages
  #'
  #'@param fun    : function you want to replace
  #'@param name   : the name of the function in a package
  #'@param package: the name of the package
  #'
  #' example usage:
  #' avoid a parallel backend from being registered which triggers a firewall message
  ns <- getNamespace(package)
  unlockBinding(name, ns)
  assign(name, fun, ns)
  lockBinding(name, ns)
}

# NOTE: let's make it a custom to start the new function names with "fake" followed by the name of the original
# function. Also, let's put all "modified" versions of functions in this file. Note that thise file probably needs
# to be resolved completely for R syntax.

# cowplot, used in flexplot (and other analyses that open pdf devices on Windows) ----
fakeGrDevicesPdf <- function(file = if(onefile) "Rplots.pdf" else "Rplot%03d.pdf",
                             width, height, onefile, family, title, fonts, version,
                             paper, encoding, bg, fg, pointsize, pagecentre, colormodel,
                             useDingbats, useKerning, fillOddEven, compress) {
  args <- as.list(match.call())
  matchedIdx <- which(names(args) %in% c("file", "width", "height", "oneFile", "family", "bg", "pointsize"))
  matchedArgs <- args[matchedIdx]

  matchedArgs[["filename"]] <- matchedArgs[["file"]]
  matchedArgs[["file"]] <- NULL

  do.call(grDevices::cairo_pdf, matchedArgs, envir = parent.frame())
}
