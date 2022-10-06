#' @export
getOS <- function() {
  os <- NULL
  if (!is.null(Sys.info())) {
    os <- Sys.info()["sysname"]
    if (os == "Darwin")
      os <- "osx"
  } else {
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  return(tolower(os))
}

restoreOptions <- function(oldOptions) {

  # NOTE: `options(oldOptions)`, like withr does, is insufficient because this does not remove any options that were set before

  # for example,
  # oldOptions <- options()
  # options("a")
  # $a
  # NULL
  # options("a" = 1)
  # options("a")
  # $a
  # [1] 1
  # options(oldOptions)
  # options("a") # still there!
  # $a
  # [1] 1

  newOptions <- options()
  oldOptions[setdiff(names(newOptions), names(oldOptions))] <- list(NULL)
  options(oldOptions)

}

setOptionsCleanupHook <- function() {

  oldGraphOptions <- jaspGraphs::graphOptions()
  oldOptions <- options()

  withr::defer({
    restoreOptions(oldOptions)
    jaspGraphs::graphOptions(oldGraphOptions)
  }, envir = parent.frame(2))

}
