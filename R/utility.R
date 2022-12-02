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
  # resetting 'nwarnings' resets the collection of warnings, so it may remove warnings that occur in any parent function
  # so we do not reset 'nwarnings'
  options(oldOptions[setdiff(names(oldOptions), "nwarnings")])

}

setOptionsCleanupHook <- function() {

  oldGraphOptions <- jaspGraphs::graphOptions()
  oldOptions <- options()

  withr::defer_parent({
    restoreOptions(oldOptions)
    jaspGraphs::graphOptions(oldGraphOptions)
  })
}

setLegacyRng <- function() {
  # R 3.6.0 changed its rng; this ensures that for the time being the results do not change
  # Unless we request to use the current method explicitly with options(jaspLegacyRngKind = FALSE)
  if(isFALSE(.Options[["jaspLegacyRngKind"]])) return()


  rngKind <- RNGkind()
  suppressWarnings(RNGkind(sample.kind = "Rounding"))
  legacyRngWarning() # give more informative warning which also does not make clutter if called repeatedly

  # reset the method to the original one once the parent function finishes
  withr::defer_parent(RNGkind(sample.kind = rngKind[[3]]))
}

legacyRngWarning <- function() {
  default    <- cli::code_highlight("RNGkind(sample.kind = 'Rounding')")
  rngKind    <- cli::code_highlight("RNGkind()")
  suggestion <- cli::code_highlight("options(jaspLegacyRngKind = FALSE)")

  cli::cli_warn(
    message = c(
      "!" = "Legacy `{default}` is used by default.",
      "i" = "To use the current `{rngKind}` setting instead, use `{suggestion}`."
    ),
    class = "jaspBaseWarning",
    .frequency    = "regularly", # show this warning once every 8 hours
    .frequency_id = "legacyRngWarning"
  )
  return()
}
