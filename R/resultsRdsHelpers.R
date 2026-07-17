#'
#' Internal helper to strip bulky attributes and objects from the results
#' tree before saving as RDS.
#'
#' Called from C++ (jaspResults::saveResults) via:
#'   Rcpp::Environment::namespace_env("jaspBase")[".jaspResults_stripEnv"]
#'
#' Only called when the JASP_RDS_STRIP environment variable is set
#' (by JASP's ProcessHelper when launching engines). When unset, the
#' full toRObject() tree is saved unchanged.
#'
#' What gets stripped:
#'   - jaspObjectEnvironment attributes (XPtrs to live C++ objects that
#'     drag the entire Rcpp module into the RDS, ~90MB bloat)
#'   - plotObject in jaspPlot wrappers (live ggplot objects whose
#'     ggproto/S7 components carry huge environments, ~57MB per plot)
#'
#' @param x A jaspResults tree (list of data.frames and nested lists).
#' @return The same tree with bulky attributes/objects removed.
#' @keywords internal
.jaspResults_stripEnv <- function(x) {
  .strip_env_recursive(x)
}

.strip_env_recursive <- function(x) {
  # Remove jaspObjectEnvironment at every level
  attr(x, "jaspObjectEnvironment") <- NULL

  if (is.data.frame(x)) {
    return(x)
  }

  if (is.list(x)) {
    # Check if this is a jaspPlot wrapper: has plotObject and class jaspPlotWrapper
    cls <- class(x)
    if ("jaspPlotWrapper" %in% cls && "plotObject" %in% names(x)) {
      # ggplot objects carry enormous environments (~57MB each).
      # The plot data is tiny (KB); the bloat is all environment overhead.
      # RoboReport reads PNG paths from the JSON, not the live ggplot.
      x$plotObject <- NULL
      return(x)
    }

    # Recurse into children (containers, tables, plots)
    for (nm in names(x))
      x[[nm]] <- .strip_env_recursive(x[[nm]])
  }

  x
}
