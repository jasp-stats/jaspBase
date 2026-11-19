#' @export
`+.jaspPlotWrapper` <- function(e1, e2) {
  # forward operator `+` to the ggplot method for `+` so people can e.g., change axes labels by doing `jaspPlot + ggplot2::xlab("new title")`
  e1$plotObject <- e1$plotObject + e2
  return(e1)
}

#' @export
print.jaspPlotWrapper <- function(x, ...) {
  decoded <- decodeplot(x$plotObject)
  print(decoded, ...)
}

#' @export
plot.jaspPlotWrapper <- function(x, ...) {
  decoded <- decodeplot(x$plotObject)
  plot(decoded, ...)
}
