#' @export
`+.jaspPlotWrapper` <- function(e1, e2) {
  e1$plotObject <- e1$plotObject + e2
  return(e1)
}
