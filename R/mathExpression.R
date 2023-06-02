#' LaTeX Expressions in JASP
#'
#' @description Construct LaTeX expressions for JASP output
#'
#' @param x A string that contains the LaTeX code for the math expression
#' @param inline Should the expression be inline or in display mode?
#'
#' @examples
#' # Escape the backslash character for LaTeX code
#' jaspBase::mathExpression("\\alpha + \\beta")
#' # Alternatively use the raw character constant
#' jaspBase::mathExpression(r"{\alpha + \beta}")
#'
#' # By default expressions are inline, to disable use
#' jaspBase::mathExpression(r"{ \bar{x} = \frac{\sum_i^n x_i}{n} }", inline = FALSE)
#'
#' @export
mathExpression <- function(x, inline = TRUE) {
  stopifnot(is.character(x))

  if (!inline) {
    output <- sprintf("\\[%s\\]", x)
  } else {
    output <- sprintf("\\(%s\\)", x)
  }

  return(output)
}
