#' Build a Formula from Column Names
#'
#' Create an ordinary R formula from character strings or vectors of column
#' names. Names are inserted as symbols, so spaces, punctuation, backticks, and
#' backslashes do not need to be escaped for formula syntax. No dataset is needed.
#'
#' @param ... Character strings or vectors of column names to include on the
#'   right-hand side. Multiple columns are joined with `+`.
#' @param dependent An optional single column name for the left-hand side.
#'   The default, `NULL`, creates a one-sided formula.
#' @param intercept A single logical value indicating whether to include an
#'   intercept. Defaults to `TRUE`.
#'
#' @details Arguments are evaluated normally, so variables holding column names
#'   and expressions such as `measurements[[i]]` can be used directly. Names are
#'   treated literally: `"a+b"` refers to one column named `a+b`.
#'
#'   With no right-hand-side columns, the formula is `~ 1` (or `~ 0` when
#'   `intercept = FALSE`), optionally with a dependent variable.
#'
#' @returns An object of class `formula`, with the calling environment as its
#'   environment. It can be passed directly to JASP analysis functions.
#'
#' @examples
#' columnFormula("x1", "x2", dependent = "y")
#' columnFormula(c("x1", "x2"))
#' columnFormula("3 - Average RSSI", dependent = "1-Supply Voltage")
#' columnFormula("a+b", intercept = FALSE)
#'
#' measurements <- c("HL", "3 - Average RSSI", "1-Supply Voltage")
#' lapply(measurements, columnFormula)
#'
#' @export
columnFormula <- function(..., dependent = NULL, intercept = TRUE) {
  columns <- list(...)
  validColumns <- vapply(columns, function(x) {
    is.character(x) && is.null(dim(x)) && !anyNA(x) && all(nzchar(x))
  }, logical(1))
  if (!all(validColumns))
    stop("`...` must contain character vectors of non-empty column names without NA values.", call. = FALSE)

  if (!is.null(dependent) &&
      (!is.character(dependent) || !is.null(dim(dependent)) ||
       length(dependent) != 1L || anyNA(dependent) || !nzchar(dependent)))
    stop("`dependent` must be NULL or a single non-empty column name without an NA value.", call. = FALSE)

  if (!is.logical(intercept) || !is.null(dim(intercept)) ||
      length(intercept) != 1L || is.na(intercept))
    stop("`intercept` must be TRUE or FALSE.", call. = FALSE)

  symbols <- lapply(unlist(columns, use.names = FALSE), as.name)
  rhs <- if (length(symbols))
    Reduce(function(left, right) call("+", left, right), symbols)
  else
    1
  if (!intercept)
    rhs <- if (length(symbols)) call("+", 0, rhs) else 0

  result <- if (is.null(dependent))
    call("~", rhs)
  else
    call("~", as.name(dependent), rhs)

  stats::as.formula(result, env = parent.frame())
}
