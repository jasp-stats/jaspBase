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
#' @param checkColumns A single logical value. When `TRUE` (the default) and a
#'   dataset is available as `data` or `filteredData` (as in the R Commander),
#'   a warning points out every column that does not exist and suggests close
#'   matches. The check is skipped when no dataset is available; use `FALSE`
#'   to disable it entirely.
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
columnFormula <- function(..., dependent = NULL, intercept = TRUE, checkColumns = TRUE) {
  columns <- list(...)

  for (i in seq_along(columns)) {
    column <- columns[[i]]

    if (is.null(column))
      stop(sprintf(paste0("Argument %d in `...` is NULL, but column names must be text.\n",
                          "Did you mistype a column name? `data$Name` is NULL when the column does not exist."),
                   i),
           call. = FALSE)

    if (is.character(column) && is.null(dim(column))) {
      naIndexes <- which(is.na(column))
      if (length(naIndexes))
        stop(sprintf("Element%s %s of argument %d in `...` %s NA, but column names must be text.",
                     if (length(naIndexes) == 1L) "" else "s",
                     paste(naIndexes, collapse = ", "), i,
                     if (length(naIndexes) == 1L) "is" else "are"),
             call. = FALSE)

      emptyIndexes <- which(!nzchar(column))
      if (length(emptyIndexes))
        stop(sprintf('Element%s %s of argument %d in `...` %s an empty string (""), but column names must not be empty.',
                     if (length(emptyIndexes) == 1L) "" else "s",
                     paste(emptyIndexes, collapse = ", "), i,
                     if (length(emptyIndexes) == 1L) "is" else "are"),
               call. = FALSE)
    } else {
      hint <- if (is.factor(column))
        "Wrap it in as.character(), e.g. as.character(data[[\"LL.nominal\"]])."
      else if (is.numeric(column))
        "If these are column positions, use names(data)[...] to get their names."
      else if (inherits(column, "formula"))
        "This is already a formula: pass it directly instead."
      else if (is.list(column))
        "Use do.call(columnFormula, names) for one formula from a list of names, or lapply(names, columnFormula) for one formula per name."
      else
        "Column names must be text, e.g. columnFormula(\"part length\")."

      stop(sprintf("Argument %d in `...` must be a character vector of column names, but it is %s.\n%s",
                   i, .columnFormulaDescribed(column), hint),
           call. = FALSE)
    }
  }

  if (!is.null(dependent) &&
      (!is.character(dependent) || !is.null(dim(dependent)) ||
       length(dependent) != 1L || is.na(dependent) || !nzchar(dependent))) {
    hint <- if (is.factor(dependent))
      "Wrap it in as.character()."
    else if (is.numeric(dependent))
      "If this is a column position, use names(data)[...] to get its name."
    else
      "It must be one column name, e.g. dependent = \"strength\"."

    stop(sprintf("`dependent` must be a single column name, but it is %s.\n%s",
                 .columnFormulaDescribed(dependent), hint),
         call. = FALSE)
  }

  if (!is.logical(intercept) || !is.null(dim(intercept)) ||
      length(intercept) != 1L || is.na(intercept))
    stop(sprintf("`intercept` must be TRUE or FALSE, but it is %s. To leave out the intercept use `intercept = FALSE`.",
                 .columnFormulaDescribed(intercept)),
         call. = FALSE)

  if (!is.logical(checkColumns) || !is.null(dim(checkColumns)) ||
      length(checkColumns) != 1L || is.na(checkColumns))
    stop(sprintf("`checkColumns` must be TRUE or FALSE, but it is %s.",
                 .columnFormulaDescribed(checkColumns)),
         call. = FALSE)

  allNames <- unlist(columns, use.names = FALSE)
  if (checkColumns)
    .columnFormulaCheckColumns(c(dependent, allNames), parent.frame())

  symbols <- lapply(allNames, as.name)
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

# Describe an object for use in the error messages above, e.g. `the string "x"` or `a factor`.
.columnFormulaDescribed <- function(x) {
  if (is.null(x))
    return("NULL")
  if (inherits(x, "formula"))
    return(paste0("a formula (", paste(deparse(x), collapse = " "), ")"))
  if (!is.null(dim(x)))
    return(paste0("a ", class(x)[1L], " with dimensions ", paste(dim(x), collapse = "x")))
  if (is.factor(x))
    return("a factor")
  if (is.list(x))
    return("a list")
  if (is.character(x)) {
    preview <- paste0('"', head(x, 3L), '"', collapse = ", ")
    if (length(x) == 1L)
      return(paste0("the string ", preview))
    return(paste0("a character vector of length ", length(x), " (", preview, ")"))
  }
  if (is.numeric(x) || is.logical(x)) {
    preview <- paste(head(x, 3L), collapse = ", ")
    kind <- if (is.numeric(x)) "number" else "logical value"
    if (length(x) == 1L)
      return(paste0("the ", kind, " ", preview))
    return(paste0(length(x), " ", kind, "s (", preview, ")"))
  }
  paste0("a ", class(x)[1L])
}

# Warn about names that do not exist in the dataset visible to the caller. Pure feedback: it
# never errors, and it stays silent when there is no dataset (no `data`/`filteredData` in
# scope) or when the encoder helpers from the engine are unavailable.
.columnFormulaCheckColumns <- function(names, callerEnv) {
  dataset <- get0("data", envir = callerEnv, inherits = TRUE)
  if (!is.data.frame(dataset) || ncol(dataset) == 0L) {
    dataset <- get0("filteredData", envir = callerEnv, inherits = TRUE)
    if (!is.data.frame(dataset) || ncol(dataset) == 0L)
      return(invisible(NULL))
  }

  encodeStrict <- get0(".encodeColNamesStrict", envir = callerEnv, inherits = TRUE)
  if (!is.function(encodeStrict))
    return(invisible(NULL))

  suggest <- get0(".suggestColumnNames", envir = callerEnv, inherits = TRUE)

  for (name in unique(names)) {
    encoded <- tryCatch(encodeStrict(name), error = function(e) NULL)
    if (!is.null(encoded) && nzchar(encoded))
      next

    warningMessage <- sprintf('No column named "%s" in the dataset.', name)

    suggestions <- if (is.function(suggest)) suggest(name) else character(0)
    if (length(suggestions)) {
      quoted <- paste0('"', suggestions, '"')
      joined <- if (length(quoted) == 2L) paste(quoted, collapse = " or ") else paste(quoted, collapse = ", ")
      warningMessage <- paste0(warningMessage, " Did you mean ", joined, "?")
    }

    warning(warningMessage, call. = FALSE)
  }

  invisible(NULL)
}
