#
# Copyright (C) 2013-2026 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

# Reader for the model terms of a FactorsForm (`modelTerms` option) ------------------------------------------
#
# One prepared object (`prepareModelTerms()`) serves both ways an analysis can fit a model:
#   - formula mode: `modelTermsFormula()` gives a formula for lm()/glm()/...;
#   - data mode:    `modelTermsDesign()` gives the numeric design columns for engines that take a matrix.
# Both are built from the same term expressions, so the design matrix is the same in both modes.
#
# Basic mode (no model has `definitions`): the formula is exactly `response ~ a + b + a:b` in the order of
#   `components`, with an intercept; nothing is learned or checked, and factors are coded the way R codes them
#   (treatment contrasts, or the contrasts of the data, with R's marginality rule).
#
# Advanced mode: the terms are defined once for the whole form (`definitions`: a variable and how it enters) and
#   assigned to models (`terms`: each term is the product of one or more definitions, given by their ids). A model
#   carries the definitions it uses. The specifications, their parameters and defaults are listed once, in
#   inst/modelTermSpecifications.json, which jasp-desktop embeds to build the form (see .modelTermsCatalogue()).
#   Every definition is one version of a variable, written as an R language object:
#   - continuous versions are numeric columns (`x`, `log(x)`, `I((x - m) / s)`, `stats::poly(x, ...)`, ...). Every
#     constant learned on the fitting data (mean, median and sd, polynomial coefficients, spline knots, bin breaks) is
#     embedded as a numeric literal, so evaluating the formula on new data never learns it again. Rank and normal
#     scores are the exception: like rank(x) in a formula, they depend on the whole sample, so they are computed on
#     the data the formula is evaluated on, and a model with one cannot be evaluated on new data;
#   - a categorical version is the coded matrix of the factor,
#     `structure(<coding>, dimnames = list(c(<levels>), NULL))[as.character(f), , drop = FALSE]`, with the levels of
#     the fitting data embedded (indicator coding, one column per level, only in a model without intercept). R's own
#     factor handling is not used: it drops contrasts for
#     unused levels, and in new data it needs the levels of the fitting data. Because the term is a numeric matrix,
#     interactions are always the product of the coded columns of their operands (R's rule that replaces the
#     coding by indicator columns when a margin is missing does not apply to these terms).
#   - an offset has its coefficient fixed at 1. It is a main effect without design columns and without coefficient,
#     not part of an interaction, and it has no restriction. There are two kinds. `offset` is the offset of one
#     continuous variable: its values are the variable itself, and the formula has `offset(x)`. `offsetConstant` has a
#     number (`value`) and no variable: it is not in the formula, because lm() and glm() cannot take a constant
#     offset there. `modelTermsOffset()` gives the sum of all offsets as a vector for engines that take a matrix, and
#     `modelTermsConstantOffset()` the sum of the constant offsets, which a caller of lm() passes as
#     `offset = rep(constant, nrow(data))`.
#   Each term is a call built with `as.name()`/`call()`/`bquote()`; user-supplied names are never pasted into code,
#   so names with spaces, unicode or JASP-encoded names work. Constants are embedded as calls to c() and list(),
#   because R compares the variables of a formula by their calls and not by embedded vectors or lists.
#   R orders the variables of an interaction by their first appearance in the formula (which follows the order of
#   `terms`), and model.matrix() varies the first of them fastest. The labels and columns of an interaction follow
#   that order, as the coefficient names of lm() do. A restriction applies to the main effect of a definition only;
#   interactions are estimated freely. The formula does not enforce restrictions: the caller fixes the coefficients
#   (`fixed` of `modelTermsDesign()`, `restriction` and `interceptFixed` of the prepared models).
#
# Labels (`decodeColNames()` is applied to variable names; `x` and `f` stand for decoded names). The label of a term is
#   the one the form shows: the `label` pattern of its specification in the catalogue, filled in with the settings as
#   typed (.modelTermsLabel(); e.g. log(x), 2<middle dot>log(x, base = 10), x^(1/3), f (reference b), offset(2)). A term with one
#   column has its label as the label of the column; the columns of the others are labelled here:
#   polynomial             Polynomial(x, 1) ... Polynomial(x, d)
#   rawPolynomial          x, x^2, ..., x^d
#   naturalSpline, bSpline Natural cubic spline(x, 1) ... (x, k), B-spline(x, 1) ... (x, k)
#   binning                x [lower, upper) for every bin but the first (the last upper bound is infinite)
#   fourier                sin(2<pi>x/P), cos(2<pi>x/P), sin(4<pi>x/P), ...
#   categorical            f (level), f (b vs. a, ...), f (linear), ... per coding:
#     treatment and sas      f (level) for every level but the reference (sas: the last level)
#     simple                 f (level) for every level but the reference (the intercept is the mean of the level means)
#     sum                    f (level) for every level but the last (deviation from the mean of the level means)
#     helmert                f (b vs. a), f (c vs. a, b), ...
#     reverseHelmert         f (a vs. b, c), f (b vs. c), ...
#     successiveDifferences  f (b <minus> a), f (c <minus> b), ...; forwardDifference: f (a <minus> b), f (b <minus> c), ...
#     orthogonalPolynomial   f (linear), f (quadratic), f (cubic), f (degree 4), ...
#     indicator              f (level) for every level
#   interaction            the labels of the operands joined by `interactionSymbol`, e.g. log(x) * f (b)
# The intercept is "(Intercept)".
#
# Limits: R's model.matrix() warns about and may truncate term names longer than 2048 characters, so a term whose
#   expression exceeds 2000 characters (a factor with many levels, a spline with many knots, many bins) is rejected
#   up front.

#' Read the Model Terms of a FactorsForm
#'
#' @description Reads the value of a FactorsForm model-terms option, with or without advanced term specification
#' (term definitions shared by the models, terms that combine definitions, restrictions), and gives the formula or the
#' numeric design columns of each model.
#'
#' `prepareModelTerms()` validates the option against the fitting data and learns every data-dependent constant once.
#' `modelTermsFormula()` and `modelTermsDesign()` build the same model in formula mode and in data mode.
#' `modelTermsLabels()` translates the coefficient names of an [stats::lm] or [stats::glm] fit of the formula into
#' display labels. `modelTermsOffset()` gives the offset of a model for engines that take a design matrix, and
#' `modelTermsConstantOffset()` the constant part of it, which a formula cannot hold.
#'
#' @param modelTerms The option value: a list of models, each a list with `name`, `title`, `components` (a list of
#'   character vectors, one per term) and, with advanced specification, `definitions` and `terms`. Arrays may be
#'   character vectors or lists.
#' @param dataset The fitting data, already restricted by the analysis to the rows it fits. The learned constants
#'   (mean, median, sd, polynomial coefficients, knots, breaks) are those of these rows, unweighted.
#' @param prepared An object returned by `prepareModelTerms()`.
#' @param model The position or the `name` of one of the prepared models.
#' @param response Names of the response column(s) for a two-sided formula; several names give `cbind(...)`.
#' @param intercept Whether the formula has an intercept, or the design an `(Intercept)` column of ones. `NULL` (the
#'   default) is the intercept of the model: `TRUE`, unless a model with advanced specification has `intercept =
#'   FALSE`. `TRUE` or `FALSE` overrides it. The columns of a design are coded as for a model with an intercept,
#'   also without the intercept column; with advanced specification the same holds for the formula, because a
#'   nominal variable is a coded matrix and not a factor (a formula of plain factors is coded by R, which gives the
#'   first factor of a model without intercept an indicator per level).
#' @param data New data for predictions. `NULL` (the default) is the fitting data. The learned constants and the
#'   levels of the fitting data are used; levels that are not in the fitting data are an error. This holds for
#'   `modelTermsDesign()` and `modelTermsOffset()`.
#' @param coefficientNames Coefficient names of a fit of `modelTermsFormula()` (in basic mode, of a fit with an
#'   intercept: without one, R codes the first factor by indicators, which these labels do not describe).
#'
#' @details The formula of `modelTermsFormula()` has the calling environment as its environment (as
#' [stats::as.formula]); its terms call only base, stats and splines functions.
#'
#' Without advanced specification (no model has `definitions`) the formula is exactly
#' `response ~ a + b + a:b` in the order of `components`, with an intercept, factors are coded as R codes them,
#' and nothing is learned or checked. The ids of the terms are `t1`, `t2`, ... in the order R gives the terms of the
#' formula.
#'
#' With advanced specification, a model has `definitions` and `terms`. A definition (`id`, `variable`,
#' `specification`, the parameters of the specification and `restriction`) is one version of a variable, such as
#' `log(x)` or the treatment coding of a factor; the same definition (the same `id`) can be used by several models.
#' A term (`id`, `operands`) is the product of the versions named by its operands, the ids of definitions of the
#' model: one operand is a main effect, several are an interaction. The model has the terms in the order of `terms`.
#' A categorical version is the coded matrix of the factor, so an interaction is always the product of the coded
#' columns of its operands, also when a margin is missing. The terms come from `terms`, `components` is not read; a
#' model without `definitions` may not have `components` when another model has `definitions`. Definitions that no
#' term uses are ignored. Every inconsistency (an operand that is not a definition of the model, two terms with the
#' same operands, a definition without variable or specification, an unknown specification, two definitions with the
#' same settings, two codings of one nominal variable, a restriction with the wrong number of values) stops with a
#' message that names the model by its title. So does a value outside the domain of a specification, in the fitting
#' data and in new data.
#'
#' A model with advanced specification may also have `intercept` (`TRUE` when absent) and, when the analysis allows
#' restrictions, `interceptRestriction` (`""` estimates the intercept freely, a numeric expression fixes it; it is
#' not read for a model without intercept).
#'
#' The specifications, their parameters, the defaults of parameters a definition does not give and the labels of the
#' terms are listed in `modelTermSpecifications.json` of this package (the file jasp-desktop builds the form from). Rank
#' and normal scores depend on the whole sample, as `rank(x)` in a formula does: they are computed on the data the
#' formula is evaluated on, so `modelTermsDesign()` and `modelTermsOffset()` refuse new data for a model with one
#' (`sampleDependent` of the model); `predict()` on a fit of the formula does not know this, so a caller that predicts
#' checks `sampleDependent`. Indicator coding (one column per level) needs a model without intercept.
#'
#' A restriction is one number or numeric expression per column of the term, separated by commas or semicolons;
#' `""` estimates the term freely. The restriction of a definition applies to the main effect of that definition (a
#' term with one operand); interaction terms are estimated freely. The intercept is one more term in `nested`: it is
#' free, fixed at a value, or (without intercept) fixed at 0. `modelTermsFormula()` does not enforce restrictions: the
#' caller fixes the coefficients, e.g. with the `fixed` attribute of `modelTermsDesign()`.
#'
#' An offset adds a term to the linear predictor with its coefficient fixed at 1. There are two kinds. The specification
#' `offset` (continuous variables only, no parameters) is the offset of one variable: its values are the variable itself.
#' The specification `offsetConstant` has no variable but `value`, a finite number. An offset is a main effect that has no
#' coefficient: an offset of a variable gives `offset(x)` in the formula (so that [stats::lm] and [stats::glm] apply it),
#' a constant offset is not in the formula (they cannot take a constant offset there; `modelTermsConstantOffset()` gives
#' it, to be passed as `offset = rep(constant, nrow(data))`), and neither has a column in the design. `modelTermsOffset()`
#' has the values of all offsets. Several offsets add up. An offset cannot be an operand of an interaction and cannot
#' have a restriction. A nominal variable cannot be an offset, and a value that is not a finite number stops the
#' analysis. In `terms` an offset is a row without columns (`offset` is `TRUE`, `label` is `offset(x)` or
#' `offset(2)`), listed after the other terms. It counts as a term in `nested`, so models are nested only when they
#' have the same offsets (the same variables and the same constant values).
#'
#' @returns `prepareModelTerms()` returns an object of class `jaspModelTerms`, a list with
#' \itemize{
#'   \item{\code{advanced}}{ Whether any model has `definitions`.}
#'   \item{\code{levels}}{ The levels of every categorical variable in the fitting data, by variable name.}
#'   \item{\code{data}}{ The variables of the models in the fitting data, categorical ones as factors with these levels.}
#'   \item{\code{nested}}{ Logical, one entry per pair of consecutive models: the terms of model k are a subset of
#'     those of model k + 1, comparing the full specifications (a restricted term is nested in the same
#'     term estimated freely or restricted in the same way), and both models have the same offsets.}
#'   \item{\code{models}}{ One list per model with `name`, `title`, `terms` (a data frame with one row per term,
#'     in the order [stats::lm] uses, followed by the offsets, and columns `id`, `type`
#'     (`main`/`interaction`), `label`, `expression`, `restriction` and `offset` (logical, `TRUE` for an offset
#'     term, which has no columns)), `intercept` (logical), `interceptFixed` (the fixed intercept, `NA` when it is
#'     estimated freely or absent), `sampleDependent` (whether a term depends on the whole sample, such as a rank) and
#'     `formula` (one-sided, with the intercept of the model and the constants in the expression text).}
#' }
#' `modelTermsFormula()` returns a formula. `modelTermsDesign()` returns a data frame of numeric design columns (named
#' by term id, with `.1`, `.2`, ... for terms with several columns) with attributes `labels` (display label per
#' column), `term` (term id per column) and `fixed` (named numeric vector, one entry per column: the fixed
#' coefficient from the restriction, `NA` for a column that is estimated freely; the `(Intercept)` column carries
#' the restriction of the intercept); an offset term has no column. `modelTermsLabels()` returns the
#' display labels in the order of `coefficientNames`; names that are not columns of the model are returned unchanged.
#' `modelTermsOffset()` returns the offset of the model, one value per row of the fitting data or of `data`, as a
#' numeric vector (the sum of its offset terms, constants repeated), or `NULL` when the model has no offset; the linear predictor of
#' the model is `modelTermsDesign() %*% coefficients + modelTermsOffset()`. `modelTermsConstantOffset()` returns the sum of
#' the constant offsets of the model (`0` without), a single number that is not in `modelTermsFormula()`.
#' @rdname prepareModelTerms
#' @export
prepareModelTerms <- function(modelTerms, dataset) {

  if (!is.data.frame(dataset))
    stop("`dataset` must be a data frame.", call. = FALSE)

  models   <- .modelTermsNormalize(modelTerms)
  advanced <- any(vapply(models, function(model) !is.null(model[["definitions"]]), logical(1L)))

  # The levels of a categorical variable are learned once for all models: they define its coding and the levels
  # that new data may contain.
  variables   <- intersect(unique(as.character(unlist(lapply(models, `[[`, "variables"), use.names = FALSE))), names(dataset))
  categorical <- variables[vapply(variables, function(variable) .modelTermsIsCategorical(dataset[[variable]]), logical(1L))]
  levels      <- stats::setNames(lapply(categorical, function(variable) .modelTermsLevels(dataset[[variable]])), categorical)
  data        <- dataset[variables]
  for (variable in categorical)
    data[[variable]] <- .modelTermsAlignFactor(data[[variable]], levels[[variable]])

  models <- lapply(models, function(model) {
    if (advanced)
      .modelTermsAdvancedModel(model, data, levels)
    else
      .modelTermsBasicModel(model)
  })

  return(structure(list(advanced = advanced, nested = .modelTermsNested(models), models = models,
                        data = data, levels = levels), class = "jaspModelTerms"))
}

#' @rdname prepareModelTerms
#' @export
modelTermsFormula <- function(prepared, model, response = NULL, intercept = NULL) {

  model     <- .modelTermsModel(prepared, model)
  intercept <- .modelTermsIntercept(intercept, model)

  return(.modelTermsFormulaObject(model[["calls"]][!model[["constantCalls"]]], response, intercept, env = parent.frame()))
}

#' @rdname prepareModelTerms
#' @export
modelTermsDesign <- function(prepared, model, data = NULL, intercept = NULL) {

  model     <- .modelTermsModel(prepared, model)
  intercept <- .modelTermsIntercept(intercept, model)
  data      <- if (is.null(data)) prepared[["data"]] else .modelTermsCheckNewData(prepared, model, data)
  matrix    <- .modelTermsMatrix(model, data)

  columns <- if (intercept) seq_along(matrix[["names"]]) else which(!is.na(matrix[["term"]]))
  design  <- as.data.frame(matrix[["values"]][, columns, drop = FALSE], stringsAsFactors = FALSE)
  names(design) <- matrix[["names"]][columns]
  attr(design, "labels") <- matrix[["labels"]][columns]
  attr(design, "term")   <- matrix[["term"]][columns]
  attr(design, "fixed")  <- stats::setNames(matrix[["fixed"]][columns], names(design))

  return(design)
}

#' @rdname prepareModelTerms
#' @export
modelTermsOffset <- function(prepared, model, data = NULL) {

  model   <- .modelTermsModel(prepared, model)
  data    <- if (is.null(data)) prepared[["data"]] else .modelTermsCheckNewData(prepared, model, data)
  offsets <- Filter(function(version) isTRUE(version[["offset"]]), model[["versions"]])
  if (length(offsets) == 0L)
    return(NULL)

  # The offset terms add up: the constants are one number for every row, the others are variables
  total <- rep(.modelTermsConstantSum(model), nrow(data))
  for (version in Filter(function(version) is.null(version[["constant"]]), offsets))
    total <- total + as.numeric(data[[version[["variable"]]]])

  return(total)
}

#' @rdname prepareModelTerms
#' @export
modelTermsConstantOffset <- function(prepared, model) {

  return(.modelTermsConstantSum(.modelTermsModel(prepared, model)))
}

# The sum of the constant offsets of a model: 0 without
.modelTermsConstantSum <- function(model) {

  constants <- Filter(function(version) !is.null(version[["constant"]]), model[["versions"]])

  return(sum(vapply(constants, `[[`, 1, "constant")))
}

#' @rdname prepareModelTerms
#' @export
modelTermsLabels <- function(prepared, model, coefficientNames) {

  model <- .modelTermsModel(prepared, model)

  # Only the names and the structure of the columns matter, so two rows of the fitting data are enough.
  data   <- prepared[["data"]][seq_len(min(2L, nrow(prepared[["data"]]))), , drop = FALSE]
  matrix <- .modelTermsMatrix(model, data)
  index  <- match(coefficientNames, colnames(matrix[["values"]]))
  labels <- coefficientNames
  labels[!is.na(index)] <- matrix[["labels"]][index[!is.na(index)]]

  return(labels)
}

# Input -----------------------------------------------------------------------------------------------------------

# The option arrives from JSON: arrays of strings are character vectors or lists. Normalize them once here. Typed
# settings are trimmed; column names and level labels (`trim = FALSE`) are taken as they are.
.modelTermsString <- function(value, default = "", trim = TRUE) {

  value <- as.character(unlist(value, use.names = FALSE))
  if (length(value) == 0L || is.na(value[1L]))
    return(default)

  return(if (trim) trimws(value[1L]) else value[1L])
}

.modelTermsNormalize <- function(modelTerms) {

  if (is.null(modelTerms))
    modelTerms <- list()
  if (!is.list(modelTerms))
    stop("`modelTerms` must be a list of models.", call. = FALSE)

  models <- lapply(seq_along(modelTerms), function(index) {
    model      <- modelTerms[[index]]
    name       <- .modelTermsString(model[["name"]], paste0("model", index - 1L))
    components <- lapply(model[["components"]], function(term) as.character(unlist(term, use.names = FALSE)))
    components <- components[lengths(components) > 0L]

    # An advanced model has definitions; its terms are given by their operands, the ids of definitions
    definitions <- NULL
    terms       <- list()
    variables   <- unique(as.character(unlist(components, use.names = FALSE)))
    if (!is.null(model[["definitions"]])) {
      definitions <- lapply(model[["definitions"]], function(definition) {
        for (key in c("id", "specification", "restriction"))
          definition[[key]] <- .modelTermsString(definition[[key]])
        definition[["variable"]] <- .modelTermsString(definition[["variable"]], trim = FALSE)
        definition
      })
      terms <- lapply(model[["terms"]], function(term)
        list(id = .modelTermsString(term[["id"]]), operands = as.character(unlist(term[["operands"]], use.names = FALSE))))
      variables <- unique(vapply(definitions, `[[`, "", "variable"))
    }

    intercept <- as.logical(unlist(model[["intercept"]], use.names = FALSE))

    list(name = name, title = .modelTermsString(model[["title"]], name), components = components, variables = variables,
         definitions = definitions, terms = terms,
         intercept = length(intercept) == 0L || is.na(intercept[1L]) || intercept[1L], interceptRestriction = .modelTermsString(model[["interceptRestriction"]]))
  })

  return(models)
}

.modelTermsModel <- function(prepared, model) {

  if (!inherits(prepared, "jaspModelTerms"))
    stop("`prepared` must be created by prepareModelTerms().", call. = FALSE)

  index <- if (is.character(model)) match(model[1L], vapply(prepared[["models"]], `[[`, "", "name")) else as.integer(model[1L])
  if (length(index) != 1L || is.na(index) || index < 1L || index > length(prepared[["models"]]))
    stop("`model` must be the name or the position of one of the prepared models.", call. = FALSE)

  return(prepared[["models"]][[index]])
}

# The intercept of a formula or a design: the one of the model unless the caller says otherwise.
.modelTermsIntercept <- function(intercept, model) {

  if (is.null(intercept))
    return(model[["intercept"]])
  if (!is.logical(intercept) || length(intercept) != 1L || is.na(intercept))
    stop("`intercept` must be TRUE, FALSE or NULL.", call. = FALSE)

  return(intercept)
}

.modelTermsIsCategorical <- function(x) {
  return(is.factor(x) || is.character(x) || is.logical(x))
}

.modelTermsLevels <- function(x) {
  return(if (is.factor(x)) levels(droplevels(x)) else levels(factor(x)))
}

.modelTermsAlignFactor <- function(x, levels) {

  if (is.factor(x) && identical(levels(x), levels))
    return(x)

  return(factor(as.character(x), levels = levels, ordered = is.ordered(x)))
}

# Formulas --------------------------------------------------------------------------------------------------------

.modelTermsFormulaObject <- function(calls, response = NULL, intercept = TRUE, env = baseenv()) {

  add <- function(left, right) call("+", left, right)
  rhs <- if (length(calls) == 0L) {
    if (intercept) 1 else 0
  } else if (intercept)
    Reduce(add, calls)
  else
    Reduce(add, calls, 0)

  lhs <- NULL
  if (!is.null(response)) {
    response <- as.character(response)
    lhs      <- if (length(response) == 1L) as.name(response) else as.call(c(list(as.name("cbind")), lapply(response, as.name)))
  }

  return(stats::as.formula(if (is.null(lhs)) call("~", rhs) else call("~", lhs, rhs), env = env))
}

# The text of a term. 17 digits keep the embedded constants exact when the text is parsed again.
.modelTermsDeparse <- function(expression) {

  text <- deparse(expression, width.cutoff = 500L,
                  control = c("keepNA", "keepInteger", "niceNames", "showAttributes", "digits17"))

  return(paste(trimws(text), collapse = " "))
}

# Basic mode ------------------------------------------------------------------------------------------------------

# Plain terms, as the analysis wrote them before: the formula is `a + b + a:b` in the order of `components`. Only
# the labels need R's own view of the terms (their order, and which factors are coded by contrasts).
.modelTermsBasicModel <- function(model) {

  calls   <- lapply(model[["components"]], function(term) Reduce(function(left, right) call(":", left, right), lapply(term, as.name)))
  formula <- .modelTermsFormulaObject(calls)
  info    <- stats::terms(formula)

  variables   <- vapply(as.list(attr(info, "variables"))[-1L], as.character, "")
  factors     <- attr(info, "factors")
  expressions <- attr(info, "term.labels")
  layout      <- lapply(seq_along(expressions), function(index) {
    rows <- which(factors[, index] > 0L)
    list(variables = variables[rows], coding = unname(factors[rows, index]))
  })

  terms <- data.frame(
    id          = sprintf("t%d", seq_along(expressions)),
    type        = if (length(expressions)) ifelse(lengths(lapply(layout, `[[`, "variables")) == 1L, "main", "interaction") else character(0L),
    label       = vapply(layout, function(term) paste(decodeColNames(term[["variables"]]), collapse = interactionSymbol), ""),
    expression  = expressions,
    restriction = rep("", length(expressions)),
    offset      = rep(FALSE, length(expressions)),
    stringsAsFactors = FALSE
  )

  return(list(name = model[["name"]], title = model[["title"]], advanced = FALSE, terms = terms, formula = formula, sampleDependent = FALSE,
              calls = calls, offsetCalls = rep(FALSE, length(calls)), constantCalls = rep(FALSE, length(calls)), layout = layout, variables = model[["variables"]],
              intercept = TRUE, interceptFixed = NA_real_))
}

# The labels of the columns of every term, in the order of the model matrix: R codes a factor by contrasts, or by
# indicator columns when the term without that factor is not in the model (a code of 2 in `terms()$factors`).
.modelTermsBasicColumns <- function(model, data) {

  return(lapply(model[["layout"]], function(term) {
    operands <- Map(function(variable, coding) {
      x     <- data[[variable]]
      label <- decodeColNames(variable)
      if (!is.factor(x))
        return(label)
      parts <- if (coding == 2L) levels(x) else .modelTermsContrastParts(x)
      return(paste0(label, " (", parts, ")"))
    }, term[["variables"]], term[["coding"]])
    return(.modelTermsProductLabels(unname(operands)))
  }))
}

# The coefficient labels of the contrasts R uses for a factor without a coding of its own.
.modelTermsContrastParts <- function(x) {

  contrast <- attr(x, "contrasts")
  if (is.null(contrast))
    contrast <- getOption("contrasts")[[1L + is.ordered(x)]]

  names  <- levels(x)
  coding <- if (is.character(contrast)) switch(contrast, contr.treatment = "treatment", contr.SAS = "sas", contr.sum = "sum",
                                               contr.helmert = "helmert", contr.poly = "orthogonalPolynomial")
  if (!is.null(coding))
    return(.modelTermsCodingParts(coding, names, names[1L]))

  # Any other contrasts: the names of their columns, or their position
  matrix <- if (is.character(contrast)) match.fun(contrast)(length(names)) else contrast
  if (is.null(colnames(matrix)))
    return(as.character(seq_len(ncol(matrix))))

  return(colnames(matrix))
}

# Columns ---------------------------------------------------------------------------------------------------------

# The labels of the columns of a product of operands: the first operand varies fastest, as in model.matrix().
.modelTermsProductLabels <- function(operands) {

  if (length(operands) == 1L)
    return(operands[[1L]])

  grid <- expand.grid(operands, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

  return(do.call(paste, c(unname(as.list(grid)), sep = interactionSymbol)))
}

# The model matrix of a model on `data` (`values`, the intercept first) with the `names`, `labels`, `term` ids and
# `fixed` coefficients of its columns. Missing values stay in the matrix: the rows of `data` are the rows of the design.
.modelTermsMatrix <- function(model, data) {

  # Always with an intercept, so that the coding of the columns does not depend on the intercept of the model. The
  # offsets have no columns and are left out.
  formula <- .modelTermsFormulaObject(model[["calls"]][!model[["offsetCalls"]]], intercept = TRUE)
  frame   <- stats::model.frame(formula, data, na.action = stats::na.pass)
  values  <- stats::model.matrix(formula, frame)
  assign  <- attr(values, "assign")
  terms   <- model[["terms"]]
  columns <- if (model[["advanced"]]) model[["columns"]] else .modelTermsBasicColumns(model, data)

  # The columns of a term are the labels of the term; if R built other columns, only its own names are known.
  # An offset has no columns and is no term of the model matrix. The offsets are the last rows of `terms`, so the other
  # rows are the terms of the matrix, in its order.
  labels <- decodeColNames(colnames(values))
  fixed  <- rep(NA_real_, ncol(values))
  term   <- rep(NA_character_, ncol(values))
  names  <- colnames(values)
  for (index in which(!terms[["offset"]])) {
    positions <- which(assign == index)
    term[positions]  <- terms[["id"]][index]
    names[positions] <- if (length(positions) == 1L) terms[["id"]][index] else paste0(terms[["id"]][index], ".", seq_along(positions))
    if (length(columns[[index]]) == length(positions))
      labels[positions] <- columns[[index]]
    if (model[["advanced"]] && length(model[["fixed"]][[index]]) == length(positions))
      fixed[positions] <- model[["fixed"]][[index]]
  }
  labels[assign == 0L] <- "(Intercept)"
  names[assign == 0L]  <- "(Intercept)"
  fixed[assign == 0L]  <- model[["interceptFixed"]]

  return(list(values = values, names = names, labels = labels, term = term, fixed = fixed))
}

# New data: the columns must exist and the values must be in the domain of the learned versions. A categorical
# variable may only have levels of the fitting data; its levels are those of the fitting data.
.modelTermsCheckNewData <- function(prepared, model, data) {

  if (!is.data.frame(data))
    stop("`data` must be a data frame.", call. = FALSE)

  title <- model[["title"]]
  if (isTRUE(model[["sampleDependent"]]))
    .quitAnalysis(gettextf("%1$s: a rank or normal score depends on the whole sample, so the model cannot be evaluated for new data.", title))

  for (variable in model[["variables"]]) {
    label <- decodeColNames(variable)
    if (!variable %in% names(data))
      .quitAnalysis(gettextf("%1$s: the data has no variable '%2$s'.", title, label))
    x <- data[[variable]]

    if (variable %in% names(prepared[["levels"]])) {
      if (!.modelTermsIsCategorical(x))
        .quitAnalysis(gettextf("%1$s: the variable '%2$s' must be nominal.", title, label))
      if (anyNA(x) && model[["advanced"]])
        .quitAnalysis(gettextf("%1$s: the nominal variable '%2$s' has missing values.", title, label))
      unknown <- setdiff(unique(as.character(x)), c(NA_character_, prepared[["levels"]][[variable]]))
      if (length(unknown) > 0L)
        .quitAnalysis(gettextf("%1$s: the data has levels of '%2$s' that are not in the fitting data: %3$s.", title,
                               label, paste(utils::head(unknown, 5L), collapse = ", ")))
      data[[variable]] <- .modelTermsAlignFactor(x, prepared[["levels"]][[variable]])
    }
  }

  for (version in Filter(function(version) !version[["categorical"]] && is.null(version[["constant"]]), model[["versions"]])) {
    x <- data[[version[["variable"]]]]
    if (!is.numeric(x))
      .quitAnalysis(gettextf("%1$s: the variable '%2$s' must be numeric.", title, decodeColNames(version[["variable"]])))
    .modelTermsCheckValues(version, x, title)
  }

  return(data)
}

# Nesting ---------------------------------------------------------------------------------------------------------

# The terms of a model as keys that name the variables and their full specification. A term of one model is a term
# of the next model when the keys agree and the next model does not restrict it more than this one: a free term
# contains a restricted one, and equal restrictions (compared as values: "1/2" is 0.5) are equal.
.modelTermsNested <- function(models) {

  if (length(models) < 2L)
    return(logical(0L))

  keys <- lapply(models, function(model) {
    if (model[["advanced"]])
      vapply(model[["keys"]], function(key) paste(sort(key), collapse = "&"), "")
    else
      vapply(model[["layout"]], function(term) paste(sort(term[["variables"]]), collapse = "&"), "")
  })
  fixed <- lapply(models, function(model) if (model[["advanced"]]) model[["fixed"]] else rep(list(NA_real_), nrow(model[["terms"]])))

  # An offset is a term without coefficient: it is in a model or it is not, and adding or dropping one does not give a
  # nested model. So the models must have the same offsets, and the others are compared as terms.
  offsets      <- lapply(models, function(model) model[["terms"]][["offset"]])
  offsetKeys   <- Map(function(key, offset) unname(sort(key[offset])), keys, offsets)
  keys         <- Map(function(key, offset) key[!offset], keys, offsets)
  fixed        <- Map(function(values, offset) values[!offset], fixed, offsets)

  # The intercept is one more term: free (NA), or fixed at a value, where a model without intercept is fixed at 0
  intercepts <- vapply(models, function(model) if (model[["intercept"]]) model[["interceptFixed"]] else 0, numeric(1L))

  return(vapply(seq_len(length(models) - 1L), function(index) {
    current <- keys[[index]]
    larger  <- keys[[index + 1L]]
    termsFit <- all(vapply(seq_along(current), function(term) {
      candidates <- which(larger == current[term])
      any(vapply(fixed[[index + 1L]][candidates], function(values) all(is.na(values)) || identical(values, fixed[[index]][[term]]), TRUE))
    }, logical(1L)))
    interceptFits <- is.na(intercepts[index + 1L]) || (!is.na(intercepts[index]) && intercepts[index] == intercepts[index + 1L])
    termsFit && interceptFits && identical(offsetKeys[[index]], offsetKeys[[index + 1L]])
  }, logical(1L)))
}

# Advanced mode ---------------------------------------------------------------------------------------------------

.modelTermsAdvancedModel <- function(model, data, levels) {

  title <- model[["title"]]
  if (is.null(model[["definitions"]]) && length(model[["components"]]) > 0L)
    .quitAnalysis(gettextf("%1$s: the model has terms but no term definitions.", title))

  # The definitions of the model by id; the terms refer to them through their operands
  definitions <- as.list(model[["definitions"]])
  ids         <- vapply(definitions, `[[`, "", "id")
  if (any(!nzchar(ids)) || anyDuplicated(ids))
    .quitAnalysis(gettextf("%1$s: the term definitions need unique, nonempty ids.", title))
  names(definitions) <- ids

  terms   <- model[["terms"]]
  termIds <- vapply(terms, `[[`, "", "id")
  if (any(!nzchar(termIds)) || anyDuplicated(termIds))
    .quitAnalysis(gettextf("%1$s: the terms need unique, nonempty ids.", title))
  for (term in terms) {
    if (length(term[["operands"]]) == 0L || anyDuplicated(term[["operands"]]))
      .quitAnalysis(gettextf("%1$s: the term '%2$s' needs one or more different definitions as operands.", title, term[["id"]]))
    unknown <- setdiff(term[["operands"]], ids)
    if (length(unknown) > 0L)
      .quitAnalysis(gettextf("%1$s: the term '%2$s' refers to '%3$s', which is not a term definition of this model.",
                             title, term[["id"]], unknown[1L]))
  }

  # Only the definitions that a term uses count: the others are neither read nor checked against the data
  used <- as.character(unique(unlist(lapply(terms, `[[`, "operands"), use.names = FALSE)))
  # (a constant offset has no variable but a number)
  isConstant <- vapply(definitions, function(definition) definition[["specification"]] == "offsetConstant", TRUE)
  for (id in used) {
    definition <- definitions[[id]]
    if (!nzchar(definition[["specification"]]) || (!isConstant[[id]] && !nzchar(definition[["variable"]])))
      .quitAnalysis(gettextf("%1$s: the definition '%2$s' needs a variable and a specification.", title, id))
  }
  for (id in used[isConstant[used]])
    definitions[[id]][["value"]] <- .modelTermsConstantValue(definitions[[id]], title)

  # An offset is a main effect without coefficient: it is no operand of an interaction and has nothing to restrict
  isOffset <- isConstant | vapply(definitions, function(definition) definition[["specification"]] == "offset", TRUE)
  for (term in terms) {
    offsets <- term[["operands"]][isOffset[term[["operands"]]]]
    if (length(term[["operands"]]) > 1L && length(offsets) > 0L)
      .quitAnalysis(gettextf("%1$s: the term '%2$s' is an interaction with %3$s; an offset can only be a main effect.",
                             title, term[["id"]], .modelTermsLabel(definitions[[offsets[1L]]])))
  }
  for (id in used[isOffset[used]]) {
    if (nzchar(definitions[[id]][["restriction"]]))
      .quitAnalysis(gettextf("%1$s: %2$s has no coefficient, so it cannot have a restriction.",
                             title, .modelTermsLabel(definitions[[id]])))
  }

  # Indicator coding has a column for every level, which add up to the intercept
  if (model[["intercept"]])
    for (id in used[vapply(definitions[used], function(definition) definition[["specification"]] == "indicator", TRUE)])
      .quitAnalysis(gettextf("%1$s: the indicator coding of '%2$s' (one column per level) needs a model without intercept.",
                             title, decodeColNames(definitions[[id]][["variable"]])))

  # The definitions of one variable are checked together; a constant offset has no variable
  regular   <- used[!isConstant[used]]
  variables <- vapply(definitions[regular], `[[`, "", "variable")
  for (variable in unique(variables))
    .modelTermsCheckDefinitions(definitions[regular][variables == variable], variable, variable %in% names(levels), title)

  versions <- list()
  version  <- function(id) {
    if (is.null(versions[[id]]))
      versions[[id]] <<- .modelTermsVersion(definitions[[id]], data, levels, title)
    return(versions[[id]])
  }

  # Different definitions can build the same R expression (power 2 and root 1/2 are both x^2). R would merge them, also
  # inside an interaction (x^2:x^2 is x^2), and later terms would get the wrong labels, so the user has to pick one.
  expressions <- vapply(used, function(id) .modelTermsDeparse(version(id)[["call"]]), "")
  if (anyDuplicated(expressions))
    .quitAnalysis(gettextf("%1$s: two term definitions give the same term %2$s; use only one of them.", title,
                           version(used[anyDuplicated(expressions)])[["label"]]))

  # The terms in the order of the model. R orders the variables of a term by their first appearance in the formula;
  # so do the labels and the columns. The restriction of a definition applies to its main effect only.
  built <- lapply(terms, function(term) {
    operands    <- term[["operands"]][order(match(term[["operands"]], used))]
    restriction <- if (length(operands) == 1L) definitions[[operands]][["restriction"]] else ""
    .modelTermsTerm(term[["id"]], lapply(operands, version), restriction, title)
  })

  # Two terms of one model are different products of definitions (in any order)
  operandSets <- vapply(terms, function(term) paste(sort(term[["operands"]]), collapse = "&"), "")
  if (anyDuplicated(operandSets))
    .quitAnalysis(gettextf("%1$s: two terms have the same operands: %2$s.", title, built[[anyDuplicated(operandSets)]][["label"]]))

  # The formula keeps the order of the model; the terms are listed in the order lm() orders them: by degree. The
  # offsets are no terms of lm(): they follow the other terms.
  calls  <- lapply(built, `[[`, "call")
  sorted <- built[order(vapply(built, `[[`, TRUE, "offset"), vapply(built, `[[`, 1L, "degree"))]
  table  <- data.frame(
    id          = vapply(sorted, `[[`, "", "id"),
    type        = vapply(sorted, `[[`, "", "type"),
    label       = vapply(sorted, `[[`, "", "label"),
    expression  = vapply(sorted, `[[`, "", "expression"),
    restriction = vapply(sorted, `[[`, "", "restriction"),
    offset      = vapply(sorted, `[[`, TRUE, "offset"),
    stringsAsFactors = FALSE
  )

  # A fixed intercept needs an intercept: the restriction of a model without one is not read
  intercept      <- model[["intercept"]]
  interceptFixed <- if (intercept && nzchar(model[["interceptRestriction"]]))
    .modelTermsRestriction(model[["interceptRestriction"]], 1L, title, "(Intercept)")
  else
    NA_real_

  return(list(name = model[["name"]], title = title, advanced = TRUE, terms = table,
              intercept = intercept, interceptFixed = interceptFixed,
              sampleDependent = any(vapply(versions[used], function(version) isTRUE(version[["sampleDependent"]]), TRUE)),
              formula = .modelTermsFormulaObject(calls[!vapply(built, `[[`, TRUE, "constant")], intercept = intercept), calls = calls,
              offsetCalls = vapply(built, `[[`, TRUE, "offset"), constantCalls = vapply(built, `[[`, TRUE, "constant"),
              columns = lapply(sorted, `[[`, "columns"), fixed = lapply(sorted, `[[`, "fixed"),
              keys = lapply(sorted, `[[`, "keys"), versions = unname(versions[used]),
              variables = unique(unlist(lapply(sorted, `[[`, "variables"), use.names = FALSE))))
}

# The definitions of one variable that a model uses: a categorical variable has at most one, a continuous variable
# has none that repeat the same specification with the same parameters.
.modelTermsCheckDefinitions <- function(definitions, variable, categorical, title) {

  label <- decodeColNames(variable)
  if (categorical && any(vapply(definitions, function(definition) definition[["specification"]] == "offset", TRUE)))
    .quitAnalysis(gettextf("%1$s: the offset of '%2$s' needs a continuous variable, but '%2$s' is nominal.", title, label))
  if (categorical && length(definitions) > 1L)
    .quitAnalysis(gettextf("%1$s: the nominal variable '%2$s' can have only one term definition.", title, label))

  keys <- vapply(definitions, function(definition) .modelTermsKey(definition, .modelTermsParameters(definition, title)), "")
  if (anyDuplicated(keys))
    .quitAnalysis(gettextf("%1$s: '%2$s' has two term definitions with the same settings.", title, label))

  return(invisible())
}

# One term of a model: the product of its operands. The columns of the term (the labels in the order of the
# model matrix) come from the columns of the operands, the first operand varying fastest.
.modelTermsTerm <- function(id, versions, restriction, title) {

  columns    <- .modelTermsProductLabels(lapply(versions, `[[`, "columns"))
  termCall   <- Reduce(function(left, right) call(":", left, right), lapply(versions, `[[`, "call"))
  expression <- .modelTermsDeparse(termCall)
  label      <- paste(vapply(versions, `[[`, "", "label"), collapse = interactionSymbol)

  if (nchar(expression) > 2000L)
    .quitAnalysis(gettextf("%1$s: the term %2$s is too long to be used in a model formula (too many levels, knots or bins).",
                           title, label))

  return(list(id = id, type = if (length(versions) == 1L) "main" else "interaction", label = label,
              expression = expression, restriction = restriction, call = termCall, degree = length(versions),
              offset = any(vapply(versions, function(version) isTRUE(version[["offset"]]), TRUE)),
              constant = any(vapply(versions, function(version) !is.null(version[["constant"]]), TRUE)),
              columns = columns, fixed = .modelTermsRestriction(restriction, length(columns), title, label),
              keys = vapply(versions, `[[`, "", "key"), variables = unlist(lapply(versions, `[[`, "variable"), use.names = FALSE)))
}

# The key of a definition names its variable and its parsed settings, so "10" and "5+5" are the same base.
.modelTermsKey <- function(definition, parameters) {

  keys       <- as.character(names(parameters))
  parameters <- parameters[sort(keys[!endsWith(keys, "Text")])]   # the parsed values; the typed texts may differ ("10", "5+5")
  values     <- vapply(parameters, function(value) if (is.numeric(value)) format(value, digits = 17L) else as.character(value), "")

  return(paste(c(definition[["variable"]], definition[["specification"]], values), collapse = "|"))
}

# Specifications -------------------------------------------------------------------------------------------------

# The specifications are listed once, in inst/modelTermSpecifications.json: their kind, their parameters with defaults,
# and the label of their term. jasp-desktop embeds the file and builds the table of the form from it. A new
# specification needs an entry there and here: its parameters (.modelTermsParameters()) and its columns
# (.modelTermsContinuousVersion(), or .modelTermsCoding() and .modelTermsCodingParts() for a coding); a new domain also
# .modelTermsInvalid() and .modelTermsCheckValues(); a sample-dependent one .modelTermsVersion(). Tests check that every
# entry has a builder and a label.
.modelTermsCatalogueCache <- new.env(parent = emptyenv())

.modelTermsCataloguePath <- function() system.file("modelTermSpecifications.json", package = "jaspBase")

.modelTermsCatalogue <- function() {

  if (is.null(.modelTermsCatalogueCache[["specifications"]])) {
    specifications <- jsonlite::fromJSON(.modelTermsCataloguePath(), simplifyVector = FALSE)[["specifications"]]
    names(specifications) <- vapply(specifications, `[[`, "", "name")
    .modelTermsCatalogueCache[["specifications"]] <- specifications
  }

  return(.modelTermsCatalogueCache[["specifications"]])
}

.modelTermsSpecificationsOf <- function(kind) {
  return(names(Filter(function(specification) specification[["kind"]] == kind, .modelTermsCatalogue())))
}

# A parameter of a specification in the catalogue (NULL: the specification has no such parameter)
.modelTermsParameter <- function(specification, parameter) {

  for (candidate in .modelTermsCatalogue()[[specification]][["parameters"]])
    if (candidate[["name"]] == parameter)
      return(candidate)

  return(NULL)
}

# The default of a parameter, for a definition that does not give it
.modelTermsParameterDefault <- function(specification, parameter) {
  return(.modelTermsParameter(specification, parameter)[["default"]])
}

# The label of a definition's term, as the form shows it: the `label` pattern of its specification in the catalogue,
# filled in as the desktop does (FactorsFormBase::_label): {x} is the variable, {parameter} its setting as typed,
# {parameter|paren} parenthesized unless a plain number, {parameter|signed} " + v" or " - v", {parameter|choice} the
# short text of the choice, {parameter|number} the number written shortest (.modelTermsShortest()), and [ ... ] is left out while every
# parameter in it has its default (a reference level: the first level). `levels`: those of a categorical variable.
.modelTermsLabel <- function(definition, levels = NULL) {

  specification <- definition[["specification"]]
  isLevel       <- function(name) identical(.modelTermsParameter(specification, name)[["type"]], "level")
  value         <- function(name) {
    text <- .modelTermsString(definition[[name]], trim = !isLevel(name))
    if (nzchar(text)) text else if (isLevel(name)) (if (length(levels) > 0L) levels[1L] else "") else .modelTermsParameterDefault(specification, name)
  }
  fill <- function(placeholder) {
    name   <- sub("\\|.*$", "", placeholder)
    filter <- if (grepl("|", placeholder, fixed = TRUE)) sub("^[^|]*\\|", "", placeholder) else ""
    if (name == "x")
      return(decodeColNames(definition[["variable"]]))
    text <- value(name)
    switch(filter,
           paren  = if (grepl("^[0-9]+(\\.[0-9]+)?$", text)) text else paste0("(", text, ")"),
           signed = if (startsWith(text, "-")) paste0(" - ", substring(text, 2L)) else paste0(" + ", text),
           number = .modelTermsShortest(as.numeric(text)),
           choice = {
             choices <- Filter(function(choice) choice[["value"]] == text, .modelTermsParameter(specification, name)[["choices"]])
             if (length(choices) > 0L) choices[[1L]][["short"]] else text
           },
           text)
  }
  placeholders <- function(text) {
    found <- regmatches(text, gregexpr("\\{[^}]*\\}", text))[[1L]]
    substring(found, 2L, nchar(found) - 1L)
  }
  render <- function(text) {
    for (placeholder in placeholders(text))
      text <- sub(paste0("{", placeholder, "}"), fill(placeholder), text, fixed = TRUE)
    text
  }

  # the pattern in pieces: [optional parts], {placeholders} and text
  pattern <- .modelTermsCatalogue()[[specification]][["label"]]
  pieces  <- regmatches(pattern, gregexpr("\\[[^]]*\\]|[^[]+", pattern))[[1L]]
  label   <- vapply(pieces, function(piece) {
    if (!startsWith(piece, "["))
      return(render(piece))
    inner <- substring(piece, 2L, nchar(piece) - 1L)
    names <- setdiff(sub("\\|.*$", "", placeholders(inner)), "x")
    atDefault <- vapply(names, function(name) {
      if (!isLevel(name))
        return(value(name) == .modelTermsParameterDefault(specification, name))
      if (length(levels) > 0L) isTRUE(value(name) == levels[1L]) else !nzchar(value(name))
    }, TRUE)
    if (all(atDefault)) "" else render(inner)
  }, "")

  label <- paste(label, collapse = "")

  return(if (label == "(Intercept)") "`(Intercept)`" else label)   # a column named (Intercept) is not the intercept
}

# A number written with as few digits as give it back exactly, as the desktop writes it (Qt's shortest form)
.modelTermsShortest <- function(value) {

  for (digits in seq_len(17L)) {
    text <- sprintf("%.*g", digits, value)
    if (as.numeric(text) == value)
      return(text)
  }

  return(sprintf("%.17g", value))
}

# Evaluate a number written as an expression ("10", "exp(1)", "1/3"). Only arithmetic and mathematical functions
# are allowed, and they are looked up in an environment of their own, so nothing else can run.
.modelTermsEvaluate <- function(text) {

  allowed <- c("+", "-", "*", "/", "^", "%%", "%/%", "(", "c", "abs", "sign", "sqrt", "exp", "expm1", "log", "log1p",
               "log2", "log10", "round", "signif", "floor", "ceiling", "trunc", "sin", "cos", "tan", "asin", "acos",
               "atan", "sinh", "cosh", "tanh", "min", "max", "sum", "prod", "mean")
  valid <- function(node) {
    if (is.numeric(node) || is.logical(node))
      return(TRUE)
    if (is.name(node))
      return(as.character(node) %in% c("pi", "Inf"))
    return(is.call(node) && is.name(node[[1L]]) && as.character(node[[1L]]) %in% allowed &&
             all(vapply(as.list(node)[-1L], valid, logical(1L))))
  }

  parsed <- tryCatch(parse(text = text, keep.source = FALSE), error = function(e) NULL)
  if (length(parsed) != 1L || !tryCatch(valid(parsed[[1L]]), error = function(e) FALSE))
    return(NULL)

  environment <- list2env(c(stats::setNames(lapply(allowed, get, envir = baseenv()), allowed), list(pi = pi, "Inf" = Inf)),
                          parent = emptyenv())
  value <- tryCatch(eval(parsed[[1L]], environment), error = function(e) NULL, warning = function(w) NULL)

  return(if (is.numeric(value)) value else NULL)
}

# One number of a specification: a single finite value that passes `valid`.
.modelTermsNumber <- function(definition, key, description, requirement, valid, title, default = NULL) {

  text <- .modelTermsString(definition[[key]], default = "")
  if (!nzchar(text) && !is.null(default))
    text <- default
  value <- if (nzchar(text)) .modelTermsEvaluate(text)
  label <- decodeColNames(definition[["variable"]])

  if (!is.numeric(value) || length(value) != 1L || !is.finite(value))
    .quitAnalysis(gettextf("%1$s: %2$s for '%3$s' must be a single finite number or numeric expression, not '%4$s'.",
                           title, description, label, text))
  if (!valid(value))
    .quitAnalysis(gettextf("%1$s: %2$s for '%3$s' %4$s.", title, description, label, requirement))

  return(list(value = value, text = text))
}

# A location (center, knot, cutoff): mean or median of the fitting data, or a number. Checked here and written as
# "mean", "median" or the number with 17 digits (so "2.5" and "5/2" are the same setting); the value is learned with
# the data (.modelTermsLocation()).
.modelTermsLocationText <- function(definition, key, description, title, default) {

  text <- .modelTermsString(definition[[key]], default = "")
  if (!nzchar(text))
    text <- default
  if (text %in% c("mean", "median"))
    return(text)

  value <- .modelTermsEvaluate(text)
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value))
    .quitAnalysis(gettextf("%1$s: %2$s for '%3$s' must be mean, median or a single finite number, not '%4$s'.",
                           title, description, decodeColNames(definition[["variable"]]), text))

  return(format(value, digits = 17L))
}

.modelTermsLocation <- function(location, x) {

  return(switch(location,
                mean   = mean(x),
                median = stats::median(x),
                as.numeric(location)))
}

# The settings of a definition, checked and parsed; independent of the data. A parameter the definition does not give
# takes its default from the catalogue.
.modelTermsParameters <- function(definition, title) {

  specification <- definition[["specification"]]
  if (is.null(.modelTermsCatalogue()[[specification]]))
    .quitAnalysis(gettextf("%1$s: '%2$s' is not a known term specification.", title, specification))

  number   <- function(key, description, requirement, valid)
    .modelTermsNumber(definition, key, description, requirement, valid, title, .modelTermsParameterDefault(specification, key))
  anything <- function(value) TRUE
  whole    <- function(value) value >= 1 && value == round(value)
  location <- function(key, description)
    .modelTermsLocationText(definition, key, description, title, .modelTermsParameterDefault(specification, key))

  return(switch(
    specification,
    center = list(center = location("center", gettext("the center"))),
    logShift = {
      shift <- number("shift", gettext("the shift"), "", anything)
      list(shift = shift[["value"]], shiftText = shift[["text"]])
    },
    logBase = {
      base       <- number("logBase", gettext("the logarithm base"), gettext("must be positive and different from 1"),
                           function(value) value > 0 && value != 1)
      multiplier <- number("logMultiplier", gettext("the logarithm multiplier"), gettext("must not be 0"),
                           function(value) value != 0)
      list(base = base[["value"]], baseText = base[["text"]], multiplier = multiplier[["value"]], multiplierText = multiplier[["text"]])
    },
    power = {
      exponent <- number("exponent", gettext("the exponent"), gettext("must not be 0"), function(value) value != 0)
      list(exponent = exponent[["value"]], exponentText = exponent[["text"]])
    },
    root = {
      degree <- number("rootDegree", gettext("the root degree"), gettext("must not be 0"), function(value) value != 0)
      list(degree = degree[["value"]], degreeText = degree[["text"]])
    },
    boxCox = , yeoJohnson = {
      lambda <- number("lambda", gettext("lambda"), "", anything)
      list(lambda = lambda[["value"]], lambdaText = lambda[["text"]])
    },
    polynomial = , rawPolynomial = list(degree = number("polynomialDegree", gettext("the polynomial degree"),
                                                        gettext("must be a whole number of at least 1"), whole)[["value"]]),
    naturalSpline = list(df = number("splineDf", gettext("the degrees of freedom of the spline"),
                                     gettext("must be a whole number of at least 1"), whole)[["value"]]),
    bSpline = {
      df     <- number("splineDf", gettext("the degrees of freedom of the spline"), gettext("must be a whole number of at least 1"), whole)[["value"]]
      degree <- number("splineDegree", gettext("the degree of the spline"), gettext("must be a whole number of at least 1"), whole)[["value"]]
      if (df < degree)
        .quitAnalysis(gettextf("%1$s: the B-spline of '%2$s' needs at least as many degrees of freedom as its degree (%3$s).",
                               title, decodeColNames(definition[["variable"]]), degree))
      list(df = df, degree = degree)
    },
    hinge     = list(knot = location("knot", gettext("the knot"))),
    threshold = list(cutoff = location("cutoff", gettext("the cutoff"))),
    binning   = list(bins = number("bins", gettext("the number of bins"), gettext("must be a whole number of at least 2"),
                                   function(value) value >= 2 && value == round(value))[["value"]]),
    fourier   = {
      period    <- number("period", gettext("the period"), gettext("must be positive"), function(value) value > 0)
      harmonics <- number("harmonics", gettext("the number of harmonics"), gettext("must be a whole number of at least 1"), whole)
      list(period = period[["value"]], periodText = period[["text"]], harmonics = harmonics[["value"]])
    },
    arrhenius = {
      unit  <- .modelTermsString(definition[["temperatureUnit"]], .modelTermsParameterDefault(specification, "temperatureUnit"))
      units <- vapply(.modelTermsParameter(specification, "temperatureUnit")[["choices"]], `[[`, "", "value")
      if (!unit %in% units)
        .quitAnalysis(gettextf("%1$s: '%2$s' is not a known temperature unit.", title, unit))
      list(unit = unit)
    },
    treatment = , simple = list(reference = .modelTermsString(definition[["referenceLevel"]], trim = FALSE)),
    list()
  ))
}

# One version of a variable for one definition: the call that computes its columns, its labels, and what is needed to
# check its domain on new data. Constants are learned on the fitting data here.
.modelTermsVersion <- function(definition, data, levels, title) {

  if (definition[["specification"]] == "offsetConstant")
    return(.modelTermsConstantVersion(definition, title))

  variable      <- definition[["variable"]]
  specification <- definition[["specification"]]
  name          <- decodeColNames(variable)
  parameters    <- .modelTermsParameters(definition, title)
  categorical   <- specification %in% .modelTermsSpecificationsOf("categorical")

  if (!variable %in% names(data))
    .quitAnalysis(gettextf("%1$s: the data has no variable '%2$s'.", title, name))
  x     <- data[[variable]]
  label <- .modelTermsLabel(definition, levels[[variable]])
  if (categorical != (variable %in% names(levels)))
    .quitAnalysis(if (categorical)
      gettextf("%1$s: %2$s needs a nominal variable, but '%3$s' is not nominal.", title, label, name)
    else
      gettextf("%1$s: %2$s needs a continuous variable, but '%3$s' is nominal.", title, label, name))

  version <- if (categorical)
    .modelTermsCategoricalVersion(specification, parameters, variable, x, levels[[variable]], title)
  else
    .modelTermsContinuousVersion(specification, parameters, variable, x, label, title)

  version[["label"]]           <- label
  version[["columns"]]         <- if (is.null(version[["columns"]])) label else version[["columns"]]   # one column: the term
  version[["sampleDependent"]] <- specification %in% c("rank", "normalScores")
  version[["id"]]              <- definition[["id"]]
  version[["variable"]]        <- variable
  version[["specification"]]   <- specification
  version[["categorical"]]     <- categorical
  version[["key"]]             <- if (categorical) paste(c(variable, specification, version[["reference"]]), collapse = "|")
                                  else .modelTermsKey(definition, parameters)
  if (categorical)
    return(version)

  # The version must be defined for every fitting value, and its columns are what the labels describe.
  .modelTermsCheckValues(version, x, title)
  if (specification == "offset")   # its values are the variable, checked to be numeric; there is nothing to compute
    return(version)
  values <- eval(version[["call"]], stats::setNames(list(x), variable), baseenv())
  if (NCOL(values) != length(version[["columns"]]) || any(!is.finite(values)) && specification != "none")
    .quitAnalysis(gettextf("%1$s: %2$s cannot be computed for the fitting data.", title, version[["label"]]))

  return(version)
}

# The call and the domain of a continuous version; `columns` only for a specification with several columns (the column
# of the others is labelled as the term). `term` is the label of the term, for messages.
.modelTermsContinuousVersion <- function(specification, parameters, variable, x, term, title) {

  label  <- decodeColNames(variable)
  symbol <- as.name(variable)
  if (!is.numeric(x))
    .quitAnalysis(gettextf("%1$s: %2$s needs a numeric variable, but '%3$s' is not numeric.", title, term, label))
  # `none` and an offset take the values as they are, also when they are missing; the others learn from them
  count <- sum(!is.finite(x))
  if (!specification %in% c("none", "offset") && count > 0L)
    .quitAnalysis(sprintf(ngettext(count,
                                   "%1$s: %2$s needs finite values of %3$s; %4$d value is missing or infinite.",
                                   "%1$s: %2$s needs finite values of %3$s; %4$d values are missing or infinite."),
                          title, term, label, count))

  domain  <- "none"
  unit    <- NULL
  bound   <- NULL
  columns <- NULL
  switch(
    specification,
    none = {
      expression <- symbol
    },
    center = {
      expression <- bquote(I(.(symbol) - .(.modelTermsLocation(parameters[["center"]], x))))
    },
    scale = {
      deviation <- stats::sd(x)
      if (!is.finite(deviation) || deviation == 0)
        .quitAnalysis(gettextf("%1$s: Scale(%2$s) needs a standard deviation that is positive; '%2$s' is constant.", title, label))
      expression <- bquote(I((.(symbol) - .(mean(x))) / .(deviation)))
    },
    log = {
      domain     <- "positive"
      expression <- bquote(log(.(symbol)))
    },
    logShift = {
      domain     <- "above"
      bound      <- -parameters[["shift"]]
      expression <- bquote(log(.(symbol) + .(parameters[["shift"]])))
    },
    logBase = {
      domain     <- "positive"
      expression <- if (parameters[["multiplier"]] == 1)
        bquote(log(.(symbol), base = .(parameters[["base"]])))
      else
        bquote(I(.(parameters[["multiplier"]]) * log(.(symbol), base = .(parameters[["base"]]))))
    },
    reciprocal = {
      domain     <- "nonzero"
      expression <- bquote(I(1 / .(symbol)))
    },
    power = {
      domain     <- "finite"
      expression <- bquote(I(.(symbol)^.(parameters[["exponent"]])))
    },
    root = {
      domain   <- "finite"
      exponent <- 1 / parameters[["degree"]]
      # An odd root keeps the sign of a negative value
      odd <- parameters[["degree"]] == round(parameters[["degree"]]) && abs(parameters[["degree"]]) %% 2 == 1
      expression <- if (odd)
        bquote(I(sign(.(symbol)) * abs(.(symbol))^.(exponent)))
      else
        bquote(I(.(symbol)^.(exponent)))
    },
    boxCox = {
      domain     <- "positive"
      lambda     <- parameters[["lambda"]]
      expression <- if (lambda == 0) bquote(log(.(symbol))) else bquote(I((.(symbol)^.(lambda) - 1) / .(lambda)))
    },
    yeoJohnson = {
      # the two branches of Yeo and Johnson (2000), each zero on the other side of 0, so no branch sees a value it is
      # not defined for
      lambda     <- parameters[["lambda"]]
      positive   <- if (lambda == 0) bquote(log1p(pmax(.(symbol), 0))) else bquote(((pmax(.(symbol), 0) + 1)^.(lambda) - 1) / .(lambda))
      negative   <- if (lambda == 2) bquote(-log1p(-pmin(.(symbol), 0))) else bquote(-((1 - pmin(.(symbol), 0))^.(2 - lambda) - 1) / .(2 - lambda))
      expression <- bquote(I(.(positive) + .(negative)))
    },
    asinh = {
      expression <- bquote(asinh(.(symbol)))
    },
    logit = {
      domain     <- "openUnit"
      expression <- bquote(stats::qlogis(.(symbol)))
    },
    probit = {
      domain     <- "openUnit"
      expression <- bquote(stats::qnorm(.(symbol)))
    },
    arcsineSqrt = {
      domain     <- "unit"
      expression <- bquote(asin(sqrt(.(symbol))))
    },
    polynomial = {
      degree <- parameters[["degree"]]
      if (length(unique(x)) <= degree)
        .quitAnalysis(gettextf("%1$s: a polynomial of degree %2$s needs at least %3$s different values of '%4$s'.",
                               title, degree, degree + 1, label))
      coefficients <- attr(stats::poly(x, degree = degree), "coefs")
      expression   <- bquote(stats::poly(.(symbol), degree = .(degree), coefs = .(call("list", alpha = .modelTermsVector(coefficients[["alpha"]]),
                                                                                       norm2 = .modelTermsVector(coefficients[["norm2"]])))))
      columns      <- gettextf("Polynomial(%1$s, %2$s)", label, seq_len(degree))
    },
    rawPolynomial = {
      degree <- parameters[["degree"]]
      if (length(unique(x)) <= degree)
        .quitAnalysis(gettextf("%1$s: a polynomial of degree %2$s needs at least %3$s different values of '%4$s'.",
                               title, degree, degree + 1, label))
      expression <- bquote(stats::poly(.(symbol), degree = .(degree), raw = TRUE))
      columns    <- c(label, sprintf("%s^%d", label, seq_len(degree)[-1L]))
    },
    naturalSpline = {
      df <- parameters[["df"]]
      if (length(unique(x)) <= df)
        .quitAnalysis(gettextf("%1$s: a natural cubic spline with %2$s degrees of freedom needs at least %3$s different values of '%4$s'.",
                               title, df, df + 1, label))
      knots    <- if (df > 1L) stats::quantile(x, seq_len(df - 1L) / df, names = FALSE)
      boundary <- range(x)
      if (any(diff(c(boundary[1L], knots, boundary[2L])) <= 0))
        .quitAnalysis(gettextf("%1$s: the knots of the natural cubic spline of '%2$s' cannot be placed: the values are too concentrated for %3$s degrees of freedom.",
                               title, label, df))
      expression <- bquote(splines::ns(.(symbol), knots = .(.modelTermsVector(knots)), Boundary.knots = .(.modelTermsVector(boundary)), intercept = FALSE))
      columns    <- gettextf("Natural cubic spline(%1$s, %2$s)", label, seq_len(df))
    },
    bSpline = {
      df     <- parameters[["df"]]
      degree <- parameters[["degree"]]
      if (length(unique(x)) <= df)
        .quitAnalysis(gettextf("%1$s: a B-spline with %2$s degrees of freedom needs at least %3$s different values of '%4$s'.",
                               title, df, df + 1, label))
      inner    <- df - degree
      knots    <- if (inner > 0L) stats::quantile(x, seq_len(inner) / (inner + 1L), names = FALSE)
      boundary <- range(x)
      if (any(diff(c(boundary[1L], knots, boundary[2L])) <= 0))
        .quitAnalysis(gettextf("%1$s: the knots of the B-spline of '%2$s' cannot be placed: the values are too concentrated for %3$s degrees of freedom.",
                               title, label, df))
      expression <- bquote(splines::bs(.(symbol), knots = .(.modelTermsVector(knots)), degree = .(degree),
                                       Boundary.knots = .(.modelTermsVector(boundary)), intercept = FALSE))
      columns    <- gettextf("B-spline(%1$s, %2$s)", label, seq_len(df))
    },
    hinge = {
      expression <- bquote(I(pmax(.(symbol) - .(.modelTermsLocation(parameters[["knot"]], x)), 0)))
    },
    threshold = {
      expression <- bquote(I(as.numeric(.(symbol) > .(.modelTermsLocation(parameters[["cutoff"]], x)))))
    },
    binning = {
      # quantile groups: the breaks are learned, a value goes into the bin [lower, upper) (the outer bins are open), and
      # the bins are coded as a treatment coding with the first bin as reference
      bins   <- parameters[["bins"]]
      breaks <- stats::quantile(x, seq_len(bins - 1L) / bins, names = FALSE)
      if (any(diff(c(min(x), breaks)) <= 0) || breaks[bins - 1L] >= max(x))
        .quitAnalysis(gettextf("%1$s: '%2$s' cannot be split into %3$s bins: the values are too concentrated.", title, label, bins))
      expression <- bquote(stats::contr.treatment(.(bins))[findInterval(.(symbol), .(.modelTermsVector(breaks))) + 1L, , drop = FALSE])
      gaps       <- diff(c(min(x), breaks, max(x)))
      digits     <- 4L   # as few as keep the bounds apart and close to their values
      while (digits < 15L && (anyDuplicated(signif(breaks, digits)) || any(abs(signif(breaks, digits) - breaks) > min(gaps) / 20)))
        digits <- digits + 1L
      shown      <- formatC(signif(breaks, digits), digits = digits, format = "fg")
      columns    <- sprintf("%s [%s, %s)", label, shown, c(shown[-1L], "\u221E"))
    },
    fourier = {
      # sine and cosine of every harmonic h: sin(2 pi h x / P), cos(2 pi h x / P), with 2 pi h / P as a number (a name
      # `pi` would be looked up in the data)
      period     <- parameters[["period"]]
      harmonics  <- seq_len(parameters[["harmonics"]])
      waves      <- unlist(lapply(harmonics, function(harmonic) list(bquote(sin(.(2 * pi * harmonic / period) * .(symbol))),
                                                                     bquote(cos(.(2 * pi * harmonic / period) * .(symbol))))))
      expression <- as.call(c(list(as.name("cbind")), waves))
      angle      <- vapply(harmonics, function(harmonic) sprintf("%s\u03C0%s/%s", if (harmonic == 1L) "2" else 2 * harmonic, label, parameters[["periodText"]]), "")
      columns    <- as.vector(rbind(sprintf("sin(%s)", angle), sprintf("cos(%s)", angle)))
    },
    rank = {
      expression <- bquote(rank(.(symbol)))
    },
    normalScores = {
      expression <- bquote(stats::qnorm(rank(.(symbol)) / (length(.(symbol)) + 1)))
    },
    arrhenius = {
      domain     <- "temperature"
      unit       <- parameters[["unit"]]
      expression <- bquote(I(1 / (.(8.617333262e-5) * .(.modelTermsKelvinCall(symbol, unit)))))   # Boltzmann constant in eV/K
    },
    offset = {
      expression <- call("offset", symbol)
      columns    <- character(0)
    }
  )

  return(list(call = expression, columns = columns, domain = list(kind = domain, unit = unit, bound = bound),
              offset = specification == "offset"))
}

.modelTermsKelvinCall <- function(symbol, unit) {

  return(switch(unit, kelvin = symbol, celsius = bquote(.(symbol) + 273.15), fahrenheit = bquote((.(symbol) - 32) * 5 / 9 + 273.15)))
}

.modelTermsKelvin <- function(values, unit) {

  return(switch(unit, kelvin = values, celsius = values + 273.15, fahrenheit = (values - 32) * 5 / 9 + 273.15))
}

# A vector of constants as a call to c(): R compares the variables of a model by their calls, and it compares an
# embedded vector by its first element only (and an embedded list not at all). c() without values is NULL.
.modelTermsVector <- function(values) {

  if (length(values) == 0L)
    return(NULL)

  return(as.call(c(list(as.name("c")), as.list(values))))
}

# Where a version is not defined. `none` and the specifications that only need finite values have no domain.
.modelTermsInvalid <- function(version, values) {

  kind <- version[["domain"]][["kind"]]

  return(switch(
    kind,
    positive    = values <= 0,
    above       = values <= version[["domain"]][["bound"]],
    nonzero     = values == 0,
    openUnit    = values <= 0 | values >= 1,
    unit        = values < 0 | values > 1,
    temperature = .modelTermsKelvin(values, version[["domain"]][["unit"]]) <= 0,
    finite      = !is.finite(suppressWarnings(eval(version[["call"]], stats::setNames(list(values), version[["variable"]]), baseenv()))),
    rep(FALSE, length(values))
  ))
}

.modelTermsCheckValues <- function(version, values, title) {

  if (version[["specification"]] %in% c("none", "offset"))
    return(invisible())

  label <- version[["label"]]
  name  <- decodeColNames(version[["variable"]])
  count <- sum(!is.finite(values))
  if (count > 0L)
    .quitAnalysis(sprintf(ngettext(count,
                                   "%1$s: %2$s needs finite values of %3$s; %4$d value is missing or infinite.",
                                   "%1$s: %2$s needs finite values of %3$s; %4$d values are missing or infinite."),
                          title, label, name, count))

  count <- sum(.modelTermsInvalid(version, values))
  if (count > 0L)
    .quitAnalysis(switch(
      version[["domain"]][["kind"]],
      positive = sprintf(ngettext(count,
                                  "%1$s: %2$s needs positive values of %3$s; %4$d value is zero or negative.",
                                  "%1$s: %2$s needs positive values of %3$s; %4$d values are zero or negative."),
                         title, label, name, count),
      nonzero = sprintf(ngettext(count,
                                 "%1$s: %2$s needs nonzero values of %3$s; %4$d value is zero.",
                                 "%1$s: %2$s needs nonzero values of %3$s; %4$d values are zero."),
                        title, label, name, count),
      above = sprintf(ngettext(count,
                               "%1$s: %2$s needs values of %3$s above %5$s; %4$d value is not.",
                               "%1$s: %2$s needs values of %3$s above %5$s; %4$d values are not."),
                      title, label, name, count, format(version[["domain"]][["bound"]])),
      openUnit = sprintf(ngettext(count,
                                  "%1$s: %2$s needs values of %3$s between 0 and 1 (without 0 and 1); %4$d value is not.",
                                  "%1$s: %2$s needs values of %3$s between 0 and 1 (without 0 and 1); %4$d values are not."),
                         title, label, name, count),
      unit = sprintf(ngettext(count,
                              "%1$s: %2$s needs values of %3$s from 0 to 1; %4$d value is not.",
                              "%1$s: %2$s needs values of %3$s from 0 to 1; %4$d values are not."),
                     title, label, name, count),
      temperature = sprintf(ngettext(count,
                                     "%1$s: %2$s needs temperatures above absolute zero in the selected unit; %4$d value of %3$s is not.",
                                     "%1$s: %2$s needs temperatures above absolute zero in the selected unit; %4$d values of %3$s are not."),
                            title, label, name, count),
      sprintf(ngettext(count,
                       "%1$s: %2$s is not a finite number for %4$d value of %3$s.",
                       "%1$s: %2$s is not a finite number for %4$d values of %3$s."),
              title, label, name, count)
    ))

  return(invisible())
}

# Offsets ---------------------------------------------------------------------------------------------------------

# The number of a constant offset: one finite number
.modelTermsConstantValue <- function(definition, title) {

  value <- unlist(definition[["value"]], use.names = FALSE)
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value))
    .quitAnalysis(gettextf("%1$s: the value of a constant offset must be a finite number, not '%2$s'.", title,
                           paste(as.character(value), collapse = ", ")))

  return(as.numeric(value))
}

# A constant offset as a version: it has no variable and no columns, and it is not in the formula of the model. Its call
# only makes it a term (R never sees it).
.modelTermsConstantVersion <- function(definition, title) {

  value <- .modelTermsConstantValue(definition, title)

  return(list(call = call("offset", value), label = .modelTermsLabel(definition),
              columns = character(0), domain = list(kind = "none"), id = definition[["id"]], variable = character(0),
              specification = "offsetConstant", key = paste("offsetConstant", format(value, digits = 17L), sep = "|"),
              categorical = FALSE, offset = TRUE, constant = value))
}

# Factor codings ---------------------------------------------------------------------------------------------------

# The coded matrix of a factor, as a call with the levels of the fitting data embedded:
#   structure(stats::contr.treatment(k, base = b), dimnames = list(<levels>, NULL))[as.character(f), , drop = FALSE]
# Indexing by the level label stops with an error for a level that was not in the fitting data.
.modelTermsCategoricalVersion <- function(specification, parameters, variable, x, levels, title) {

  label <- decodeColNames(variable)
  size  <- length(levels)
  if (size < 2L)
    .quitAnalysis(gettextf("%1$s: the nominal variable '%2$s' needs at least two levels in the fitting data.", title, label))
  if (any(!nzchar(levels)))
    .quitAnalysis(gettextf("%1$s: the nominal variable '%2$s' has an empty level.", title, label))
  if (anyNA(x))
    .quitAnalysis(gettextf("%1$s: the nominal variable '%2$s' has missing values.", title, label))

  reference <- levels[1L]
  if (specification %in% c("treatment", "simple") && nzchar(parameters[["reference"]])) {
    reference <- parameters[["reference"]]
    if (!reference %in% levels)
      .quitAnalysis(gettextf("%1$s: the reference level '%2$s' of '%3$s' is not in the fitting data.", title, reference, label))
  }
  if (specification == "sas")
    reference <- levels[size]

  coded      <- call("structure", .modelTermsCoding(specification, size, match(reference, levels)), dimnames = call("list", .modelTermsVector(levels), NULL))
  expression <- substitute(CODED[as.character(VARIABLE), , drop = FALSE], list(CODED = coded, VARIABLE = as.name(variable)))
  parts      <- .modelTermsCodingParts(specification, levels, reference)

  return(list(call = expression, columns = paste0(label, " (", parts, ")"), domain = list(kind = "none"), reference = reference))
}

# The coding matrix of a factor with `size` levels (one row per level, one column per coefficient) as a call.
.modelTermsCoding <- function(coding, size, reference) {

  # successive differences (MASS::contr.sdif): column j is the difference of level j + 1 and level j, the intercept
  # the mean of the level means; C[i, j] = j / k - (i <= j)
  differences <- bquote(outer(rep(1, .(size)), seq_len(.(size - 1L))) / .(size) - outer(seq_len(.(size)), seq_len(.(size - 1L)), "<="))

  return(switch(
    coding,
    treatment = , sas      = bquote(stats::contr.treatment(.(size), base = .(reference))),
    simple                 = bquote(stats::contr.treatment(.(size), base = .(reference)) - 1 / .(size)),
    sum                    = bquote(stats::contr.sum(.(size))),
    helmert                = bquote(stats::contr.helmert(.(size))),
    reverseHelmert         = bquote(stats::contr.helmert(.(size))[.(size):1, .(size - 1L):1, drop = FALSE]),
    successiveDifferences  = differences,
    forwardDifference      = bquote(-.(differences)),
    orthogonalPolynomial   = bquote(stats::contr.poly(.(size))),
    indicator              = bquote(diag(.(size)))
  ))
}

# What each coded column of a factor contrasts, in the order of the columns.
.modelTermsCodingParts <- function(coding, levels, reference) {

  size <- length(levels)

  return(switch(
    coding,
    treatment = , simple = levels[levels != reference],
    sas       = levels[-size],
    sum       = levels[-size],
    indicator = levels,
    helmert   = vapply(seq_len(size - 1L), function(index)
      gettextf("%1$s vs. %2$s", levels[index + 1L], paste(levels[seq_len(index)], collapse = ", ")), ""),
    reverseHelmert = vapply(seq_len(size - 1L), function(index)
      gettextf("%1$s vs. %2$s", levels[index], paste(levels[(index + 1L):size], collapse = ", ")), ""),
    successiveDifferences = sprintf("%s \u2212 %s", levels[-1L], levels[-size]),
    forwardDifference     = sprintf("%s \u2212 %s", levels[-size], levels[-1L]),
    orthogonalPolynomial = vapply(seq_len(size - 1L), function(index)
      if (index <= 3L) c(gettext("linear"), gettext("quadratic"), gettext("cubic"))[index] else gettextf("degree %d", index), "")
  ))
}

# Restrictions ----------------------------------------------------------------------------------------------------

# One fixed value per column of the term, separated by commas or semicolons; "" estimates the term freely.
.modelTermsRestriction <- function(text, count, title, label) {

  if (!nzchar(text))
    return(rep(NA_real_, count))

  # the numbers as the arguments of one call to c(): "1) * (2" would otherwise be read as c(1) * (2)
  values <- paste0("c(", gsub("[;\r\n\t]+", ",", text), ")")
  parsed <- tryCatch(str2lang(values), error = function(e) NULL)
  value  <- if (is.call(parsed) && identical(parsed[[1L]], as.name("c"))) .modelTermsEvaluate(values)
  if (is.null(value) || length(value) == 0L || any(!is.finite(value)))
    .quitAnalysis(gettextf("%1$s: the restriction '%3$s' of %2$s must be a finite number or numbers separated by commas or semicolons.",
                           title, label, text))
  if (length(value) != count)
    .quitAnalysis(sprintf(ngettext(count,
                                   "%1$s: the restriction of %2$s needs %3$d value (one per coefficient), but %4$d were given.",
                                   "%1$s: the restriction of %2$s needs %3$d values (one per coefficient), but %4$d were given."),
                          title, label, count, length(value)))

  return(unname(value))
}
