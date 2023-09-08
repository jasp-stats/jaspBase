#' @name column-types
#' @rdname column-types
#' @importFrom vctrs vec_ptype2 vec_cast vec_ptype_abbr obj_print_footer
#' @title JASP Column Types
#'
#' @description Columns types in JASP.
#' @param x the main object for which the operation is done
NULL


# jaspScale -----
newJaspScale <- function(x = double()) {
  if (!rlang::is_double(x)) {
    rlang::abort("`x` must be a double vector.")
  }
  vctrs::new_vctr(x, class = "jaspScale")
}


#' @rdname column-types
#' @export
jaspScale <- function(x = double()) {
  x <- vctrs::vec_cast(x, double())
  newJaspScale(x)
}

#' @rdname column-types
#' @export
isJaspScale <- function(x) {
  inherits(x, "jaspScale")
}

#' @export
vec_ptype_abbr.jaspScale <- function(x, ...) {
  return("jspScl")
}

#' @export
format.jaspScale <- function(x, ...) {
  x <- vctrs::vec_data(x)
  miss <- is.na(x)
  infty <- is.infinite(x)
  valid <- !miss# & !infty

  out        <- rep(NA_character_, vctrs::vec_size(x))
  out[valid] <- format(x[valid], ...)
  #out[miss]  <- NA
  out[infty] <- ifelse(x[infty] > 0, "\u221E", "-\u221E")

  return(out)
}

#' @rdname column-types
#' @export
asJaspScale <- function(x, ...) {
  vctrs::vec_cast(x, newJaspScale())
}

## Coercion ----
#' @export
vec_ptype2.jaspScale.jaspScale <- function(x, y, ...) newJaspScale()

## Casting ----
### to jaspScale ----
#' @export
vec_cast.jaspScale.jaspScale <- function(x, to, ...) x
#' @export
vec_cast.jaspScale.double <- function(x, to, ...) jaspScale(x)
#' @export
vec_cast.jaspScale.integer <- function(x, to, ...) jaspScale(x)
#' @export
vec_cast.jaspScale.character <- function(x, to, ...) jaspScale(as.double(x))
#' @export
vec_cast.jaspScale.logical <- function(x, to, ...) jaspScale(as.double(x))
#' @export
vec_cast.jaspScale.factor <- function(x, to, ...) x |> as.character() |> as.double() |> jaspScale()
#' @export
vec_cast.jaspScale.ordered <- function(x, to, ...) x |> as.character() |> as.double() |> jaspScale()

### to R types----
#' @export
vec_cast.double.jaspScale <- function(x, to, ...) vctrs::vec_data(x) |> as.double()
#' @export
vec_cast.integer.jaspScale <- function(x, to, ...) vctrs::vec_data(x) |> as.integer()
#' @export
vec_cast.character.jaspScale <- function(x, to, ...) vctrs::vec_data(x) |> as.character()
#' @export
vec_cast.logical.jaspScale <- function(x, to, ...) vctrs::vec_data(x) |> as.logical()

# jaspOrdinal ----
newJaspOrdinal <- function(x = integer(), values = integer(), labels = character()) {
  if (!rlang::is_integer(x) || !rlang::is_integer(values) || !rlang::is_character(labels)) {
    rlang::abort("`x` and `values` must be integer vectors, `labels` must be a character vector.")
  }

  ord <- ordered(x, levels = values, labels = labels)
  attr(ord, "values") <- values
  class(ord) <- c("jaspOrdinal", "vctrs_vctr", class(ord))
  return(ord)
  #vctrs::new_vctr(ordered(x, levels = values, labels = labels), values = values, class = "jaspOrdinal")
}

#' @rdname column-types
#' @export
jaspOrdinal <- function(x = integer(), values = sort(unique(x)), labels = values) {
  x <- vctrs::vec_cast(x, integer())
  values <- vctrs::vec_cast(values, integer())
  labels <- as.character(labels)

  newJaspOrdinal(x, values, labels)
}

#' @rdname column-types
#' @export
isJaspOrdinal <- function(x) {
  inherits(x, "jaspOrdinal")
}

#' @export
vec_ptype_abbr.jaspOrdinal <- function(x, ...) {
  return("jspOrd")
}

#' @export
format.jaspOrdinal <- function(x, ...) {
  labels <- attr(x, "levels")
  x <- vctrs::vec_data(x)


  valid <- !is.na(x)

  out        <- rep(NA_character_, vctrs::vec_size(x))
  out[valid] <- labels[x[valid]]

  return(out)
}

#' @export
obj_print_footer.jaspOrdinal <- function(x, ...) {
  val <- attr(x, "values")
  lab <- attr(x, "levels")

  out <- paste(sprintf("%s(%i)", lab, val), collapse = " < ")
  cat("Labels(Values):", out)
}

#' @rdname column-types
#' @export
asJaspOrdinal <- function(x, ...) {
  vctrs::vec_cast(x, newJaspOrdinal())
}


## Casting
#' @export
vec_cast.jaspOrdinal.jaspOrdinal <- function(x, to, ...) x
#' @export
vec_cast.jaspOrdinal.double <- function(x, to, ...) jaspOrdinal(x)
#' @export
vec_cast.double.jaspOrdinal <- function(x, to, ...) {
  values <- attr(x, "values")
  x <- vctrs::vec_data(x)
  as.double(values[x])
}
#' @export
vec_cast.jaspOrdinal.integer <- function(x, to, ...) jaspOrdinal(x)
#' @export
vec_cast.integer.jaspOrdinal <- function(x, to, ...) {
  vec_cast.double.jaspOrdinal(x, to, ...) |> as.integer()
}
#' @export
vec_cast.jaspOrdinal.ordered <- function(x, to, ...) jaspOrdinal(x)
#' @export
vec_cast.ordered.jaspOrdinal <- function(x, to, ...) {
  print("hole!")
  attr(x, "values") <- NULL
  class(x) <- c("ordered", "factor")
  return(x)
}

# jaspNominal ----
newJaspNominal <- function(x = integer(), values = integer(), labels = character()) {
  if (!rlang::is_integer(x) || !rlang::is_integer(values) || !rlang::is_character(labels)) {
    rlang::abort("`x` and `values` must be integer vectors, `labels` must be a character vector.")
  }

  if (!all(x %in% values)) {
    rlang::abort("`values` must be a superset of `x`.")
  }

  if (length(values) != length(labels)) {
    rlangs::abort("`values` and `labels` must be of equal length.")
  }

  vctrs::new_vctr(x, values = values, labels = labels, class = "jaspNominal")
}

#' @rdname column-types
#' @export
jaspNominal <- function(x = integer(), values = sort(unique(x)), labels = values) {
  x <- vctrs::vec_cast(x, integer())
  values <- vctrs::vec_cast(values, integer())
  labels <- as.character(labels)

  newJaspNominal(x, values, labels)
}

#' @rdname column-types
#' @export
isJaspNominal <- function(x) {
  inherits(x, "jaspNominal")
}

#' @export
vec_ptype_abbr.jaspNominal <- function(x, ...) {
  return("jspNom")
}

#' @export
format.jaspNominal <- function(x, ...) {
  format.jaspOrdinal(x, ...)
}

#' @export
obj_print_footer.jaspNominal <- function(x, ...) {
  val <- attr(x, "values")
  lab <- attr(x, "labels")

  out <- paste(sprintf("%s(%i)", lab, val), collapse = ", ")
  cat("Labels(Values):", out)
}

#' @rdname column-types
#' @export
asJaspNominal <- function(x, ...) {
  vctrs::vec_cast(x, newJaspOrdinal())
}

## Coercion
#' @export
vec_ptype2.jaspNominal.jaspNominal <- function(x, y, ...) newjaspNominal()
#' @export
vec_ptype2.jaspNominal.double <- function(x, y, ...) numeric()
#' @export
vec_ptype2.double.jaspNominal <- function(x, y, ...) numeric()
#' @export
vec_ptype2.jaspNominal.integer <- function(x, y, ...) numeric()
#' @export
vec_ptype2.integer.jaspNominal <- function(x, y, ...) numeric()

## Casting
#' @export
vec_cast.jaspNominal.jaspNominal <- function(x, to, ...) x
#' @export
vec_cast.jaspNominal.double <- function(x, to, ...) jaspNominal(x, ...)
#' @export
vec_cast.double.jaspNominal <- function(x, to, ...) vctrs::vec_data(x) |> as.double()
#' @export
vec_cast.jaspNominal.integer <- function(x, to, ...) jaspNominal(x, ...)
#' @export
vec_cast.integer.jaspNominal <- function(x, to, ...) vctrs::vec_data(x) |> as.integer()

# S3 conversions ----
#' @export
jasp2R <- function(x) {
  UseMethod("jasp2R")
}

#' @export
jasp2R.default <- function(x) {
  warning("Object is not of JASP type, no conversion done")
  return(x)
}

#' @export
jasp2R.jaspScale <- function(x) {
  as.numeric(x)
}

#' @export
jasp2R.jaspOrdinal <- function(x) {
  values <- attr(x, "values")
  labels <- attr(x, "levels")
  ordered(vctrs::vec_data(x), levels = values, labels = labels)
}

#' @export
jasp2R.jaspNominal <- function(x) {
  values <- attr(x, "values")
  labels <- attr(x, "levels")
  factor(vctrs::vec_data(x), levels = values, labels = labels)
}

r2jasp <- function(x) {
  UseMethod("r2jasp")
}

#' @export
r2jasp.default <- function(x) {
  warning("Object is not of type that can be explicitly converted to a JASP type, try converting your column into `numeric` or `factor`.")
  return(x)
}

#' @export
r2jasp.numeric <- function(x) {
  asJaspScale(x)
}

#' @export
r2jasp.ordered <- function(x) {
  asJaspOrdinal(x)
}

#' @export
r2jasp.factor <- function(x) {
  asJaspNominal(x)
}

#' @export
r2jasp.character <- function(x) {
  asJaspNominal(x)
}
