#' @name column-types
#' @rdname column-types
#' @importFrom vctrs vec_ptype2 vec_cast vec_ptype_abbr obj_print_footer
#' @title JASP Column Types
#'
#' @description Columns types in JASP. JASP recognizes 3 main types (Scale, Ordinal, Nominal),
#' with Nominal being further split between basic Nominal and Text.
#' These types roughly correspond to [numeric()], [ordered()], and [factor()].
#' @param x object to be coerced or tested.
#' @param values set of possible values (similar to `levels` in [factor()]).
#' @param labels set of labels of the values (similar to `labels` in [factor()]).
#' @param ... not used.
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

## Casting ----
### to jaspScale ----
#' @export
vec_cast.jaspScale.double <- function(x, to, ...) { jaspScale(x) }
#' @export
vec_cast.jaspScale.integer <- function(x, to, ...) { jaspScale(x) }
#' @export
vec_cast.jaspScale.character <- function(x, to, ...) { jaspScale(as.double(x)) }
#' @export
vec_cast.jaspScale.logical <- function(x, to, ...) { jaspScale(as.double(x)) }
#' @export
vec_cast.jaspScale.factor <- function(x, to, ...) { x |> as.character() |> as.double() |> jaspScale() }
#' @export
vec_cast.jaspScale.ordered <- function(x, to, ...) { x |> as.character() |> as.double() |> jaspScale() }

### to R types----
#' @export
vec_cast.double.jaspScale <- function(x, to, ...) { vctrs::vec_data(x) |> as.double() }
#' @export
vec_cast.integer.jaspScale <- function(x, to, ...) { vctrs::vec_data(x) |> as.integer() }
#' @export
vec_cast.character.jaspScale <- function(x, to, ...) { vctrs::vec_data(x) |> as.character() }
#' @export
vec_cast.logical.jaspScale <- function(x, to, ...) { vctrs::vec_data(x) |> as.logical() }

# jaspOrdinal ----
newJaspOrdinal <- function(x = integer(), values = integer(), labels = character()) {
  if (!rlang::is_integer(x) || !rlang::is_character(labels))
    rlang::abort("`x` must be integer vectors, `labels` must be a character vector.")

  vctrs::new_vctr(x, values = values, labels = labels, class = "jaspOrdinal")
}

#' @rdname column-types
#' @export
jaspOrdinal <- function(x = integer(), values = sort(unique(x)), labels = values) {
  if (!all(na.omit(x) %in% values)) {
    rlang::abort("`values` must be a superset of `x`.")
  }
  if (length(values) != length(labels)) {
    rlang::abort("`values` and `labels` must be of same length.")
  }

  x <- vctrs::vec_cast(x, integer())
  idx <- match(x, values)
  values <- as.character(values)
  labels <- as.character(labels)

  vctr   <- newJaspOrdinal(x = idx, values = values, labels = labels)

  return(vctr)
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
  lab <- attr(x, "labels")
  x <- vctrs::vec_data(x)

  valid <- !is.na(x)

  out        <- rep(NA_character_, vctrs::vec_size(x))
  out[valid] <- lab[x[valid]]

  return(out)
}

#' @export
obj_print_footer.jaspOrdinal <- function(x, ...) {
  val <- attr(x, "values")
  lab <- attr(x, "labels")

  out <- paste(sprintf("%s(%s)", lab, val), collapse = " < ")
  cat("Labels(Values):", out)
}

#' @rdname column-types
#' @export
asJaspOrdinal <- function(x, ...) {
  if(is.ordered(x)) {
    vec_cast.jaspOrdinal.ordered(x, ...)
  } else {
    vctrs::vec_cast(x, newJaspOrdinal())
  }
}


## Casting ----
### to jaspOrdinal ----
#' @export
vec_cast.jaspOrdinal.double <- function(x, to, ...) { jaspOrdinal(x) }
#' @export
vec_cast.jaspOrdinal.integer <- function(x, to, ...) { jaspOrdinal(x) }
#' @export
vec_cast.jaspOrdinal.character <- function(x, to, ...) {
  xx <- as.integer(x)
  jaspOrdinal(xx)
}
#' @export
vec_cast.jaspOrdinal.factor <- function(x, to, ...) {
  xx <- as.integer(x)
  labels <- levels(x)
  jaspOrdinal(xx, labels = labels)
}
#' @export
vec_cast.jaspOrdinal.ordered <- vec_cast.jaspOrdinal.factor

### to R types ----
#' @export
vec_cast.double.jaspOrdinal <- function(x, to, ...) {
  values <- attr(x, "values")
  x <- vctrs::vec_data(x)
  as.double(values[x])
}
#' @export
vec_cast.integer.jaspOrdinal <- function(x, to, ...) {
  vec_cast.double.jaspOrdinal(x, to, ...) |> as.integer()
}
#' @export
vec_cast.character.jaspOrdinal <- function(x, to, ...) {
  data <- vctrs::vec_data(x)
  labels <- attr(x, "labels")
  return(labels[data])
}

# jaspNominal(Text) ----
newJaspNominal <- function(x = integer(), values = integer(), labels = character(), class = character()) {
  if (!rlang::is_integer(x) || !rlang::is_character(labels))
    rlang::abort("`x` must be integer vectors, `labels` must be a character vector.")

  vctrs::new_vctr(x, values = values, labels = labels, class = c(class, "jaspNominal"))
}

newJaspText <- function(x = integer(), values = character(), labels = character()) {
  newJaspNominal(x = x, values = values, labels = labels, class = "jaspText")
}

#' @rdname column-types
#' @export
jaspNominal <- function(x = integer(), values = sort(unique(x)), labels = values) {
  if (!all(na.omit(x) %in% values)) {
    rlang::abort("`values` must be a superset of `x`.")
  }
  if (length(values) != length(labels)) {
    rlang::abort("`values` and `labels` must be of same length.")
  }

  cast_integers <- try(vctrs::vec_cast(x, integer()), silent = TRUE)
  idx <- match(x, values)
  values <- as.character(values)
  labels <- as.character(labels)

  if (isTryError(cast_integers)) {
    labels <- as.character(labels)
    vctr   <- newJaspText(x = idx, values = values, labels = labels)
  } else {
    labels <- as.character(labels)
    vctr   <- newJaspNominal(x = idx, values = values, labels = labels)
  }

  return(vctr)
}

#' @rdname column-types
#' @export
isJaspNominal <- function(x) {
  inherits(x, "jaspNominal")
}

#' @rdname column-types
#' @export
isJaspText <- function(x) {
  inherits(x, "jaspText")
}

#' @export
vec_ptype_abbr.jaspNominal <- function(x, ...) {
  return("jspNom")
}

#' @export
vec_ptype_abbr.jaspText <- function(x, ...) {
  return("jspTxt")
}

#' @export
format.jaspNominal <- function(x, ...) {
  lab <- attr(x, "labels")
  x <- vctrs::vec_data(x)

  valid <- !is.na(x)

  out        <- rep(NA_character_, vctrs::vec_size(x))
  out[valid] <- lab[x[valid]]

  return(out)
}

#' @export
obj_print_footer.jaspNominal <- function(x, ...) {
  val <- attr(x, "values")
  lab <- attr(x, "labels")

  out <- paste(sprintf("%s(%s)", lab, val), collapse = ", ")
  cat("Labels(Values):", out)
}

#' @rdname column-types
#' @export
asJaspNominal <- function(x, ...) {
  vctrs::vec_cast(x, newJaspNominal())
}


## Casting ----
### to jaspNominal ----
#' @export
vec_cast.jaspNominal.double <- function(x, to, ...) { jaspNominal(x) }
#' @export
vec_cast.jaspNominal.integer <- function(x, to, ...) { jaspNominal(x) }
#' @export
vec_cast.jaspNominal.character <- function(x, to, ...) { jaspNominal(x) }
#' @export
vec_cast.jaspNominal.logical <- function(x, to, ...) { jaspNominal(x) }
#' @export
vec_cast.jaspNominal.factor <- function(x, to, ...) {
  xx <- as.integer(x)
  labels <- levels(x)
  jaspNominal(xx, values = seq_along(labels), labels = labels)
}
#' @export
vec_cast.jaspNominal.ordered <- vec_cast.jaspNominal.factor

### to R types ----
#' @export
vec_cast.double.jaspNominal <- function(x, to, ...) {
  data <- vctrs::vec_data(x)
  values <- attr(x, "values")
  values[data] |> as.double()
}
#' @export
vec_cast.integer.jaspNominal <- function(x, to, ...) {
  vec_cast.double.jaspNominal(x, to, ...) |> as.integer()
}
#' @export
vec_cast.character.jaspNominal <- function(x, to, ...) {
  data <- vctrs::vec_data(x)
  labels <- attr(x, "labels")
  return(labels[data])
}
#' @export
vec_cast.character.jaspText <- vec_cast.character.jaspNominal

# S3 conversions ----
#' @rdname column-types
#' @export
jasp2r <- function(x) {
  UseMethod("jasp2r")
}

#' @export
jasp2r.default <- function(x) {
  warning("Object is not of JASP type, no conversion done")
  return(x)
}

#' @export
jasp2r.jaspScale <- function(x) {
  as.numeric(x)
}

#' @export
jasp2r.jaspOrdinal <- function(x) {
  values <- attr(x, "values")
  labels <- attr(x, "levels")
  ordered(vctrs::vec_data(x), levels = values, labels = labels)
}

#' @export
jasp2r.jaspNominal <- function(x) {
  values <- attr(x, "values")
  labels <- attr(x, "levels")
  factor(vctrs::vec_data(x), levels = values, labels = labels)
}

#' @rdname column-types
#' @export
r2jasp <- function(x) {
  UseMethod("r2jasp")
}

#' @export
r2jasp.default <- function(x) {
  warning("Object is not of type that can be automatically converted to a JASP type.")
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

#' @export
r2jasp.logical <- function(x) {
  asJaspNominal(x)
}

# Casting between JASP types ----

#' @export
vec_cast.jaspScale.jaspScale <- function(x, to, ...) { x }
#' @export
vec_cast.jaspScale.jaspOrdinal <- function(x, to, ...) { x |> as.double() |> asJaspScale() }
#' @export
vec_cast.jaspScale.jaspNominal <- function(x, to, ...) { x |> as.double() |> asJaspScale() }
#' @export
vec_cast.jaspScale.jaspText <- function(x, to, ...) { x |> as.character() |> asJaspScale() }

#' @export
vec_cast.jaspOrdinal.jaspScale <- function(x, to, ...) { x |> as.double() |> asJaspOrdinal() }
#' @export
vec_cast.jaspOrdinal.jaspOrdinal <- function(x, to, ...) { x }
#' @export
vec_cast.jaspOrdinal.jaspNominal <- function(x, to, ...) {
  class(x) <- c("jaspOrdinal", "vctrs_vctr")
  x
}

#' @export
vec_cast.jaspNominal.jaspScale <- function(x, to, ...) { x |> as.double() |> asJaspNominal() }
#' @export
vec_cast.jaspNominal.jaspOrdinal <- function(x, to, ...) {
  class(x) <- c("jaspNominal", "vctrs_vctr")
  x
}
#' @export
vec_cast.jaspNominal.jaspNominal <- function(x, to, ...) { x }
#' @export
vec_cast.jaspNominal.jaspText <- function(x, to, ...) { x }
