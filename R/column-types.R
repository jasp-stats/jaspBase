#' @name column-types
#' @rdname column-types
#' @importFrom vctrs vec_ptype2 vec_cast vec_ptype_abbr
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

## Coercion
#' @export
vec_ptype2.jaspScale.jaspScale <- function(x, y, ...) newJaspScale()
#' @export
vec_ptype2.jaspScale.double <- function(x, y, ...) numeric()
#' @export
vec_ptype2.double.jaspScale <- function(x, y, ...) numeric()
#' @export
vec_ptype2.jaspScale.integer <- function(x, y, ...) numeric()
#' @export
vec_ptype2.integer.jaspScale <- function(x, y, ...) numeric()

## Casting
#' @export
vec_cast.jaspScale.jaspScale <- function(x, to, ...) x
#' @export
vec_cast.jaspScale.double <- function(x, to, ...) jaspScale(x)
#' @export
vec_cast.double.jaspScale <- function(x, to, ...) vctrs::vec_data(x) |> as.double()
#' @export
vec_cast.jaspScale.integer <- function(x, to, ...) jaspScale(x)
#' @export
vec_cast.integer.jaspScale <- function(x, to, ...) vctrs::vec_data(x) |> as.integer()
