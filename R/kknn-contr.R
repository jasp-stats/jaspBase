# These functions are re-exported from package {kknn} (version 1.3.1 at the time of writing this code).
# Need to be included in jaspBase because of this issue: https://github.com/jasp-stats/jasp-test-release/issues/2000

#' Contrast matrices
#' @md
#' @description These functions are re-exports of [kknn::contr.dummy()], [kknn::contr.ordinal()], and [kknn::contr.metric()].
#' @return A matrix of contrasts.
#' @keywords internal
#' @name contrasts
#' @rdname contrasts
NULL

#' @importFrom kknn contr.dummy
#' @export contr.dummy
#' @name contr.dummy
#' @rdname contrasts
NULL

#' @importFrom kknn contr.ordinal
#' @export contr.ordinal
#' @name contr.ordinal
#' @rdname contrasts
NULL

#' @importFrom kknn contr.metric
#' @export contr.metric
#' @name contr.metric
#' @rdname contrasts
NULL
