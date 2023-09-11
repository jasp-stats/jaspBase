#' @rdname column-types
#' @export
setDataSet <- function(dataset) {
  .internal[["dataset"]] <- as.data.frame(lapply(dataset, r2jasp))
}

#' @rdname column-types
#' @export
getDataSet <- function(dataset) {
  return(.internal[["dataset"]])
}

#' @rdname column-types
#' @export
dataSetColumnSpecification <- function() {
  dataset <- .readDataSetHeaderFromR(all.columns = TRUE)
  if (is.null(dataset)) stop("No dataset set!")

  specification <- list()
  for(i in seq_len(ncol(dataset))) {
    cl <- class(dataset[[i]])[1]
    specification[[i]] <- list(
      name = colnames(dataset)[[i]],
      type = cl,
      values = if(cl == "jaspScale") NA else attr(dataset[[i]], "values"),
      labels = if(cl == "jaspScale") NA else attr(dataset[[i]], "labels")
    )
  }
  return(specification)
}

.readDataSetToEndFromR <- function(columns=NULL, columns.as.numeric=NULL, columns.as.ordinal=NULL, columns.as.factor=NULL, all.columns=FALSE, ...) {
  dataset <- .internal[["dataset"]]
  dataset <- .dataSetSubsetColumns(dataset, columns=columns, columns.as.numeric=columns.as.numeric, columns.as.ordinal=columns.as.ordinal, columns.as.factor=columns.as.factor, all.columns=all.columns, ...)
  return(dataset)
}

.readDataSetHeaderFromR <- function(columns=NULL, columns.as.numeric=NULL, columns.as.ordinal=NULL, columns.as.factor=NULL, all.columns=FALSE, ...) {
  dataset <- .readDataSetToEndFromR(columns=columns, columns.as.numeric=columns.as.numeric, columns.as.ordinal=columns.as.ordinal, columns.as.factor=columns.as.factor, all.columns=all.columns, ...)
  dataset <- dataset[NULL, ]
  return(dataset)
}

.dataSetSubsetColumns <- function(dataset, columns=NULL, columns.as.numeric=NULL, columns.as.ordinal=NULL, columns.as.factor=NULL, all.columns=FALSE, ...) {
  if(!all.columns) {
    dataset <- dataset[, unique(c(columns, columns.as.numeric, columns.as.ordinal, columns.as.factor)), drop = FALSE]
  }

  dataset <- .recodeColumns(dataset, columns.as.numeric, as.numeric)
  dataset <- .recodeColumns(dataset, columns.as.ordinal, as.ordered)
  dataset <- .recodeColumns(dataset, columns.as.factor,  as.factor )

  return(dataset)
}

.recodeColumns <- function(dataset, which, type) {
  if(!is.null(which)) {
    dataset[, which] <- .coerceColumnType(dataset[, which, drop = FALSE], type)
  }
  return(dataset)
}
.coerceColumnType <- function(columns, type) {
  as.data.frame(
    lapply(columns, function(col) type(col))
  )
}
