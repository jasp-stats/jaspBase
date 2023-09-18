#' @rdname column-types
#' @export
setDataSet <- function(dataset) {
  .internal[["dataset"]] <- r2jasp(dataset)
}

#' @rdname column-types
#' @export
getDataSet <- function() {
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
  } else {
    dataset <- jasp2r(dataset)
  }

  dataset <- .convertColumns(dataset, columns,            jasp2r)
  dataset <- .convertColumns(dataset, columns.as.numeric, as.numeric)
  dataset <- .convertColumns(dataset, columns.as.ordinal, as.ordered)
  dataset <- .convertColumns(dataset, columns.as.factor,  as.factor )

  return(dataset)
}

.convertColumns <- function(dataset, columns, type) {
  for(column in columns) {
    data <- dataset[[column]]
    if(!is.null(data)) {
      dataset[[column]] <- type(data)
    } else {
      warning("Variable ", column, " not found!")
    }
  }
  return(dataset)
}
