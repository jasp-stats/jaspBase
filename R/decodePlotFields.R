# The fields of a plot object with encoded column names, decoded: what toRObject() gives in R (.decodedPlotObject).
# decodeplot() only makes a plot *draw* with the plain names (with ggplot2 >= 4 the guide titles are decoded while it is
# drawn); this also decodes what the object holds, so that e.g. p$labels$x or the name of a scale reads "contNormal".
#
# - the column names of the data of the plot and of its layers, with the mappings and facets that refer to them renamed
#   the same way. A column is only renamed when its decoded name is free in all the data of the plot, so the plot still
#   finds every column, and when it is not a name rlang or ggplot2 reads in a mapping (.data, .env, ..count..);
# - the text and factor values of that data (e.g. variable names that are the categories of an axis);
# - the stored labels, the text parameters of the layers (e.g. annotate(label = ...)) and the name, labels, limits and
#   breaks of the scales.
#
# Layers, facets and scales are ggproto objects that the plot shares with any copy of it: they are cloned before they
# are changed, so the plot JASP keeps stays as it is. Code in the plot that refers to a column cannot be rewritten: no
# column is renamed in a plot with a layer whose data is a function (it could silently select nothing), and a decoded
# plot that does not build any more is given back with its fields as they were. decodeplot() still makes it draw with the
# plain names. Only used outside JASP (see .decodedPlotObject).

.decodePlotFields <- function(plot) {
  if (inherits(plot, "jaspGraphsPlot")) {
    plot$subplots <- lapply(plot$subplots, .decodePlotFields)
    return(plot)
  }

  if (!ggplot2::is_ggplot(plot))
    return(plot)

  if (inherits(plot, "patchwork"))
    plot$patches$plots <- lapply(plot$patches$plots, .decodePlotFields)

  decoded <- .decodeGgplotFields(plot)
  if (.plotBuilds(decoded)) decoded else plot
}

.plotBuilds <- function(plot) {
  tryCatch({
    suppressMessages(suppressWarnings(ggplot2::ggplot_build(plot)))
    TRUE
  }, error = function(e) FALSE)
}

.decodeGgplotFields <- function(plot) {
  renames <- .plotColumnRenames(plot)

  plot$data    <- .decodePlotData(plot$data, renames)
  plot$mapping <- .renamePlotMapping(plot$mapping, renames)

  plot$layers <- lapply(plot$layers, function(layer) {
    clone            <- ggplot2::ggproto(NULL, layer) # not `layer <- ...`: the clone would inherit from itself
    clone$data       <- .decodePlotData(layer$data, renames)
    clone$mapping    <- .renamePlotMapping(layer$mapping, renames)
    clone$aes_params <- .decodeTextIn(layer$aes_params)
    clone
  })

  facet        <- .copyGgproto(plot$facet)
  facetParams  <- facet$params
  for (field in intersect(c("facets", "rows", "cols"), names(facetParams)))
    facetParams[[field]] <- .renamePlotMapping(facetParams[[field]], renames)
  facet$params <- facetParams
  plot$facet   <- facet

  # element by element: with ggplot2 >= 4 the labels are a <ggplot2::labels> object, which a plain list cannot replace
  labels <- plot$labels
  for (aesthetic in names(labels))
    if (!is.null(labels[[aesthetic]]))
      labels[[aesthetic]] <- .decodeTextIn(labels[[aesthetic]])
  plot$labels <- labels

  scales <- plot$scales$clone()
  for (scale in scales$scales) {
    scale$name <- .decodeTextIn(scale$name)
    # a factor too, e.g. scale_x_discrete(limits = <the factor on the x-axis>): left encoded, none of the decoded data
    # values would fall within the limits
    for (field in c("labels", "limits", "breaks"))
      if (is.character(scale[[field]]) || is.factor(scale[[field]]))
        scale[[field]] <- decodeColNames(scale[[field]])
  }
  plot$scales <- scales

  plot
}

# The encoded column names of the data of the plot and its layers, named by them, that can take their decoded name.
.plotColumnRenames <- function(plot) {
  datas   <- c(list(plot$data), lapply(plot$layers, function(layer) layer$data))
  if (any(vapply(datas, is.function, logical(1L))))
    return(character(0))

  columns <- unique(unlist(lapply(datas, function(data) if (is.data.frame(data)) names(data))))
  if (length(columns) == 0L)
    return(character(0))

  decoded <- decodeColNames(columns)
  encoded <- columns != decoded
  renames <- stats::setNames(decoded[encoded], columns[encoded])

  # a decoded name that another column already has, or that two columns decode to, would make the plot pick the
  # wrong column, and in a mapping rlang reads .data and .env as pronouns and ggplot2 ..name.. as a computed variable:
  # those columns keep their encoded name
  taken    <- columns[!encoded]
  clashes  <- renames %in% taken | duplicated(renames) | duplicated(renames, fromLast = TRUE)
  reserved <- renames %in% c(".data", ".env") | grepl("^\\.\\..+\\.\\.$", renames)
  renames[!clashes & !reserved]
}

.decodePlotData <- function(data, renames) {
  if (!is.data.frame(data))
    return(data)

  for (i in seq_along(data)) {
    column <- data[[i]]
    if (is.factor(column))
      levels(column) <- decodeColNames(levels(column))
    else if (is.character(column))
      column <- decodeColNames(column)
    data[[i]] <- column
  }

  toRename <- names(data) %in% names(renames)
  names(data)[toRename] <- renames[names(data)[toRename]]
  data
}

# A mapping (aes()) or the variables of a facet: lists of quosures that refer to columns by symbol (x = col) or by string
# (.data[["col"]]). Only the exact names of `renames` are changed, as in the data.
.renamePlotMapping <- function(mapping, renames) {
  if (length(renames) == 0L || length(mapping) == 0L)
    return(mapping)

  for (i in seq_along(mapping))
    mapping[[i]] <- .renameInExpression(mapping[[i]], renames)

  mapping
}

.renameInExpression <- function(x, renames) {
  # a quosure is a `~` call with its environment; rlang deprecates `[[` on it, so it is renamed without its class
  if (inherits(x, "quosure")) {
    quosureClass <- class(x)
    x <- .renameInExpression(unclass(x), renames)
    class(x) <- quosureClass
    return(x)
  }

  if (is.name(x)) {
    name <- as.character(x)
    return(if (name %in% names(renames)) as.name(renames[[name]]) else x)
  }

  if (is.character(x) && length(x) == 1L && !is.na(x) && x %in% names(renames))
    return(renames[[x]])

  if (is.call(x))
    for (i in seq_along(x)[-1L])
      if (!is.null(x[[i]]))
        x[[i]] <- .renameInExpression(x[[i]], renames)

  x
}

# The text in a label, a scale name or a list of parameters; functions and other values are kept.
.decodeTextIn <- function(x) {
  if (is.character(x))
    return(decodeColNames(x))

  if (is.list(x) && !is.object(x))
    return(rapply(x, decodeColNames, classes = "character", how = "replace"))

  x
}

# A copy of a ggproto object (an environment) with the same fields, parent and class. A facet cannot be cloned as a child
# (ggproto(NULL, facet)): ggplot2 >= 4 then recurses endlessly when it builds the plot.
.copyGgproto <- function(x) {
  copy <- list2env(as.list.environment(x, all.names = TRUE), parent = parent.env(x))
  attributes(copy) <- attributes(x)
  copy
}
