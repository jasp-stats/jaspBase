# The analysis only sees encoded column names. toRObject() gives the results of a run in R (an R wrapper) with the plain
# names: tables (cells, titles, row names), container names and titles, plot titles, html text, and the plot objects,
# decoded as JASP decodes a plot before it draws it. The decoder is the one the bridge installs, faked here.
localFakeDecoder <- function(decoder, envir = parent.frame()) {
  oldDecoder <- get0(".decodeColNamesLax", envir = .GlobalEnv, inherits = FALSE)
  assign(".decodeColNamesLax", decoder, envir = .GlobalEnv)
  withr::defer({
    if (is.null(oldDecoder))
      rm(".decodeColNamesLax", envir = .GlobalEnv)
    else
      assign(".decodeColNamesLax", oldDecoder, envir = .GlobalEnv)
  }, envir = envir)
}

resultsWithEncodedNames <- function() {
  jaspResults <- jaspResultsR$new(loadJaspResults("toRObjectDecoding"))

  plots <- createJaspContainer(title = "Plots of encoded_cont")
  jaspResults[["plots"]] <- plots

  plot <- createJaspPlot(title = "encoded_cont")
  plots[["encoded_cont"]] <- plot
  suppressWarnings(plot$plotObject <- ggplot2::ggplot(data.frame(x = 1:3, y = 1:3), ggplot2::aes(x, y)) +
                     ggplot2::geom_point() + ggplot2::labs(x = "encoded_cont"))

  jaspResults[["note"]] <- createJaspHtml(text = "Note on encoded_cont", title = "About encoded_cont")
  jaspResults
}

drawnText <- function(plot) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  unlist(rapply(ggplot2::ggplotGrob(plot), function(x) x, classes = "character", how = "unlist"))
}

test_that("toRObject decodes the names, titles, html and plots of a run in R", {
  localFakeDecoder(function(x) gsub("encoded_cont", "contNormal", x, fixed = TRUE))
  local_mocked_bindings(jaspResultsCalledFromJasp = function() FALSE)

  result <- resultsWithEncodedNames()$toRObject()

  expect_identical(names(result$plots), "contNormal")
  expect_identical(attr(result$plots, "title"), "Plots of contNormal")

  plotWrapper <- result$plots[["contNormal"]]
  expect_identical(attr(plotWrapper, "title"), "contNormal")
  expect_true(ggplot2::is_ggplot(plotWrapper$plotObject))
  expect_identical(plotWrapper$plotObject$labels$x, "contNormal")
  expect_true("contNormal" %in% drawnText(plotWrapper$plotObject))
  expect_false(any(grepl("encoded_cont", drawnText(plotWrapper$plotObject))))

  # still a ggplot that can be changed
  changed <- plotWrapper$plotObject + ggplot2::ggtitle("added")
  expect_true(all(c("added", "contNormal") %in% drawnText(changed)))

  expect_identical(result$note$rawtext, "Note on contNormal")
  expect_identical(attr(result$note, "title"), "About contNormal")
})

test_that("toRObject leaves names and plots as they are without a decoder", {
  oldDecoder <- get0(".decodeColNamesLax", envir = .GlobalEnv, inherits = FALSE)
  if (!is.null(oldDecoder)) {
    rm(".decodeColNamesLax", envir = .GlobalEnv)
    withr::defer(assign(".decodeColNamesLax", oldDecoder, envir = .GlobalEnv))
  }
  local_mocked_bindings(jaspResultsCalledFromJasp = function() FALSE)

  result <- resultsWithEncodedNames()$toRObject()

  expect_identical(names(result$plots), "encoded_cont")
  expect_true("encoded_cont" %in% drawnText(result$plots[[1]]$plotObject))
})

test_that("the plot object is decoded only outside JASP, and kept when it cannot be decoded", {
  plot <- ggplot2::ggplot(data.frame(x = 1), ggplot2::aes(x, x)) + ggplot2::labs(x = "encoded_cont")

  local_mocked_bindings(jaspResultsCalledFromJasp = function() TRUE)
  expect_identical(.decodedPlotObject(plot), plot)

  local_mocked_bindings(jaspResultsCalledFromJasp = function() FALSE)
  notAPlot <- structure(list(1), class = "somethingDecodeplotDoesNotKnow")
  expect_identical(.decodedPlotObject(notAPlot), notAPlot)
  expect_null(.decodedPlotObject(NULL))
})

quosureText <- function(quosure) deparse(unclass(quosure)[[2L]])

test_that("the fields of a plot are decoded: data, mappings, layers, facets, labels and scales", {
  localFakeDecoder(function(x) gsub("encoded_grp", "my group", gsub("encoded_cont", "contNormal", x, fixed = TRUE), fixed = TRUE))

  data <- data.frame(encoded_cont = 1:4, y = 1:4, encoded_grp = c("a", "b", "a", "b"), lab = c("encoded_cont", "x", "y", "z"))
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = encoded_cont, y = .data[["y"]], colour = .data[["encoded_grp"]])) +
    ggplot2::geom_point() +
    ggplot2::geom_text(ggplot2::aes(label = lab)) +
    ggplot2::annotate("text", x = 2, y = 2, label = "about encoded_cont") +
    ggplot2::facet_wrap(~encoded_grp) +
    ggplot2::scale_y_continuous(name = "y of encoded_cont") +
    ggplot2::labs(title = "Title encoded_cont")

  decoded <- .decodePlotFields(plot)

  expect_identical(names(decoded$data), c("contNormal", "y", "my group", "lab"))
  expect_identical(decoded$data$lab, c("contNormal", "x", "y", "z"))
  expect_identical(quosureText(decoded$mapping$x), "contNormal")
  expect_identical(quosureText(decoded$mapping$colour), '.data[["my group"]]')
  expect_identical(decoded$layers[[3]]$aes_params$label, "about contNormal")
  expect_identical(quosureText(decoded$facet$params$facets[[1]]), "my group")
  expect_identical(decoded$labels$title, "Title contNormal")
  expect_identical(decoded$scales$get_scales("y")$name, "y of contNormal")

  # it still finds its columns, and draws with the plain names
  text <- drawnText(decoded)
  expect_true(all(c("contNormal", "my group", "Title contNormal", "y of contNormal") %in% text))
  expect_false(any(grepl("encoded_", text)))

  # the plot it was decoded from is not changed: layers, facets and scales are shared ggproto objects
  expect_identical(names(plot$data)[1], "encoded_cont")
  expect_identical(quosureText(plot$facet$params$facets[[1]]), "encoded_grp")
  expect_identical(plot$scales$get_scales("y")$name, "y of encoded_cont")
  expect_identical(plot$layers[[3]]$aes_params$label, "about encoded_cont")
})

test_that("a column keeps its encoded name when its decoded name is taken", {
  localFakeDecoder(function(x) gsub("encoded_cont", "contNormal", x, fixed = TRUE))

  data <- data.frame(encoded_cont = 1:3, contNormal = 3:1)
  plot <- ggplot2::ggplot(data, ggplot2::aes(encoded_cont, contNormal)) + ggplot2::geom_point()

  decoded <- .decodePlotFields(plot)

  expect_identical(names(decoded$data), c("encoded_cont", "contNormal"))
  expect_identical(quosureText(decoded$mapping$x), "encoded_cont")
  expect_silent(ggplot2::ggplot_build(decoded))
})

test_that("the plots of the state are decoded outside JASP, and left as they are in JASP", {
  localFakeDecoder(function(x) gsub("encoded_cont", "contNormal", x, fixed = TRUE))

  plot    <- ggplot2::ggplot(data.frame(encoded_cont = 1:3), ggplot2::aes(encoded_cont, encoded_cont)) + ggplot2::geom_point()
  figures <- list("resources/_1.png" = list(obj = plot, width = 480, height = 320), "resources/_2.png" = list(width = 1))

  local_mocked_bindings(jaspResultsCalledFromJasp = function() FALSE)
  decoded <- .decodedStateFigures(figures)
  expect_named(decoded, names(figures))
  expect_identical(names(decoded[[1]]$obj$data), "contNormal")
  expect_identical(decoded[[1]][c("width", "height")], figures[[1]][c("width", "height")])
  expect_identical(decoded[[2]], figures[[2]])
  expect_identical(.decodedStateFigures(list()), list())

  local_mocked_bindings(jaspResultsCalledFromJasp = function() TRUE)
  expect_identical(.decodedStateFigures(figures), figures)
})

test_that("a column does not get a name that rlang or ggplot2 reads in a mapping", {
  for (reserved in c(".data", ".env", "..count..")) {
    localFakeDecoder(local({ name <- reserved; function(x) gsub("encoded_cont", name, x, fixed = TRUE) }))

    plot    <- ggplot2::ggplot(data.frame(encoded_cont = c("a", "b"), v = 1:2), ggplot2::aes(encoded_cont, v)) + ggplot2::geom_point()
    decoded <- .decodePlotFields(plot)

    expect_identical(names(decoded$data), c("encoded_cont", "v"), info = reserved)
    expect_silent(ggplot2::ggplot_build(decoded))
  }
})

test_that("no column is renamed in a plot with a layer whose data is a function, which still draws with the plain names", {
  localFakeDecoder(function(x) gsub("encoded_cont", "contNormal", x, fixed = TRUE))
  local_mocked_bindings(jaspResultsCalledFromJasp = function() FALSE)

  # the function refers to the column by its encoded name: renamed, it would silently select nothing
  plot <- ggplot2::ggplot(data.frame(encoded_cont = 1:3, v = 1:3), ggplot2::aes(encoded_cont, v)) +
    ggplot2::geom_point(data = function(d) d[d$encoded_cont > 1, ])

  decoded <- .decodedPlotObject(plot)

  expect_identical(names(decoded$data), c("encoded_cont", "v"))
  expect_identical(nrow(ggplot2::layer_data(decoded)), 2L)
  text <- drawnText(decoded)
  expect_true("contNormal" %in% text)
  expect_false(any(grepl("encoded_cont", text)))
})

test_that("a decoded plot that does not build is given back with its fields as they were", {
  localFakeDecoder(function(x) gsub("encoded_cont", "contNormal", x, fixed = TRUE))
  local_mocked_bindings(.decodeGgplotFields = function(plot) plot + ggplot2::aes(x = no_such_column))

  plot <- ggplot2::ggplot(data.frame(encoded_cont = 1:3), ggplot2::aes(encoded_cont, encoded_cont)) + ggplot2::geom_point()

  expect_identical(.decodePlotFields(plot), plot)
})

test_that("container names that would decode to the same name keep their encoded name", {
  localFakeDecoder(function(x) gsub("encoded_[ab]", "contNormal", gsub("encoded_c", "other", x)))
  local_mocked_bindings(jaspResultsCalledFromJasp = function() FALSE)

  jaspResults <- jaspResultsR$new(loadJaspResults("duplicateNames"))
  container   <- createJaspContainer(title = "Tables")
  jaspResults[["tables"]] <- container
  for (key in c("encoded_a", "encoded_b", "encoded_c"))
    container[[key]] <- createJaspHtml(text = key)

  expect_setequal(names(jaspResults$toRObject()$tables), c("encoded_a", "encoded_b", "other"))
})

test_that("missing values stay missing, although the decoders of the bridge give back \"NA\" for them", {
  # as .decodeColNamesLax of the bridge does: its Rcpp::String argument turns a missing value into "NA"
  localFakeDecoder(function(x) gsub("encoded_cont", "contNormal", if (is.na(x)) "NA" else x, fixed = TRUE))

  expect_identical(decodeColNames(c("encoded_cont", NA)), c("contNormal", NA))

  data    <- data.frame(x = 1:3, y = 1:3, lab = c(NA, "encoded_cont", NA), grp = c("a", NA, "b"))
  plot    <- ggplot2::ggplot(data, ggplot2::aes(x, y, label = lab, colour = grp)) + ggplot2::geom_text()
  decoded <- .decodePlotFields(plot)

  expect_identical(decoded$data$lab, c(NA, "contNormal", NA))
  expect_identical(decoded$data$grp, c("a", NA, "b"))
  # only the one label is drawn, not the text "NA" for the others
  expect_identical(ggplot2::layer_data(decoded)$label, c(NA, "contNormal", NA))
})

test_that("factor limits, breaks and labels of a scale are decoded like the data they refer to", {
  localFakeDecoder(function(x) gsub("encoded_a", "contNormal", gsub("encoded_b", "contGamma", x, fixed = TRUE), fixed = TRUE))

  # e.g. the paired samples bar plot of jaspTTests: scale_x_discrete(limits = <the factor on the x-axis>)
  groups  <- factor(c("encoded_a", "encoded_b"))
  plot    <- ggplot2::ggplot(data.frame(group = groups, y = 1:2), ggplot2::aes(group, y)) +
    ggplot2::geom_point() +
    ggplot2::scale_x_discrete(limits = groups, breaks = groups, labels = groups)
  decoded <- .decodePlotFields(plot)
  scale   <- decoded$scales$get_scales("x")

  expect_identical(levels(decoded$data$group), c("contNormal", "contGamma"))
  for (field in c("limits", "breaks", "labels"))
    expect_identical(as.character(scale[[field]]), c("contNormal", "contGamma"), info = field)
  # every point still falls within the limits
  expect_identical(sum(!is.na(ggplot2::layer_data(decoded)$x)), 2L)
})
