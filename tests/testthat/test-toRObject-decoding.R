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
