# The analysis only sees encoded column names. toRObject() gives the results of a run in R (an R wrapper) with the plain
# names: tables (cells, titles, row names), container names and titles, plot titles, html text, and the plot objects,
# decoded as JASP decodes a plot before it draws it: a plot recipe is built from its decoded arguments, any other plot only
# gets its titles decoded. The decoder is the one the bridge installs, faked here.
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

test_that("a base R plot stays a plot: decodeplot() would give it back as a grob", {
  localFakeDecoder(function(x) gsub("encoded_cont", "contNormal", x, fixed = TRUE))
  local_mocked_bindings(jaspResultsCalledFromJasp = function() FALSE)

  basePlot <- function() graphics::plot(1:3, xlab = "encoded_cont")
  expect_identical(.decodedPlotObject(basePlot), basePlot)

  grDevices::pdf(NULL)
  grDevices::dev.control("enable")
  basePlot()
  recorded <- grDevices::recordPlot()
  grDevices::dev.off()
  expect_identical(.decodedPlotObject(recorded), recorded)

  figures <- list("resources/_1.png" = list(obj = basePlot, width = 480, height = 320))
  expect_identical(.decodedStateFigures(figures), figures)
})

decodeRecipeNames <- function(x) {
  decodings <- c(encoded_cont = "contNormal", encoded_grp = "facGender", encoded_a = "f", encoded_b = "m")
  for (encoded in names(decodings))
    x <- gsub(encoded, decodings[[encoded]], x, fixed = TRUE)
  x
}

# as e.g. the descriptives plot of jaspTTests: the plot is built from these arguments when it is drawn
encodedRecipe <- function() {
  jaspGraphs::createJaspPlotRecipe(
    fun  = "jaspGraphs::descriptivesPlot",
    args = list(
      x = factor(c("encoded_a", "encoded_b")), y = c(1, 2), ciLower = c(0, 1), ciUpper = c(2, 3),
      xName = "encoded_grp", yName = "encoded_cont", noXLevelNames = FALSE
    )
  )
}

plotDataText <- function(plot) unlist(lapply(plot$data, as.character))

test_that("toRObject gives a plot recipe as the plot built from its decoded arguments", {
  localFakeDecoder(decodeRecipeNames)
  local_mocked_bindings(jaspResultsCalledFromJasp = function() FALSE)

  jaspResults <- jaspResultsR$new(loadJaspResults("toRObjectRecipe"))
  plot        <- createJaspPlot(title = "encoded_cont by encoded_grp")
  jaspResults[["plot_encoded_cont"]] <- plot
  suppressWarnings(plot$plotObject <- encodedRecipe())

  result      <- jaspResults$toRObject()
  plotWrapper <- result[["plot_contNormal"]]
  expect_identical(attr(plotWrapper, "title"), "contNormal by facGender")

  decoded <- plotWrapper$plotObject
  expect_true(ggplot2::is_ggplot(decoded))
  # not only how it draws: its data has the plain names too
  expect_true(all(c("f", "m") %in% plotDataText(decoded)))
  expect_false(any(grepl("encoded_", plotDataText(decoded))))
  text <- drawnText(decoded)
  expect_true(all(c("contNormal", "facGender", "f", "m") %in% text))
  expect_false(any(grepl("encoded_", text)))

  # still a ggplot that can be changed
  expect_true("added" %in% drawnText(decoded + ggplot2::ggtitle("added")))
})

test_that("a plot that is not a recipe keeps its encoded data, but draws with the plain names", {
  localFakeDecoder(decodeRecipeNames)
  local_mocked_bindings(jaspResultsCalledFromJasp = function() FALSE)

  plot    <- ggplot2::ggplot(data.frame(encoded_cont = 1:3), ggplot2::aes(encoded_cont, encoded_cont)) + ggplot2::geom_point()
  decoded <- .decodedPlotObject(plot)

  expect_true(ggplot2::is_ggplot(decoded))
  expect_identical(names(decoded$data), "encoded_cont")
  text <- drawnText(decoded)
  expect_true("contNormal" %in% text)
  expect_false(any(grepl("encoded_", text)))
})

test_that("the plots of the state are decoded outside JASP, and left as they are in JASP", {
  localFakeDecoder(decodeRecipeNames)

  recipe  <- encodedRecipe()
  figures <- list("resources/_1.png" = list(obj = recipe, width = 480, height = 320), "resources/_2.png" = list(width = 1))

  local_mocked_bindings(jaspResultsCalledFromJasp = function() FALSE)
  decoded <- .decodedStateFigures(figures)
  expect_named(decoded, names(figures))
  expect_true(ggplot2::is_ggplot(decoded[[1]]$obj))
  expect_false(any(grepl("encoded_", c(plotDataText(decoded[[1]]$obj), drawnText(decoded[[1]]$obj)))))
  expect_identical(decoded[[1]][c("width", "height")], figures[[1]][c("width", "height")])
  expect_identical(decoded[[2]], figures[[2]])
  expect_identical(.decodedStateFigures(list()), list())

  local_mocked_bindings(jaspResultsCalledFromJasp = function() TRUE)
  expect_identical(.decodedStateFigures(figures), figures)
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

  # the arguments of a plot recipe are decoded that way, in JASP too: a missing group must not become an "NA" group
  args    <- list(data = data.frame(lab = c(NA, "encoded_cont", NA), grp = c("a", NA, "b")))
  decoded <- .decodeJaspPlotRecipeArguments(args, decodeNames = FALSE)

  expect_identical(decoded$data$lab, c(NA, "contNormal", NA))
  expect_identical(decoded$data$grp, c("a", NA, "b"))
})
