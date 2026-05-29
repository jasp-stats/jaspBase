localDecoder <- function(mapping) {
  oldDecoder <- if (exists(".decodeColNamesLax", envir = .GlobalEnv, inherits = FALSE)) {
    get(".decodeColNamesLax", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  hadDecoder <- exists(".decodeColNamesLax", envir = .GlobalEnv, inherits = FALSE)

  assign(
    ".decodeColNamesLax",
    function(x) {
      for (encoded in names(mapping))
        x <- gsub(encoded, unname(mapping[[encoded]]), x, fixed = TRUE)
      x
    },
    envir = .GlobalEnv
  )

  function() {
    if (hadDecoder) {
      assign(".decodeColNamesLax", oldDecoder, envir = .GlobalEnv)
    } else if (exists(".decodeColNamesLax", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".decodeColNamesLax", envir = .GlobalEnv)
    }
  }
}

localDecodeContext <- function() {
  jaspBase:::.jaspDecodeContext(
    columns = c(
      JaspColumn_1_Encoded = "group",
      JaspColumn_2_Encoded = "score",
      JaspColumn_3_Encoded = "cluster"
    ),
    factors = list(
      JaspColumn_1_Encoded = c("1" = "control", "2" = "treatment")
    )
  )
}

testthat::test_that("decodeplot.gg returns decoded labels for R-facing plots", {
  plot <- ggplot2::ggplot(
    data.frame(x = 1, y = 2),
    ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = "JaspColumn_1_Encoded",
      y = "JaspColumn_2_Encoded"
    )

  decoded <- jaspBase:::decodeplot(plot, returnGrob = FALSE, decodeContext = localDecodeContext())

  testthat::expect_equal(unname(decoded$labels$x), "group")
  testthat::expect_equal(unname(decoded$labels$y), "score")
})

testthat::test_that("writeImage uses decoded editable objects for state and interactive conversion", {
  ns <- asNamespace("jaspGraphs")
  original <- get("convertGgplotToPlotly", envir = ns)
  captured <- new.env(parent = emptyenv())
  unlockBinding("convertGgplotToPlotly", ns)
  assign("convertGgplotToPlotly", function(plot, ...) {
    captured$x <- unname(plot$labels$x)
    "{}"
  }, envir = ns)
  lockBinding("convertGgplotToPlotly", ns)
  on.exit({
    unlockBinding("convertGgplotToPlotly", ns)
    assign("convertGgplotToPlotly", original, envir = ns)
    lockBinding("convertGgplotToPlotly", ns)
  }, add = TRUE)

  oldTempFile <- if (exists(".requestTempFileNameNative", envir = .GlobalEnv, inherits = FALSE)) get(".requestTempFileNameNative", envir = .GlobalEnv) else NULL
  oldBackground <- if (exists(".imageBackground", envir = .GlobalEnv, inherits = FALSE)) get(".imageBackground", envir = .GlobalEnv) else NULL
  oldPpi <- if (exists(".ppi", envir = .GlobalEnv, inherits = FALSE)) get(".ppi", envir = .GlobalEnv) else NULL
  hadTempFile <- exists(".requestTempFileNameNative", envir = .GlobalEnv, inherits = FALSE)
  hadBackground <- exists(".imageBackground", envir = .GlobalEnv, inherits = FALSE)
  hadPpi <- exists(".ppi", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (hadTempFile) assign(".requestTempFileNameNative", oldTempFile, envir = .GlobalEnv) else if (exists(".requestTempFileNameNative", envir = .GlobalEnv, inherits = FALSE)) rm(".requestTempFileNameNative", envir = .GlobalEnv)
    if (hadBackground) assign(".imageBackground", oldBackground, envir = .GlobalEnv) else if (exists(".imageBackground", envir = .GlobalEnv, inherits = FALSE)) rm(".imageBackground", envir = .GlobalEnv)
    if (hadPpi) assign(".ppi", oldPpi, envir = .GlobalEnv) else if (exists(".ppi", envir = .GlobalEnv, inherits = FALSE)) rm(".ppi", envir = .GlobalEnv)
  }, add = TRUE)
  assign(".requestTempFileNameNative", function(extension) list(root = tempdir(), relativePath = paste0("decoded-write-image.", extension)), envir = .GlobalEnv)
  assign(".imageBackground", "white", envir = .GlobalEnv)
  assign(".ppi", 300, envir = .GlobalEnv)

  plot <- ggplot2::ggplot(
    data.frame(x = 1, y = 2),
    ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "JaspColumn_1_Encoded")

  image <- jaspBase:::writeImageJaspResults(
    plot,
    location = list(root = tempdir(), relativePath = "decoded-write-image.png"),
    decodeContext = localDecodeContext()
  )

  testthat::expect_equal(unname(image$obj$labels$x), "group")
  testthat::expect_equal(captured$x, "group")
})

testthat::test_that("toRObject result copies decode tables, footnotes, and plots", {
  table <- data.frame(
    JaspColumn_1_Encoded = c("1", "2"),
    label = "JaspColumn_2_Encoded",
    check.names = FALSE
  )
  class(table) <- c("jaspTableWrapper", "jaspWrapper", class(table))
  attr(table, "title") <- "JaspColumn_1_Encoded table"
  attr(table, "footnotes") <- list(list(
    text = "The following variable is 'JaspColumn_3_Encoded'.",
    symbol = "<em>Note.</em>"
  ))

  plot <- ggplot2::ggplot(
    data.frame(x = 1, y = 2),
    ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = "JaspColumn_1_Encoded",
      y = "JaspColumn_2_Encoded"
    )
  plotWrapper <- list(plotObject = plot)
  class(plotWrapper) <- c("jaspPlotWrapper", "jaspWrapper")
  attr(plotWrapper, "title") <- "JaspColumn_3_Encoded plot"

  result <- list(
    JaspColumn_1_Encoded = table,
    Plot = plotWrapper
  )
  class(result) <- c("jaspContainerWrapper", "jaspWrapper")
  attr(result, "title") <- "JaspColumn_1_Encoded results"

  decoded <- jaspBase:::.decodeJaspRObject(result, decodeContext = localDecodeContext())

  testthat::expect_equal(names(decoded), c("group", "Plot"))
  testthat::expect_equal(names(decoded$group), c("group", "label"))
  testthat::expect_equal(decoded$group$group, c("control", "treatment"))
  testthat::expect_equal(decoded$group$label, c("score", "score"))
  testthat::expect_equal(attr(decoded$group, "title"), "group table")
  testthat::expect_equal(
    attr(decoded$group, "footnotes")[[1L]]$text,
    "The following variable is 'cluster'."
  )
  testthat::expect_equal(attr(decoded$Plot, "title"), "cluster plot")
  testthat::expect_equal(unname(decoded$Plot$plotObject$labels$x), "group")
  testthat::expect_equal(unname(decoded$Plot$plotObject$labels$y), "score")
  testthat::expect_equal(attr(decoded, "title"), "group results")
})

testthat::test_that("R6 result wrappers keep their analysis decode context", {
  jaspResults <- jaspBase:::jaspResultsR$new(jaspBase:::create_cpp_jaspResults("Context test", NULL))
  jaspResults$setDecodeContext(localDecodeContext())
  on.exit({
    jaspBase:::destroyAllAllocatedObjects()
    jaspBase:::destroyAllAllocatedRObjects()
  }, add = TRUE)

  table <- jaspBase::createJaspTable(
    title = "JaspColumn_1_Encoded table",
    data = data.frame(JaspColumn_1_Encoded = c("1", "2"), check.names = FALSE)
  )
  jaspResults[["JaspColumn_1_Encoded"]] <- table

  restoreDecoder <- localDecoder(c(JaspColumn_1_Encoded = "wrong dataset name"))
  on.exit(restoreDecoder(), add = TRUE)

  decoded <- jaspResults$toRObject()
  child <- jaspResults[["JaspColumn_1_Encoded"]]
  decodedTable <- decoded[[1L]]

  testthat::expect_equal(names(decoded), "group table")
  testthat::expect_equal(names(decodedTable), "group")
  testthat::expect_equal(decodedTable$group, c("control", "treatment"))
  testthat::expect_equal(attr(decodedTable, "title"), "group table")
  testthat::expect_false(any(grepl("wrong dataset name", capture.output(str(decoded)), fixed = TRUE)))
  testthat::expect_equal(child$getDecodeContext()[["columns"]], localDecodeContext()[["columns"]])
})

testthat::test_that("printing output wrappers shows the R-facing object", {
  richResult <- list(
    toRObject = function() list(
      "ANOVA Summary" = data.frame(
        effect = "angle",
        stat = 21.89,
        check.names = FALSE
      )
    ),
    print = function() stop("native wrapper print should not be used", domain = NA)
  )
  class(richResult) <- c("jaspOutputObjR", "jaspObjR")

  printed <- capture.output(returned <- print(richResult))

  testthat::expect_identical(returned, richResult)
  testthat::expect_true(any(grepl("ANOVA Summary", printed, fixed = TRUE)))
  testthat::expect_true(any(grepl("angle", printed, fixed = TRUE)))
})

testthat::test_that("R-facing wrapper print methods are registered", {
  testthat::expect_false(is.null(getS3method("print", "jaspContainerWrapper", optional = TRUE)))
  testthat::expect_false(is.null(getS3method("print", "jaspTableWrapper", optional = TRUE)))
  testthat::expect_false(is.null(getS3method("print", "jaspPlotWrapper", optional = TRUE)))
})

testthat::test_that("printing result wrappers keeps tables readable and plots compact", {
  table <- data.frame(
    effect = "angle",
    stat = 21.89,
    check.names = FALSE
  )
  class(table) <- c("jaspTableWrapper", "jaspWrapper", class(table))
  attr(table, "title") <- "ANOVA Summary"
  attr(table, "footnotes") <- list(
    list(
      text = "Model terms tested with Satterthwaite method.",
      symbol = "<em>Note.</em>"
    )
  )
  attr(table, "jaspObjectEnvironment") <- new.env(parent = emptyenv())

  plotWrapper <- list(plotObject = NULL)
  class(plotWrapper) <- c("jaspPlotWrapper", "jaspWrapper")
  attr(plotWrapper, "title") <- "Plot"
  attr(plotWrapper, "jaspObjectEnvironment") <- new.env(parent = emptyenv())

  result <- list(
    "ANOVA Summary" = table,
    Plot = plotWrapper
  )
  class(result) <- c("jaspContainerWrapper", "jaspWrapper")
  attr(result, "title") <- "MixedModelsLMM"
  before <- result

  printed <- capture.output(returned <- print(result))

  testthat::expect_identical(returned, result)
  testthat::expect_identical(result, before)
  testthat::expect_true(any(grepl("MixedModelsLMM", printed, fixed = TRUE)))
  testthat::expect_true(any(grepl("ANOVA Summary", printed, fixed = TRUE)))
  testthat::expect_true(any(grepl(
    "<jasp table: use x$`ANOVA Summary` as a data.frame; footnotes are in attr(x$`ANOVA Summary`, \"footnotes\")>",
    printed,
    fixed = TRUE
  )))
  testthat::expect_true(any(grepl("angle", printed, fixed = TRUE)))
  testthat::expect_true(any(grepl("Footnotes:", printed, fixed = TRUE)))
  testthat::expect_true(any(grepl("Note. Model terms tested", printed, fixed = TRUE)))
  testthat::expect_true(any(grepl(
    "<jasp plot: use x$Plot$plotObject to display or modify>",
    printed,
    fixed = TRUE
  )))
  testthat::expect_false(any(printed == "$plotObject"))
  testthat::expect_false(any(grepl("jaspObjectEnvironment", printed, fixed = TRUE)))
})

testthat::test_that("table wrapper printing forwards data-frame options", {
  table <- data.frame(
    effect = "angle",
    stat = 21.89,
    check.names = FALSE
  )
  class(table) <- c("jaspTableWrapper", "jaspWrapper", class(table))
  attr(table, "title") <- "ANOVA Summary"
  attr(table, "footnotes") <- list()

  printed <- capture.output(returned <- print(table, row.names = FALSE))

  testthat::expect_identical(returned, table)
  testthat::expect_true(any(grepl(
    "<jasp table: use x as a data.frame; footnotes are in attr(x, \"footnotes\")>",
    printed,
    fixed = TRUE
  )))
  testthat::expect_true(any(grepl("angle", printed, fixed = TRUE)))
  testthat::expect_false(any(grepl("Footnotes:", printed, fixed = TRUE)))
  testthat::expect_false(any(grepl("^1\\s+angle", printed)))
})

testthat::test_that("plot wrapper printing separates placeholders from rendering", {
  plotWrapper <- list(plotObject = "dummy plot printed")
  class(plotWrapper) <- c("jaspPlotWrapper", "jaspWrapper")
  attr(plotWrapper, "title") <- "Plot"

  suppressed <- capture.output(returned <- print(plotWrapper, display = FALSE))

  testthat::expect_identical(returned, plotWrapper)
  testthat::expect_true(any(grepl(
    "<jasp plot: use x$plotObject to display or modify>",
    suppressed,
    fixed = TRUE
  )))
  testthat::expect_false(any(grepl("dummy plot printed", suppressed, fixed = TRUE)))

  rendered <- capture.output(print(plotWrapper))
  testthat::expect_true(any(grepl("dummy plot printed", rendered, fixed = TRUE)))
})

testthat::test_that("container wrapper printing formats nested paths and protects non-JASP children", {
  plotWrapper <- list(plotObject = NULL)
  class(plotWrapper) <- c("jaspPlotWrapper", "jaspWrapper")
  attr(plotWrapper, "title") <- "Plot"

  section <- list(Plot = plotWrapper)
  class(section) <- c("jaspContainerWrapper", "jaspWrapper")
  attr(section, "title") <- "Section"

  result <- list(
    Section = section,
    Other = "plain child"
  )
  class(result) <- c("jaspContainerWrapper", "jaspWrapper")
  attr(result, "title") <- "MixedModelsLMM"

  printed <- capture.output(returned <- print(result))

  testthat::expect_identical(returned, result)
  testthat::expect_true(any(grepl(
    "<jasp plot: use x$Section$Plot$plotObject to display or modify>",
    printed,
    fixed = TRUE
  )))
  testthat::expect_true(any(grepl("plain child", printed, fixed = TRUE)))
})

testthat::test_that("result state decoding eagerly decodes figures and other state", {
  plot <- ggplot2::ggplot(
    data.frame(x = 1, y = 2),
    ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = "JaspColumn_1_Encoded",
      y = "JaspColumn_2_Encoded"
    )
  state <- list(
    figures = list(
      "1.png" = list(obj = plot),
      "2.png" = list(other = "JaspColumn_1_Encoded")
    ),
    other = list(label = "JaspColumn_2_Encoded")
  )

  decoded <- jaspBase:::.decodeJaspResultState(state, decodeContext = localDecodeContext())

  testthat::expect_equal(unname(decoded$figures[["1.png"]]$obj$labels$x), "group")
  testthat::expect_equal(unname(decoded$figures[["1.png"]]$obj$labels$y), "score")
  testthat::expect_identical(decoded$figures[["2.png"]]$other, "group")
  testthat::expect_identical(decoded$other$label, "score")
})

testthat::test_that("result state decoding is internal", {
  testthat::expect_false("decodeJaspResultState" %in% getNamespaceExports("jaspBase"))
})

testthat::test_that("decoded result objects persist without a live decoder", {
  plot <- ggplot2::ggplot(
    data.frame(x = 1, y = 2),
    ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "JaspColumn_1_Encoded")

  result <- list(
    table = data.frame(JaspColumn_1_Encoded = c("1", "2"), check.names = FALSE),
    plot = list(plotObject = plot)
  )
  class(result$plot) <- c("jaspPlotWrapper", "jaspWrapper")

  decoded <- jaspBase:::.decodeJaspRObject(result, decodeContext = localDecodeContext())
  path <- tempfile(fileext = ".rds")
  saveRDS(decoded, path)

  restoreDecoder <- localDecoder(c(JaspColumn_1_Encoded = "wrong dataset name"))
  on.exit(restoreDecoder(), add = TRUE)

  restored <- readRDS(path)
  testthat::expect_equal(restored$table$group, c("control", "treatment"))
  testthat::expect_equal(unname(restored$plot$plotObject$labels$x), "group")
})

testthat::test_that("missing decode context warns for encoded legacy state", {
  restoreDecoder <- localDecoder(c(JaspColumn_1_Encoded = "wrong dataset name"))
  on.exit(restoreDecoder(), add = TRUE)

  state <- list(other = list(label = "JaspColumn_1_Encoded"))

  testthat::expect_warning(
    decoded <- jaspBase:::.decodeJaspResultState(state, decodeContext = jaspBase:::.jaspDecodeContext()),
    "no analysis decode context"
  )
  testthat::expect_identical(decoded$other$label, "JaspColumn_1_Encoded")
})
