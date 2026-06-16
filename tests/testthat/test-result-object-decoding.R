localDecodeContext <- function() {
  testthat::skip_if_not_installed("jaspSyntax")
  localTestColumnDecoder()
  columnEncoderContext <- testColumnEncoderContext()
  jaspBase:::.jaspDecodeContext(
    columnEncoderContext = columnEncoderContext,
    factors = list(
      JaspColumn_1_Encoded = c("1" = "control", "2" = "treatment")
    )
  )
}

testColumnEncoderContext <- function() {
  structure(
    list(
      version = 1L,
      columns = list(
        list(name = "cluster", type = "unknown"),
        list(name = "group", type = "unknown"),
        list(name = "score", type = "unknown")
      ),
      extra = list()
    ),
    class = "jaspSyntaxColumnEncoderContext"
  )
}

localTestColumnDecoder <- local({
  function() {
    restore <- localNamespaceBinding(
      "decodeColumnText",
      function(text, encoderContext = NULL) {
        if (is.null(encoderContext))
          return(text)

        out <- text
        out <- gsub("JaspColumn_0_Encoded", "cluster", out, fixed = TRUE)
        out <- gsub("JaspColumn_1_Encoded", "group", out, fixed = TRUE)
        out <- gsub("JaspColumn_2_Encoded", "score", out, fixed = TRUE)
        out
      },
      asNamespace("jaspSyntax")
    )
    withr::defer(restore(), testthat::teardown_env())
    invisible(NULL)
  }
})

localNamespaceBinding <- function(name, value, namespace) {
  oldValue <- get(name, envir = namespace, inherits = FALSE)
  wasLocked <- bindingIsLocked(name, namespace)

  if (wasLocked)
    unlockBinding(name, namespace)
  assign(name, value, envir = namespace)
  if (wasLocked)
    lockBinding(name, namespace)

  function() {
    if (bindingIsLocked(name, namespace))
      unlockBinding(name, namespace)
    assign(name, oldValue, envir = namespace)
    if (wasLocked)
      lockBinding(name, namespace)
  }
}

localGlobalAbsent <- function(name) {
  hadValue <- exists(name, envir = .GlobalEnv, inherits = FALSE)
  oldValue <- if (hadValue) get(name, envir = .GlobalEnv, inherits = FALSE) else NULL

  if (hadValue)
    rm(list = name, envir = .GlobalEnv)

  function() {
    if (hadValue)
      assign(name, oldValue, envir = .GlobalEnv)
  }
}

testthat::test_that("decodeColNames fails for encoded names when no decoder is installed", {
  restoreStrict <- localGlobalAbsent(".decodeColNamesStrict")
  restoreLax <- localGlobalAbsent(".decodeColNamesLax")
  on.exit(restoreStrict(), add = TRUE)
  on.exit(restoreLax(), add = TRUE)

  testthat::expect_identical(jaspBase::decodeColNames("plain name"), "plain name")
  testthat::expect_error(
    jaspBase::decodeColNames("JaspColumn_1_Encoded"),
    "No JASP column decoder is available",
    fixed = TRUE
  )
})

testthat::test_that("result decoding does not use R mapping replacement when native decoding fails", {
  testthat::skip_if_not_installed("jaspSyntax")
  restoreDecoder <- localNamespaceBinding(
    "decodeColumnText",
    function(text, encoderContext = NULL) {
      stop("native decode failure", call. = FALSE)
    },
    asNamespace("jaspSyntax")
  )
  on.exit(restoreDecoder(), add = TRUE)

  decodeContext <- jaspBase:::.jaspDecodeContext(
    columnEncoderContext = testColumnEncoderContext()
  )
  testthat::expect_error(
    jaspBase:::.decodeJaspText("JaspColumn_1_Encoded", decodeContext = decodeContext),
    "native decode failure",
    fixed = TRUE
  )
})

testthat::test_that("result decoding delegates plain text to the native decoder", {
  testthat::skip_if_not_installed("jaspSyntax")
  seen <- new.env(parent = emptyenv())
  restoreDecoder <- localNamespaceBinding(
    "decodeColumnText",
    function(text, encoderContext = NULL) {
      seen$text <- text
      seen$encoderContext <- encoderContext
      paste0(text, " decoded")
    },
    asNamespace("jaspSyntax")
  )
  on.exit(restoreDecoder(), add = TRUE)

  decodeContext <- jaspBase:::.jaspDecodeContext(
    columnEncoderContext = testColumnEncoderContext()
  )

  testthat::expect_identical(
    jaspBase:::.decodeJaspText("plain name", decodeContext = decodeContext),
    "plain name decoded"
  )
  testthat::expect_identical(seen$text, "plain name")
  testthat::expect_identical(seen$encoderContext, testColumnEncoderContext())
})

testthat::test_that("missing decode context does not borrow live native decoder state", {
  testthat::skip_if_not_installed("jaspSyntax")
  restoreDecoder <- localNamespaceBinding(
    "decodeColumnText",
    function(text, encoderContext = NULL) {
      if (is.null(encoderContext))
        return(rep("wrong dataset name", length(text)))

      text
    },
    asNamespace("jaspSyntax")
  )
  on.exit(restoreDecoder(), add = TRUE)

  state <- list(other = list(label = "JaspColumn_1_Encoded"))

  testthat::expect_warning(
    decoded <- jaspBase:::.decodeJaspResultState(state, decodeContext = jaspBase:::.jaspDecodeContext()),
    "no analysis decode context"
  )
  testthat::expect_identical(decoded$other$label, "JaspColumn_1_Encoded")
})

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

testthat::test_that("plot decoding propagates native decoder failures", {
  testthat::skip_if_not_installed("jaspSyntax")
  restoreDecoder <- localNamespaceBinding(
    "decodeColumnText",
    function(text, encoderContext = NULL) {
      stop("native decode failure", call. = FALSE)
    },
    asNamespace("jaspSyntax")
  )
  on.exit(restoreDecoder(), add = TRUE)

  plot <- ggplot2::ggplot(
    data.frame(x = 1, y = 2),
    ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "JaspColumn_1_Encoded")

  testthat::expect_error(
    jaspBase:::.decodeJaspPlotObject(
      plot,
      returnGrob = FALSE,
      decodeContext = jaspBase:::.jaspDecodeContext(columnEncoderContext = testColumnEncoderContext())
    ),
    "native decode failure",
    fixed = TRUE
  )
})

testthat::test_that("decodeplot.gg decodes plot-owned data, mappings, and metadata", {
  plotData <- data.frame(
    JaspColumn_1_Encoded = factor(c("1", "2")),
    JaspColumn_2_Encoded = c(3, 4),
    check.names = FALSE
  )
  attr(plotData, "pri.vars") <- "JaspColumn_1_Encoded"
  attr(plotData, "x")        <- "JaspColumn_1_Encoded"
  attr(plotData, "dv")       <- "JaspColumn_2_Encoded"

  plot <- ggplot2::ggplot(
    plotData,
    ggplot2::aes(x = JaspColumn_1_Encoded, y = JaspColumn_2_Encoded)
  ) +
    ggplot2::geom_point()

  decoded <- jaspBase:::decodeplot(plot, returnGrob = FALSE, decodeContext = localDecodeContext())

  testthat::expect_named(decoded$data, c("group", "score"))
  testthat::expect_equal(levels(decoded$data$group), c("control", "treatment"))
  testthat::expect_identical(attr(decoded$data, "pri.vars"), "group")
  testthat::expect_identical(attr(decoded$data, "x"), "group")
  testthat::expect_identical(attr(decoded$data, "dv"), "score")
  testthat::expect_identical(as.character(rlang::quo_get_expr(decoded$mapping$x)), "group")
  testthat::expect_identical(as.character(rlang::quo_get_expr(decoded$mapping$y)), "score")
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
    text = "The following variable is 'JaspColumn_0_Encoded'.",
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
  attr(plotWrapper, "title") <- "JaspColumn_0_Encoded plot"

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

testthat::test_that("decoder handles JASP-owned mixed table cells without broad object mutation", {
  table <- data.frame(
    JaspColumn_1_Encoded = jaspBase::createMixedColumn(
      values = list("JaspColumn_2_Encoded", 12L),
      types = c("string", "integer")
    ),
    check.names = FALSE
  )

  decoded <- jaspBase:::.decodeJaspRObject(table, decodeContext = localDecodeContext())
  decodedCells <- vctrs::vec_data(decoded$group)

  testthat::expect_s3_class(decoded$group, "mixed")
  testthat::expect_identical(decodedCells[[1L]][["value"]], "score")
  testthat::expect_identical(decodedCells[[2L]][["value"]], 12L)
})

testthat::test_that("decoder handles legacy scalar mixed table cells", {
  cell <- structure(
    list(value = "JaspColumn_2_Encoded", type = "string", format = NULL),
    class = "mixed"
  )

  decoded <- jaspBase:::.decodeJaspRObject(cell, fieldName = "JaspColumn_1_Encoded", decodeContext = localDecodeContext())

  testthat::expect_s3_class(decoded, "mixed")
  testthat::expect_identical(decoded$value, "score")
  testthat::expect_identical(decoded$type, "string")
})

testthat::test_that("decoder leaves foreign mixed model objects intact", {
  object <- structure(
    list(
      anova_table = data.frame(JaspColumn_1_Encoded = "JaspColumn_2_Encoded", check.names = FALSE),
      full_model = structure(list(JaspColumn_3_Encoded = "JaspColumn_1_Encoded"), class = "opaqueModel")
    ),
    class = "mixed",
    type = "3",
    method = "S"
  )

  decoded <- jaspBase:::.decodeJaspRObject(object, decodeContext = localDecodeContext())

  testthat::expect_identical(decoded, object)
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

testthat::test_that("decoder leaves opaque S4 internals intact", {
  className <- paste0("OpaqueDecodeState", sample.int(.Machine$integer.max, 1L))
  methods::setClass(className, slots = c(label = "character"), where = environment())
  object <- methods::new(className, label = "JaspColumn_1_Encoded")

  decoded <- jaspBase:::.decodeJaspRObject(object, decodeContext = localDecodeContext())

  testthat::expect_s4_class(decoded, className)
  testthat::expect_identical(methods::slot(decoded, "label"), "JaspColumn_1_Encoded")
})

testthat::test_that("decoder leaves opaque classed objects intact", {
  object <- structure(
    list(JaspColumn_1_Encoded = "JaspColumn_2_Encoded"),
    class = "opaqueModelState"
  )

  decoded <- jaspBase:::.decodeJaspRObject(object, decodeContext = localDecodeContext())

  testthat::expect_identical(decoded, object)
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

testthat::test_that("result state decoding eagerly decodes figures and preserves analysis state", {
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
    other = list(model = list(label = "JaspColumn_2_Encoded"))
  )

  decoded <- jaspBase:::.decodeJaspResultState(state, decodeContext = localDecodeContext())

  testthat::expect_equal(unname(decoded$figures[["1.png"]]$obj$labels$x), "group")
  testthat::expect_equal(unname(decoded$figures[["1.png"]]$obj$labels$y), "score")
  testthat::expect_identical(decoded$figures[["2.png"]]$other, "group")
  testthat::expect_identical(decoded$other, state$other)
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

  restored <- readRDS(path)
  testthat::expect_equal(restored$table$group, c("control", "treatment"))
  testthat::expect_equal(unname(restored$plot$plotObject$labels$x), "group")
})

testthat::test_that("missing decode context warns for encoded legacy state", {
  state <- list(other = list(label = "JaspColumn_1_Encoded"))

  testthat::expect_warning(
    decoded <- jaspBase:::.decodeJaspResultState(state, decodeContext = jaspBase:::.jaspDecodeContext()),
    "no analysis decode context"
  )
  testthat::expect_identical(decoded$other$label, "JaspColumn_1_Encoded")
})
