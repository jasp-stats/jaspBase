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

testthat::test_that("decodeplot.gg returns decoded labels for R-facing plots", {
  restoreDecoder <- localDecoder(c(
    JaspColumn_1_Encoded = "group",
    JaspColumn_2_Encoded = "score"
  ))
  on.exit(restoreDecoder(), add = TRUE)

  plot <- ggplot2::ggplot(
    data.frame(x = 1, y = 2),
    ggplot2::aes(x = x, y = y)
  ) +
    ggplot2::geom_point() +
    ggplot2::labs(
      x = "JaspColumn_1_Encoded",
      y = "JaspColumn_2_Encoded"
    )

  decoded <- jaspBase:::decodeplot(plot, returnGrob = FALSE)

  testthat::expect_equal(unname(decoded$labels$x), "group")
  testthat::expect_equal(unname(decoded$labels$y), "score")
})

testthat::test_that("toRObject result copies decode tables, footnotes, and plots", {
  restoreDecoder <- localDecoder(c(
    JaspColumn_1_Encoded = "group",
    JaspColumn_2_Encoded = "score",
    JaspColumn_3_Encoded = "cluster"
  ))
  on.exit(restoreDecoder(), add = TRUE)

  table <- data.frame(
    JaspColumn_1_Encoded = "JaspColumn_2_Encoded",
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

  decoded <- jaspBase:::.decodeJaspRObject(result)

  testthat::expect_equal(names(decoded), c("group", "Plot"))
  testthat::expect_equal(names(decoded$group), "group")
  testthat::expect_equal(decoded$group$group, "score")
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

testthat::test_that("decodeJaspResultState decodes stored figure objects", {
  restoreDecoder <- localDecoder(c(
    JaspColumn_1_Encoded = "group",
    JaspColumn_2_Encoded = "score"
  ))
  on.exit(restoreDecoder(), add = TRUE)

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

  decoded <- jaspBase::decodeJaspResultState(state)

  testthat::expect_equal(unname(decoded$figures[["1.png"]]$obj$labels$x), "group")
  testthat::expect_equal(unname(decoded$figures[["1.png"]]$obj$labels$y), "score")
  testthat::expect_identical(decoded$figures[["2.png"]]$other, "JaspColumn_1_Encoded")
  testthat::expect_identical(decoded$other$label, "JaspColumn_2_Encoded")
})
