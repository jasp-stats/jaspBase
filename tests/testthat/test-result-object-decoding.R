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
