skip_if(packageVersion("ggplot2") < "4.0.0", "decodeplot.gg decodes guides only with ggplot2 >= 4")

localDecoder <- function(env = parent.frame()) {
  oldDecoder <- get0(".decodeColNamesLax", envir = .GlobalEnv, inherits = FALSE)
  assign(".decodeColNamesLax", function(x) gsub("encoded_hp", "Horse Power", gsub("encoded_cyl", "Cylinders", x)), envir = .GlobalEnv)
  withr::defer({
    if (is.null(oldDecoder))
      rm(".decodeColNamesLax", envir = .GlobalEnv)
    else
      assign(".decodeColNamesLax", oldDecoder, envir = .GlobalEnv)
  }, envir = env)
}

decodedPlot <- function(plot) {
  jaspBase:::decodeplot.gg(plot, returnGrob = FALSE)
}

# The legends of a plot as drawn, by aesthetic: the class of their guide and their title
builtLegends <- function(plot) {
  guides <- ggplot2::ggplot_build(plot)@plot@guides
  stats::setNames(
    Map(function(guide, params) list(class = class(guide)[1], title = params$title), guides$guides, guides$params),
    guides$aesthetics
  )
}

axisTitle <- function(plot, axis = "xlab-b") {
  withr::local_pdf(NULL)
  table <- ggplot2::ggplotGrob(plot)
  grob  <- table$grobs[[which(table$layout$name == axis)]]
  grob$children[[1]]$label
}

testData <- data.frame(
  wt          = mtcars$wt,
  mpg         = mtcars$mpg,
  encoded_hp  = mtcars$hp,
  encoded_cyl = factor(mtcars$cyl)
)

continuousPlot <- ggplot2::ggplot(testData, ggplot2::aes(wt, mpg, colour = encoded_hp)) + ggplot2::geom_point()
discretePlot   <- ggplot2::ggplot(testData, ggplot2::aes(wt, mpg, colour = encoded_cyl)) + ggplot2::geom_point()

test_that("a scale's own guide keeps its title and settings", {
  localDecoder()
  # As in jaspAnova: the column has an internal name and the real name is only the title of the scale's guide
  plot <- ggplot2::ggplot(transform(testData, descriptivePlotSeparateLines = encoded_cyl), ggplot2::aes(wt, mpg, colour = descriptivePlotSeparateLines)) +
    ggplot2::geom_point() +
    ggplot2::scale_colour_manual(values = 1:3, guide = ggplot2::guide_legend(title = "encoded_cyl", nrow = 1))

  decoded <- decodedPlot(plot)
  legend  <- builtLegends(decoded)$colour
  expect_equal(legend$class, "GuideLegend")
  expect_equal(legend$title, "Cylinders")
  expect_equal(decoded@scales$get_scales("colour")$guide$params$nrow, 1)
  # the guide of the original plot is left untouched
  expect_equal(plot@scales$get_scales("colour")$guide$params$title, "encoded_cyl")
})

test_that("a title from an encoded mapping is decoded without any guide", {
  localDecoder()
  legend <- builtLegends(decodedPlot(discretePlot))$colour
  expect_equal(legend$class, "GuideLegend")
  expect_equal(legend$title, "Cylinders")
})

test_that("a plot-level guide wins over the scale's guide and is decoded with its settings", {
  localDecoder()
  plot <- discretePlot +
    ggplot2::scale_colour_manual(values = 1:3, guide = ggplot2::guide_legend(title = "scale title")) +
    ggplot2::guides(colour = ggplot2::guide_legend(title = "encoded_cyl", ncol = 2))

  decoded <- decodedPlot(plot)
  expect_equal(builtLegends(decoded)$colour$title, "Cylinders")
  expect_equal(decoded@guides$guides$colour$params$ncol, 2)
})

test_that("a function title is composed with the decoder", {
  localDecoder()
  plot <- discretePlot + ggplot2::guides(colour = ggplot2::guide_legend(title = function(t) paste(t, "(n)")))
  expect_equal(builtLegends(decodedPlot(plot))$colour$title, "Cylinders (n)")
})

test_that("hidden guides stay hidden", {
  localDecoder()
  expect_length(builtLegends(decodedPlot(discretePlot + ggplot2::guides(colour = "none"))), 0)
  expect_length(builtLegends(decodedPlot(discretePlot + ggplot2::guides(colour = ggplot2::guide_none()))), 0)
  expect_length(builtLegends(decodedPlot(discretePlot + ggplot2::scale_colour_discrete(guide = "none"))), 0)
  expect_length(builtLegends(decodedPlot(continuousPlot + ggplot2::scale_colour_gradient(guide = "none"))), 0)
})

test_that("continuous colour scales keep their colourbar", {
  localDecoder()

  legend <- builtLegends(decodedPlot(continuousPlot))$colour
  expect_equal(legend$class, "GuideColourbar")
  expect_equal(legend$title, "Horse Power")

  legend <- builtLegends(decodedPlot(continuousPlot + ggplot2::scale_colour_gradient()))$colour
  expect_equal(legend$class, "GuideColourbar")
  expect_equal(legend$title, "Horse Power")

  legend <- builtLegends(decodedPlot(continuousPlot + ggplot2::scale_colour_gradient(name = "encoded_hp")))$colour
  expect_equal(legend$class, "GuideColourbar")
  expect_equal(legend$title, "Horse Power")

  legend <- builtLegends(decodedPlot(continuousPlot + ggplot2::scale_colour_binned()))$colour
  expect_equal(legend$class, "GuideColoursteps")
  expect_equal(legend$title, "Horse Power")
})

test_that("labels are decoded and removed titles stay removed", {
  localDecoder()
  expect_equal(builtLegends(decodedPlot(continuousPlot + ggplot2::labs(colour = "encoded_hp per car")))$colour$title, "Horse Power per car")
  expect_null(builtLegends(decodedPlot(continuousPlot + ggplot2::labs(colour = NULL)))$colour$title)
})

test_that("axis titles are decoded, also from a scale's own axis guide", {
  localDecoder()
  plot <- ggplot2::ggplot(testData, ggplot2::aes(encoded_hp, mpg)) + ggplot2::geom_point()
  expect_equal(axisTitle(decodedPlot(plot)), "Horse Power")

  plot <- plot + ggplot2::scale_y_continuous(guide = ggplot2::guide_axis(title = "encoded_cyl"))
  expect_equal(axisTitle(decodedPlot(plot), "ylab-l"), "Cylinders")
})

test_that("decoding adds no labels for aesthetics the plot does not use", {
  localDecoder()
  withr::local_pdf(NULL)
  expect_no_message(decodedPlot(continuousPlot))
  expect_no_warning(ggplot2::ggplotGrob(decodedPlot(continuousPlot)))
})
