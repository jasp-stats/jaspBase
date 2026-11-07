# Helpers ---------------------------------------------------------------------------------------------------------

mtSeparator <- jaspBase::interactionSymbol

mtData <- function(n = 90, seed = 1) {
  set.seed(seed)
  data.frame(
    y = rnorm(n),
    x = runif(n, 1, 5),
    z = runif(n, 2, 6),
    w = runif(n, -2, 2),
    f = factor(sample(rep_len(c("a", "b", "c"), n)), levels = c("a", "b", "c")),
    g = factor(sample(rep_len(c("low", "mid", "high", "top"), n)), levels = c("low", "mid", "high", "top"))
  )
}

# One definition of a variable (a version of it); the parameters of the specification and `restriction` by name
mtDef <- function(id, variable, specification, ...) {
  definition <- list(id = id, variable = variable, specification = specification, restriction = "")
  extra      <- list(...)
  definition[names(extra)] <- extra
  definition
}

# An advanced model from the definitions it uses and its terms. `terms` has the definition ids of each term (a vector
# per term: one id is a main effect, several are an interaction); by default every definition is a main effect. The terms
# are called t1, t2, ... in this order. `components` has the variables of the terms, as the form gives them (the reader
# does not read it). Further elements of the model, such as `intercept`, are given by name.
mtModel <- function(definitions, terms = NULL, title = "Model 0", name = "model0", ...) {
  ids <- vapply(definitions, `[[`, "", "id")
  if (is.null(terms))
    terms <- as.list(ids)
  variables  <- stats::setNames(lapply(definitions, function(definition) as.character(definition[["variable"]])), ids)   # a constant has none
  components <- unique(lapply(terms, function(operands) unname(unlist(variables[intersect(operands, ids)]))))
  c(list(name = name, title = title, components = components, definitions = definitions,
         terms = lapply(seq_along(terms), function(index) list(id = sprintf("t%d", index), operands = terms[[index]]))),
    list(...))
}

mtBasic <- function(components, title = "Model 0", name = "model0") {
  list(name = name, title = title, components = components)
}

# The design columns of a model with one main effect, as a matrix without names
mtColumns <- function(specification, data = mtData(), variable = "x", ...) {
  prepared <- prepareModelTerms(list(mtModel(list(mtDef("d1", variable, specification, ...)))), data)
  unname(as.matrix(mtDesign(prepared, 1)))
}

# The design columns of a model, by label
mtByLabel <- function(prepared, model = 1, data = NULL) {
  design <- mtDesign(prepared, model, data = data)
  stats::setNames(lapply(seq_along(design), function(index) design[[index]]), attr(design, "labels"))
}

# The design without the (Intercept) column unless asked for: most tests look at the columns of the terms
mtDesign <- function(prepared, model = 1, data = NULL, intercept = FALSE) {
  modelTermsDesign(prepared, model, data = data, intercept = intercept)
}

mtMatrix <- function(design) unname(as.matrix(design))

mtError <- function(expression) tryCatch({ expression; NULL }, error = function(e) conditionMessage(e))

# lm() on the formula and a fit on the design columns (with the offset of the model, if it has one) are the same model;
# lm() takes the constant offset of the model (0 without) as an argument, because the formula cannot hold it
mtExpectSameModel <- function(prepared, model, data) {
  fit    <- stats::lm(modelTermsFormula(prepared, model, response = "y"), data,
                      offset = rep(modelTermsConstantOffset(prepared, model), nrow(data)))
  design <- mtDesign(prepared, model, intercept = TRUE)
  expect_equal(mtMatrix(design), unname(stats::model.matrix(fit)), ignore_attr = TRUE)
  designFit <- stats::lm.fit(as.matrix(design), data$y, offset = modelTermsOffset(prepared, model))
  expect_equal(unname(designFit$coefficients), unname(stats::coef(fit)))
  expect_equal(unname(designFit$fitted.values), unname(stats::fitted(fit)))
  invisible(fit)
}

# A model with the main effects of x, log(x), logBase(z) and a factor f, and the interactions x:logBase(z) and
# log(x):logBase(z); the interactions choose the version of x
mtExampleModel <- function() {
  mtModel(
    definitions = list(
      mtDef("d1", "x", "none"),
      mtDef("d2", "x", "log"),
      mtDef("d3", "z", "logBase", logBase = "10", logMultiplier = "1"),
      mtDef("d4", "f", "treatment", referenceLevel = "b")
    ),
    terms = list("d1", "d2", "d3", "d4", c("d1", "d3"), c("d2", "d3"))
  )
}

# Basic mode ------------------------------------------------------------------------------------------------------

test_that("basic mode gives the plain formula in the order of the components", {
  data     <- mtData()
  prepared <- prepareModelTerms(list(
    mtBasic(list("x", "z", c("x", "z"))),
    mtBasic(list(c("x", "z"), "x", c("x", "z", "w"), "z", "w"), title = "Model 1", name = "model1")
  ), data)

  expect_s3_class(prepared, "jaspModelTerms")
  expect_false(prepared$advanced)
  expect_equal(modelTermsFormula(prepared, 1, response = "y"), y ~ x + z + x:z, ignore_formula_env = TRUE)
  expect_equal(modelTermsFormula(prepared, "model0"), ~ x + z + x:z, ignore_formula_env = TRUE)
  expect_equal(modelTermsFormula(prepared, 1, response = "y", intercept = FALSE), y ~ 0 + x + z + x:z, ignore_formula_env = TRUE)
  expect_equal(modelTermsFormula(prepared, 2, response = "y"), y ~ x:z + x + x:z:w + z + w, ignore_formula_env = TRUE)
  expect_equal(modelTermsFormula(prepared, 1, response = c("y", "w")), cbind(y, w) ~ x + z + x:z, ignore_formula_env = TRUE)
  expect_identical(environment(modelTermsFormula(prepared, 1)), environment())

  terms <- prepared$models[[1]]$terms
  expect_equal(terms$type, c("main", "main", "interaction"))
  expect_equal(terms$expression, c("x", "z", "x:z"))
  expect_equal(terms$label, c("x", "z", paste("x", "z", sep = mtSeparator)))
  expect_equal(c(prepared$models[[1]]$name, prepared$models[[2]]$title), c("model0", "Model 1"))
  expect_equal(prepared$nested, TRUE)
})

test_that("basic mode accepts arrays as lists and arbitrary column names", {
  names <- c("my var", "JaspColumn_.3._Encoded", "café éè", "a:b", "`q`")
  data  <- stats::setNames(as.data.frame(matrix(stats::rnorm(60), 12, 5)), names)
  components <- list(list(names[1]), list(names[2]), list(names[3], names[4]), names[5])
  prepared   <- prepareModelTerms(list(mtBasic(components)), data)

  formula <- modelTermsFormula(prepared, 1)
  expect_setequal(all.vars(formula), names)
  expect_equal(formula, stats::as.formula(call("~", Reduce(function(l, r) call("+", l, r), list(
    as.name(names[1]), as.name(names[2]), call(":", as.name(names[3]), as.name(names[4])), as.name(names[5])))),
    env = baseenv()), ignore_formula_env = TRUE)
  expect_equal(prepared$models[[1]]$terms$type, c("main", "main", "main", "interaction"))
  expect_equal(prepared$models[[1]]$terms$label[4], paste(names[3], names[4], sep = mtSeparator))
  expect_equal(ncol(mtDesign(prepared, 1)), 4)
})

test_that("basic mode design is the design of lm(), with labels for factors", {
  data     <- mtData()
  prepared <- prepareModelTerms(list(mtBasic(list("x", "f", c("f", "x"), c("f", "g"), "g", c("x", "z"), "z"))), data)
  fit      <- mtExpectSameModel(prepared, 1, data)

  design <- mtDesign(prepared, 1)
  expect_false("(Intercept)" %in% names(design))
  expect_equal(attr(design, "labels")[1:5], c("x", "f (b)", "f (c)", "g (mid)", "g (high)"))
  expect_equal(length(attr(design, "labels")), ncol(design))
  expect_equal(length(attr(design, "term")), ncol(design))
  expect_true(all(is.na(attr(design, "fixed"))))
  expect_equal(names(attr(design, "fixed")), names(design))

  labels <- modelTermsLabels(prepared, 1, names(stats::coef(fit)))
  expect_equal(labels[1:6], c("(Intercept)", "x", "f (b)", "f (c)", "g (mid)", "g (high)"))
  expect_true(paste("x", "f (b)", sep = mtSeparator) %in% labels)
  expect_true(paste("f (c)", "g (top)", sep = mtSeparator) %in% labels)
  expect_equal(length(unique(labels)), length(labels))

  # the term of every column is the term of lm()
  assigned <- attr(stats::model.matrix(fit), "assign")
  expect_equal(attr(mtDesign(prepared, 1), "term"), prepared$models[[1]]$terms$id[assigned[assigned > 0]])
})

test_that("basic mode follows R for interactions without their margins", {
  data     <- mtData()
  prepared <- prepareModelTerms(list(mtBasic(list("z", c("f", "x"), c("g", "z")))), data)
  fit      <- mtExpectSameModel(prepared, 1, data)

  # f:x has neither f nor x, so f has an indicator per level; g:z has the margin z and uses contrasts
  expect_equal(modelTermsLabels(prepared, 1, names(stats::coef(fit))), c(
    "(Intercept)", "z",
    paste("f (a)", "x", sep = mtSeparator), paste("f (b)", "x", sep = mtSeparator), paste("f (c)", "x", sep = mtSeparator),
    paste("z", "g (mid)", sep = mtSeparator), paste("z", "g (high)", sep = mtSeparator), paste("z", "g (top)", sep = mtSeparator)))
})

test_that("basic mode: unused levels, ordered factors, characters and a model without terms", {
  data   <- mtData()
  data$f <- factor(as.character(data$f), levels = c("a", "b", "c", "unused"))
  data$g <- factor(data$g, levels = levels(data$g), ordered = TRUE)
  data$h <- as.character(data$f)
  prepared <- prepareModelTerms(list(mtBasic(list("f", "g", "h", c("g", "x")))), data)
  fit      <- mtExpectSameModel(prepared, 1, data)

  labels <- modelTermsLabels(prepared, 1, names(stats::coef(fit)))
  expect_false(any(grepl("unused", labels)))
  expect_true(all(c("f (b)", "g (linear)", "g (quadratic)", "g (cubic)", "h (b)") %in% labels))

  empty <- prepareModelTerms(list(mtBasic(list())), data)
  expect_equal(modelTermsFormula(empty, 1, response = "y"), y ~ 1, ignore_formula_env = TRUE)
  expect_equal(dim(mtDesign(empty, 1)), c(nrow(data), 0L))
  expect_equal(dim(mtDesign(empty, 1, intercept = TRUE)), c(nrow(data), 1L))
  expect_equal(modelTermsLabels(empty, 1, "(Intercept)"), "(Intercept)")
  expect_equal(empty$nested, logical(0))
})


test_that("basic mode design on new data keeps the levels of the fitting data", {
  data     <- mtData()
  prepared <- prepareModelTerms(list(mtBasic(list("f", "x"))), data)

  design <- mtDesign(prepared, 1, data = data.frame(f = c("b", "b"), x = c(1, 2)), intercept = TRUE)
  expect_equal(mtMatrix(design), rbind(c(1, 1, 0, 1), c(1, 1, 0, 2)))
  expect_equal(names(design), c("(Intercept)", "t1.1", "t1.2", "t2"))

  expect_match(mtError(mtDesign(prepared, 1, data = data.frame(f = "zzz", x = 1))),
               "Model 0: the data has levels of 'f' that are not in the fitting data: zzz.", fixed = TRUE)
  expect_match(mtError(mtDesign(prepared, 1, data = data.frame(f = "a"))), "Model 0: the data has no variable 'x'.", fixed = TRUE)
})

# Continuous specifications ---------------------------------------------------------------------------------------

test_that("every continuous specification equals its hand-computed column", {
  data <- mtData()
  x    <- data$x
  one  <- function(values) unname(as.matrix(values))

  expect_equal(mtColumns("none"), one(x))
  expect_equal(mtColumns("scale"), one((x - mean(x)) / stats::sd(x)))
  expect_equal(mtColumns("log"), one(log(x)))
  expect_equal(mtColumns("logBase", logBase = "10", logMultiplier = "1"), one(log10(x)))
  expect_equal(mtColumns("logBase", logBase = "2", logMultiplier = "-3"), one(-3 * log(x, base = 2)))
  expect_equal(mtColumns("logBase", logBase = "exp(1)", logMultiplier = "2*3"), one(6 * log(x)))
  expect_equal(mtColumns("reciprocal"), one(1 / x))
  expect_equal(mtColumns("power", exponent = "2"), one(x^2))
  expect_equal(mtColumns("power", exponent = "-1/2"), one(x^-0.5))
  expect_equal(mtColumns("root", rootDegree = "3"), one(x^(1 / 3)))
  expect_equal(mtColumns("root", rootDegree = "2"), one(sqrt(x)))
  expect_equal(mtColumns("polynomial", polynomialDegree = 3), unname(unclass(stats::poly(x, 3))[, ]), ignore_attr = TRUE)
  expect_equal(mtColumns("polynomial", polynomialDegree = "1"), one(unclass(stats::poly(x, 1))[, 1]), ignore_attr = TRUE)
  expect_equal(mtColumns("naturalSpline", splineDf = 3), unname(unclass(splines::ns(x, df = 3))[, ]), ignore_attr = TRUE)
  expect_equal(mtColumns("naturalSpline", splineDf = 1), one(unclass(splines::ns(x, df = 1))[, 1]), ignore_attr = TRUE)

  kB <- 8.617333262e-5
  expect_equal(mtColumns("arrhenius", temperatureUnit = "kelvin"), one(1 / (kB * x)))
  expect_equal(mtColumns("arrhenius", temperatureUnit = "celsius"), one(1 / (kB * (x + 273.15))))
  expect_equal(mtColumns("arrhenius", temperatureUnit = "fahrenheit"), one(1 / (kB * ((x - 32) * 5 / 9 + 273.15))))
})

test_that("a parameter the definition does not give takes its default from the catalogue", {
  x <- mtData()$x
  expect_equal(mtColumns("logBase"), unname(as.matrix(log10(x))))
  expect_equal(mtColumns("polynomial"), mtColumns("polynomial", polynomialDegree = 2))
  expect_equal(mtColumns("arrhenius"), mtColumns("arrhenius", temperatureUnit = "celsius"))
})

test_that("odd roots keep the sign of negative values and even roots reject them", {
  data <- mtData()
  expect_equal(mtColumns("root", data, variable = "w", rootDegree = "3"), unname(as.matrix(sign(data$w) * abs(data$w)^(1 / 3))))
  expect_equal(mtColumns("root", data, variable = "w", rootDegree = "-1"), unname(as.matrix(sign(data$w) * abs(data$w)^-1)))
  expect_match(mtError(mtColumns("root", data, variable = "w", rootDegree = "2")),
               "Model 0: Root(w, 2) is not a finite number for", fixed = TRUE)
  expect_match(mtError(mtColumns("root", data, variable = "w", rootDegree = "2.5")), "is not a finite number", fixed = TRUE)
  expect_match(mtError(mtColumns("power", data, variable = "w", exponent = "0.5")), "Model 0: w^0.5 is not a finite number for", fixed = TRUE)
  expect_equal(mtColumns("power", data, variable = "w", exponent = "3"), unname(as.matrix(data$w^3)))
})

test_that("display labels of the continuous specifications", {
  data  <- mtData()
  label <- function(specification, ...) {
    prepared <- prepareModelTerms(list(mtModel(list(mtDef("d1", "x", specification, ...)))), data)
    list(term = prepared$models[[1]]$terms$label, columns = attr(mtDesign(prepared, 1), "labels"))
  }

  expect_equal(label("none")$columns, "x")
  expect_equal(label("scale")$columns, "Scale(x)")
  expect_equal(label("log")$columns, "log(x)")
  expect_equal(label("logBase", logBase = "10", logMultiplier = "1")$columns, "log(x, base = 10)")
  expect_equal(label("logBase", logBase = "exp(1)", logMultiplier = "-2")$columns, "-2\u00B7log(x, base = exp(1))")
  expect_equal(label("reciprocal")$columns, "1/x")
  expect_equal(label("power", exponent = "2")$columns, "x^2")
  expect_equal(label("power", exponent = "1/3")$columns, "x^(1/3)")
  expect_equal(label("root", rootDegree = "3")$columns, "Root(x, 3)")
  expect_equal(label("polynomial", polynomialDegree = 2)$term, "Polynomial(x, degree = 2)")
  expect_equal(label("polynomial", polynomialDegree = 2)$columns, c("Polynomial(x, 1)", "Polynomial(x, 2)"))
  expect_equal(label("naturalSpline", splineDf = 1)$columns, "Natural cubic spline(x, 1)")
  expect_equal(label("naturalSpline", splineDf = 2)$term, "Natural cubic spline(x, df = 2)")
  expect_equal(label("arrhenius", temperatureUnit = "celsius")$columns, "Arrhenius(x, °C)")
})

# Further specifications ------------------------------------------------------------------------------------------

test_that("every specification of the catalogue has a builder", {
  data   <- mtData()
  data$p <- seq(0.05, 0.95, length.out = nrow(data))
  for (specification in .modelTermsCatalogue()) {
    name <- specification[["name"]]
    if (specification[["kind"]] == "constant")
      next
    variable <- if (specification[["kind"]] == "categorical") "f" else if (name %in% c("logit", "probit", "arcsineSqrt")) "p" else "x"
    prepared <- prepareModelTerms(list(mtModel(list(mtDef("d1", variable, name)), intercept = name != "indicator")), data)
    expect_equal(nrow(prepared$models[[1]]$terms), 1L, label = name)
    if (name != "offset")
      expect_gt(ncol(mtDesign(prepared)), 0L, label = name)
  }
  expect_setequal(c(.modelTermsSpecificationsOf("continuous"), .modelTermsSpecificationsOf("categorical"), .modelTermsSpecificationsOf("constant")),
                  names(.modelTermsCatalogue()))
})

test_that("the further continuous specifications equal their hand-computed columns", {
  data   <- mtData()
  x      <- data$x
  w      <- data$w
  one    <- function(values) unname(as.matrix(values))
  data$p <- seq(0.05, 0.95, length.out = nrow(data))
  data$q <- c(0, 1, seq(0.1, 0.9, length.out = nrow(data) - 2L))

  expect_equal(mtColumns("center"), one(x - mean(x)))
  expect_equal(mtColumns("center", center = "median"), one(x - stats::median(x)))
  expect_equal(mtColumns("center", center = "2.5"), one(x - 2.5))
  expect_equal(mtColumns("logShift"), one(log1p(x)))
  expect_equal(mtColumns("logShift", data, variable = "w", shift = "3"), one(log(w + 3)))
  expect_equal(mtColumns("boxCox", lambda = "0"), one(log(x)))
  expect_equal(mtColumns("boxCox", lambda = "0.5"), one((x^0.5 - 1) / 0.5))
  expect_equal(mtColumns("boxCox", lambda = "-1"), one((x^-1 - 1) / -1))

  # Yeo and Johnson (2000), also as jaspBase's computed-column function has it
  yeoJohnson <- function(x, lambda) suppressWarnings(ifelse(x >= 0,
    if (lambda == 0) log1p(x) else ((x + 1)^lambda - 1) / lambda,
    if (lambda == 2) -log1p(-x) else -((1 - x)^(2 - lambda) - 1) / (2 - lambda)))
  for (lambda in c(0, 0.5, 2, -1)) {
    expect_equal(mtColumns("yeoJohnson", data, variable = "w", lambda = format(lambda)), one(yeoJohnson(w, lambda)))
    expect_equal(mtColumns("yeoJohnson", data, variable = "w", lambda = format(lambda)), one(YeoJohnson(w, lambda)))
  }

  expect_equal(mtColumns("asinh", data, variable = "w"), one(asinh(w)))
  expect_equal(mtColumns("logit", data, variable = "p"), one(stats::qlogis(data$p)))
  expect_equal(mtColumns("probit", data, variable = "p"), one(stats::qnorm(data$p)))
  expect_equal(mtColumns("arcsineSqrt", data, variable = "q"), one(asin(sqrt(data$q))))
  expect_equal(mtColumns("rawPolynomial", polynomialDegree = 3), unname(cbind(x, x^2, x^3)))
  expect_equal(mtColumns("bSpline", splineDf = 5, splineDegree = 3), unname(unclass(splines::bs(x, df = 5, degree = 3))[, ]), ignore_attr = TRUE)
  expect_equal(mtColumns("bSpline", splineDf = 2, splineDegree = 1), unname(unclass(splines::bs(x, df = 2, degree = 1))[, ]), ignore_attr = TRUE)
  expect_equal(mtColumns("hinge", knot = "3"), one(pmax(x - 3, 0)))
  expect_equal(mtColumns("hinge"), one(pmax(x - stats::median(x), 0)))
  expect_equal(mtColumns("threshold", cutoff = "2.5"), one(as.numeric(x > 2.5)))
  expect_equal(mtColumns("threshold", cutoff = "mean"), one(as.numeric(x > mean(x))))
  expect_equal(mtColumns("fourier", period = "4", harmonics = 2), unname(cbind(sin(2 * pi * x / 4), cos(2 * pi * x / 4), sin(4 * pi * x / 4), cos(4 * pi * x / 4))))
  expect_equal(mtColumns("rank"), one(rank(x)))
  expect_equal(mtColumns("normalScores"), one(stats::qnorm(rank(x) / (length(x) + 1))))

  # bins: quantile groups, coded with the first bin as reference
  bin <- findInterval(x, stats::quantile(x, c(1, 2, 3) / 4, names = FALSE)) + 1L
  expect_equal(mtColumns("binning", bins = 4), unname(1 * outer(bin, 2:4, `==`)))
})

test_that("display labels of the further specifications", {
  data   <- mtData()
  label  <- function(specification, ...) {
    prepared <- prepareModelTerms(list(mtModel(list(mtDef("d1", "x", specification, ...)))), data)
    list(term = prepared$models[[1]]$terms$label, columns = attr(mtDesign(prepared, 1), "labels"))
  }

  expect_equal(label("center")$columns, "Center(x)")
  expect_equal(label("center", center = "2")$columns, "Center(x, 2)")
  expect_equal(label("logShift")$columns, "log(x + 1)")
  expect_equal(label("logShift", shift = "-0.5")$columns, "log(x - 0.5)")
  expect_equal(label("boxCox", lambda = "0.5")$columns, "Box-Cox(x, \u03BB = 0.5)")
  expect_equal(label("yeoJohnson", lambda = "1/2")$columns, "Yeo-Johnson(x, \u03BB = 1/2)")
  expect_equal(label("rawPolynomial", polynomialDegree = 3)$term, "Raw polynomial(x, degree = 3)")
  expect_equal(label("rawPolynomial", polynomialDegree = 3)$columns, c("x", "x^2", "x^3"))
  expect_equal(label("bSpline")$term, "B-spline(x, df = 4, degree = 3)")
  expect_equal(label("bSpline")$columns, paste0("B-spline(x, ", 1:4, ")"))
  expect_equal(label("hinge")$columns, "Hinge(x, median)")
  expect_equal(label("threshold", cutoff = "3")$columns, "x > 3")
  expect_equal(label("fourier")$term, "Fourier(x, period = 12)")
  expect_equal(label("fourier", period = "12", harmonics = 2)$columns,
               c("sin(2\u03C0x/12)", "cos(2\u03C0x/12)", "sin(4\u03C0x/12)", "cos(4\u03C0x/12)"))
  expect_equal(label("binning", bins = 3)$term, "Bins(x, 3)")
  breaks <- formatC(signif(stats::quantile(data$x, c(1, 2) / 3, names = FALSE), 4L), digits = 4L, format = "fg")
  expect_equal(label("binning", bins = 3)$columns, c(sprintf("x [%s, %s)", breaks[1], breaks[2]), sprintf("x [%s, \u221E)", breaks[2])))
  expect_equal(label("rank")$columns, "rank(x)")
  expect_equal(label("normalScores")$columns, "Normal scores(x)")
})

test_that("the labels of the terms are the ones the form shows (the catalogue's patterns)", {
  data  <- mtData()
  label <- function(specification, variable = "x", ...) {
    prepared <- prepareModelTerms(list(mtModel(list(mtDef("d1", variable, specification, ...)), intercept = specification != "indicator")), data)
    prepared$models[[1]]$terms$label
  }

  expect_equal(label("treatment", "f"), "f")
  expect_equal(label("treatment", "f", referenceLevel = "b"), "f (reference b)")
  expect_equal(label("simple", "f"), "f (simple)")
  expect_equal(label("simple", "f", referenceLevel = "c"), "f (simple, reference c)")
  expect_equal(label("sum", "f"), "f (sum)")
  expect_equal(label("logBase", logBase = "2", logMultiplier = "1"), "log(x, base = 2)")
  expect_equal(label("logBase", logBase = "2", logMultiplier = "3"), "3\u00B7log(x, base = 2)")
  expect_equal(label("power", exponent = "-1"), "x^(-1)")
  expect_equal(label("fourier", harmonics = "2"), "Fourier(x, period = 12, harmonics = 2)")
  expect_equal(label("arrhenius", temperatureUnit = "fahrenheit"), "Arrhenius(x, \u00B0F)")
  expect_equal(prepareModelTerms(list(mtModel(list(list(id = "d1", specification = "offsetConstant", value = 2.50)))), data)$models[[1]]$terms$label, "offset(2.5)")
  expect_equal(.modelTermsShortest(c(0.1234567890123456)), "0.1234567890123456")   # as the desktop writes it, not 15 digits
  expect_equal(.modelTermsShortest(0.0001), "0.0001")
  expect_equal(.modelTermsShortest(1e-20), "1e-20")

  # a column named (Intercept) is not the intercept
  named <- data
  named[["(Intercept)"]] <- named$x
  expect_equal(prepareModelTerms(list(mtModel(list(mtDef("d1", "(Intercept)", "none")))), named)$models[[1]]$terms$label, "`(Intercept)`")

  # every pattern of the catalogue fills in: no placeholder or bracket is left, at the defaults of its parameters
  data$p <- seq(0.05, 0.95, length.out = nrow(data))
  for (specification in .modelTermsCatalogue()) {
    name       <- specification[["name"]]
    definition <- if (specification[["kind"]] == "constant") list(specification = name, value = 2)
                  else list(variable = if (specification[["kind"]] == "categorical") "f" else "x", specification = name)
    filled     <- .modelTermsLabel(definition, levels(data$f))
    expect_false(grepl("[][{}]", filled), label = name)
    names      <- regmatches(specification[["label"]], gregexpr("(?<=\\{)[^}|]*", specification[["label"]], perl = TRUE))[[1L]]
    expect_true(all(names %in% c("x", vapply(specification[["parameters"]], `[[`, "", "name"))), label = name)
  }
})

test_that("a column named pi does not change a Fourier term", {
  data    <- mtData()
  data$pi <- data$x
  model   <- mtModel(list(mtDef("d1", "pi", "fourier", period = "4")))
  design  <- mtDesign(prepareModelTerms(list(model), data))
  expect_equal(unname(as.matrix(design)), unname(cbind(sin(2 * base::pi * data$x / 4), cos(2 * base::pi * data$x / 4))))
  mtExpectSameModel(prepareModelTerms(list(model), data), 1, data)
})

test_that("settings are compared as values: nesting and duplicates", {
  data <- mtData()
  model <- function(name, ...) mtModel(list(mtDef("d1", "x", "none", ...)), title = name, name = name)

  # a restriction of 0.5 is nested in the same restriction written as 1/2, and in a free term
  nested <- prepareModelTerms(list(model("a", restriction = "0.5"), model("b", restriction = "1/2"), model("c")), data)$nested
  expect_equal(nested, c(TRUE, TRUE))
  expect_false(prepareModelTerms(list(model("a", restriction = "0.5"), model("b", restriction = "1")), data)$nested)

  # a center of 2.5 is the same setting as 5/2
  expect_match(mtError(prepareModelTerms(list(mtModel(list(mtDef("d1", "x", "center", center = "2.5"), mtDef("d2", "x", "center", center = "5/2")))), data)),
               "'x' has two term definitions with the same settings", fixed = TRUE)
})

test_that("the further specifications check their settings and their domain", {
  data  <- mtData()
  error <- function(specification, ..., variable = "x", edit = NULL) {
    if (!is.null(edit)) data[[variable]] <- edit(data[[variable]])
    mtError(prepareModelTerms(list(mtModel(list(mtDef("d1", variable, specification, ...)))), data))
  }

  expect_match(error("logit", variable = "w"), "Model 0: logit(w) needs values of w between 0 and 1 (without 0 and 1);", fixed = TRUE)
  expect_match(error("probit", variable = "x"), "needs values of x between 0 and 1", fixed = TRUE)
  expect_match(error("arcsineSqrt", variable = "w"), "Model 0: asin(sqrt(w)) needs values of w from 0 to 1;", fixed = TRUE)
  expect_equal(error("logShift", variable = "w", shift = "1", edit = function(w) replace(abs(w), 1:2, c(-1, -1.5))),
               "Model 0: log(w + 1) needs values of w above -1; 2 values are not.")
  expect_match(error("boxCox", variable = "w"), "needs positive values of w", fixed = TRUE)
  expect_match(error("bSpline", splineDf = "2", splineDegree = "3"), "needs at least as many degrees of freedom as its degree (3)", fixed = TRUE)
  expect_match(error("binning", bins = 4, edit = function(x) c(rep(1, 80), 2:11)), "'x' cannot be split into 4 bins", fixed = TRUE)
  expect_match(error("binning", bins = "1"), "the number of bins for 'x' must be a whole number of at least 2", fixed = TRUE)
  expect_match(error("center", center = "middle"), "the center for 'x' must be mean, median or a single finite number, not 'middle'.", fixed = TRUE)
  expect_match(error("fourier", period = "0"), "the period for 'x' must be positive", fixed = TRUE)
  expect_match(mtError(prepareModelTerms(list(mtModel(list(mtDef("d1", "x", "binning", bins = "150")))), mtData(300))),
               "is too long to be used in a model formula (too many levels, knots or bins)", fixed = TRUE)

  # bin bounds that differ only beyond 4 digits keep enough digits to stay apart
  wide       <- data
  wide$x     <- 100000 + seq_len(nrow(data))
  binLabels  <- attr(mtDesign(prepareModelTerms(list(mtModel(list(mtDef("d1", "x", "binning", bins = 3)))), wide)), "labels")
  expect_equal(anyDuplicated(binLabels), 0L)
  expect_equal(binLabels, c("x [100030, 100060)", "x [100060, \u221E)"))   # the bounds are 100030.67 and 100060.33

  # an empty level cannot be coded, and a restriction is one list of numbers
  empty   <- data
  empty$f <- factor(c("", "a", "b")[rep_len(1:3, nrow(data))])
  expect_match(mtError(prepareModelTerms(list(mtModel(list(mtDef("d1", "f", "treatment")))), empty)),
               "Model 0: the nominal variable 'f' has an empty level.", fixed = TRUE)
  expect_match(error("none", restriction = "1) * (2"), "must be a finite number or numbers separated by commas or semicolons", fixed = TRUE)
  expect_match(mtError(prepareModelTerms(list(mtModel(list(mtDef("d1", "x", "hinge", knot = "3"), mtDef("d2", "x", "hinge", knot = "3")))), data)),
               "'x' has two term definitions with the same settings", fixed = TRUE)
  expect_null(mtError(prepareModelTerms(list(mtModel(list(mtDef("d1", "x", "hinge", knot = "3"), mtDef("d2", "x", "hinge", knot = "median")))), data)))
})

test_that("models with the further specifications are the same in formula and data mode", {
  data   <- mtData()
  model  <- mtModel(list(mtDef("d1", "x", "center"), mtDef("d2", "z", "bSpline"), mtDef("d3", "w", "binning", bins = 3),
                         mtDef("d4", "x", "fourier", period = "3"), mtDef("d5", "z", "threshold"), mtDef("d6", "w", "yeoJohnson")),
                    terms = list("d1", "d2", "d3", "d4", "d6", c("d1", "d5"), c("d3", "d6")))
  mtExpectSameModel(prepareModelTerms(list(model), data), 1, data)
})

test_that("the further codings equal their contrast matrices and their coefficients mean what they say", {
  data  <- mtData()
  rows  <- as.integer(data$g)
  means <- tapply(data$y, data$g, mean)
  coded <- function(coding, ...) {
    prepared <- prepareModelTerms(list(mtModel(list(mtDef("d1", "g", coding, ...)), intercept = coding != "indicator")), data)
    design   <- mtDesign(prepared)
    list(values = unname(as.matrix(design)), labels = attr(design, "labels"),
         coefficients = unname(stats::coef(stats::lm(modelTermsFormula(prepared, 1, response = "y"), data))))
  }

  simple <- coded("simple")
  expect_equal(simple$values, unname((stats::contr.treatment(4) - 1 / 4)[rows, ]))
  expect_equal(simple$coefficients, c(mean(means), means[-1] - means[1]), ignore_attr = TRUE)
  expect_equal(coded("simple", referenceLevel = "high")$values, unname((stats::contr.treatment(4, base = 3) - 1 / 4)[rows, ]))

  differences <- outer(rep(1, 4), 1:3) / 4 - outer(1:4, 1:3, "<=")
  if (requireNamespace("MASS", quietly = TRUE))
    expect_equal(unname(differences), unname(MASS::contr.sdif(4)))
  successive <- coded("successiveDifferences")
  expect_equal(successive$values, unname(differences[rows, ]))
  expect_equal(successive$coefficients, c(mean(means), diff(means)), ignore_attr = TRUE)
  expect_equal(successive$labels, c("g (mid \u2212 low)", "g (high \u2212 mid)", "g (top \u2212 high)"))
  forward <- coded("forwardDifference")
  expect_equal(forward$coefficients, c(mean(means), -diff(means)), ignore_attr = TRUE)
  expect_equal(forward$labels, c("g (low \u2212 mid)", "g (mid \u2212 high)", "g (high \u2212 top)"))

  reverse <- coded("reverseHelmert")
  expect_equal(reverse$values, unname(stats::contr.helmert(4)[4:1, 3:1][rows, ]))
  expect_equal(reverse$labels, c("g (low vs. mid, high, top)", "g (mid vs. high, top)", "g (high vs. top)"))
  expect_equal(reverse$coefficients, unname(stats::coef(stats::lm(y ~ g, data, contrasts = list(g = stats::contr.helmert(4)[4:1, 3:1])))))

  indicator <- coded("indicator")
  expect_equal(indicator$values, unname(diag(4)[rows, ]))
  expect_equal(indicator$labels, c("g (low)", "g (mid)", "g (high)", "g (top)"))
  expect_equal(indicator$coefficients, unname(means), ignore_attr = TRUE)
  expect_match(mtError(prepareModelTerms(list(mtModel(list(mtDef("d1", "g", "indicator")))), data)),
               "Model 0: the indicator coding of 'g' (one column per level) needs a model without intercept.", fixed = TRUE)
})

test_that("rank terms depend on the sample: as rank(x) in lm(), and not for new data", {
  data     <- mtData()
  prepared <- prepareModelTerms(list(mtModel(list(mtDef("d1", "x", "rank"), mtDef("d2", "z", "normalScores")))), data)
  expect_true(prepared$models[[1]]$sampleDependent)
  fit <- stats::lm(modelTermsFormula(prepared, 1, response = "y"), data)
  expect_equal(unname(stats::coef(fit)), unname(stats::coef(stats::lm(y ~ rank(x) + qnorm(rank(z) / (length(z) + 1)), data))))
  expect_match(mtError(mtDesign(prepared, 1, data = data[1:3, ])), "Model 0: a rank or normal score depends on the whole sample", fixed = TRUE)
  expect_false(prepareModelTerms(list(mtModel(list(mtDef("d1", "x", "none")))), data)$models[[1]]$sampleDependent)
})

test_that("frozen constants: new data never changes scale, polynomial or spline", {
  data <- mtData()
  new  <- data.frame(x = c(1.5, 2, 9, 40), z = c(2, 3, 4, 5), w = c(-1, 0, 1.5, 3))
  model <- mtModel(list(mtDef("d1", "x", "scale"), mtDef("d2", "z", "polynomial", polynomialDegree = 3),
                        mtDef("d3", "w", "naturalSpline", splineDf = 3)))
  prepared <- prepareModelTerms(list(model), data)

  design <- mtDesign(prepared, 1, data = new)
  expect_equal(design$t1, (new$x - mean(data$x)) / stats::sd(data$x))
  expect_equal(mtMatrix(design[c("t2.1", "t2.2", "t2.3")]), unname(stats::predict(stats::poly(data$z, 3), new$z)), ignore_attr = TRUE)
  expect_equal(mtMatrix(design[c("t3.1", "t3.2", "t3.3")]), unname(stats::predict(splines::ns(data$w, df = 3), new$w)), ignore_attr = TRUE)

  # formula mode: predict() on the fit gives the design columns of the new data times the coefficients
  fit <- stats::lm(modelTermsFormula(prepared, 1, response = "y"), data)
  new$y <- 0
  expect_equal(unname(stats::predict(fit, new)),
               unname(drop(as.matrix(mtDesign(prepared, 1, data = new, intercept = TRUE)) %*% stats::coef(fit))))

  # the constants are numeric literals in the text of the term and keep their value when the text is parsed again
  terms <- prepared$models[[1]]$terms
  scaleText <- terms$expression[terms$id == "t1"]
  expect_false(grepl("mean", scaleText, fixed = TRUE))
  expect_match(scaleText, "[0-9]\\.[0-9]{14,}")
  expect_identical(as.numeric(eval(str2lang(scaleText), data)), mtDesign(prepared, 1)$t1)
  polyText <- terms$expression[terms$id == "t2"]
  expect_identical(as.vector(eval(str2lang(polyText), data)), as.vector(mtMatrix(mtDesign(prepared, 1)[c("t2.1", "t2.2", "t2.3")])))
  splineText <- terms$expression[terms$id == "t3"]
  expect_identical(as.vector(eval(str2lang(splineText), data)), as.vector(mtMatrix(mtDesign(prepared, 1)[c("t3.1", "t3.2", "t3.3")])))
})

test_that("new data is checked against the domain of the learned versions", {
  data     <- mtData()
  prepared <- prepareModelTerms(list(mtModel(list(mtDef("d1", "x", "log"), mtDef("d2", "z", "reciprocal")))), data)

  expect_match(mtError(mtDesign(prepared, 1, data = data.frame(x = c(1, -1, 0), z = 1))),
               "Model 0: log(x) needs positive values of x; 2 values are zero or negative.", fixed = TRUE)
  expect_match(mtError(mtDesign(prepared, 1, data = data.frame(x = 1, z = 0))),
               "Model 0: 1/z needs nonzero values of z; 1 value is zero.", fixed = TRUE)
  expect_match(mtError(mtDesign(prepared, 1, data = data.frame(x = "a", z = 1))), "the variable 'x' must be numeric", fixed = TRUE)
  expect_match(mtError(mtDesign(prepared, 1, data = data.frame(x = NA_real_, z = 1))), "needs finite values of x", fixed = TRUE)
  expect_equal(unname(as.matrix(mtDesign(prepared, 1, data = data.frame(x = 2, z = 4)))), cbind(log(2), 1 / 4))
})

# Advanced mode ---------------------------------------------------------------------------------------------------

test_that("a variable can be defined several times and the terms combine the definitions", {
  data     <- mtData()
  prepared <- prepareModelTerms(list(mtExampleModel()), data)
  terms    <- prepared$models[[1]]$terms

  expect_true(prepared$advanced)
  expect_equal(terms$id, c("t1", "t2", "t3", "t4", "t5", "t6"))
  expect_equal(terms$type, c("main", "main", "main", "main", "interaction", "interaction"))
  expect_equal(terms$label, c("x", "log(x)", "log(z, base = 10)", "f (reference b)",
                              paste("x", "log(z, base = 10)", sep = mtSeparator),
                              paste("log(x)", "log(z, base = 10)", sep = mtSeparator)))
  expect_equal(terms$expression[c(1:3, 5:6)], c("x", "log(x)", "log(z, base = 10)", "x:log(z, base = 10)", "log(x):log(z, base = 10)"))
  expect_equal(terms$restriction, rep("", 6))

  columns <- mtByLabel(prepared)
  expect_equal(names(columns), c("x", "log(x)", "log(z, base = 10)", "f (a)", "f (c)",
                                 paste("x", "log(z, base = 10)", sep = mtSeparator),
                                 paste("log(x)", "log(z, base = 10)", sep = mtSeparator)))
  expect_equal(columns[["f (a)"]], as.numeric(data$f == "a"))
  expect_equal(columns[["f (c)"]], as.numeric(data$f == "c"))
  expect_equal(columns[[6]], data$x * log10(data$z))
  expect_equal(columns[[7]], log(data$x) * log10(data$z))

  fit <- mtExpectSameModel(prepared, 1, data)
  expect_equal(modelTermsLabels(prepared, 1, names(stats::coef(fit))), c("(Intercept)", names(columns)))
  expect_equal(modelTermsLabels(prepared, 1, c("(Intercept)", "Log(scale)")), c("(Intercept)", "Log(scale)"))
})

test_that("a term does not need its margins: an interaction can stand alone", {
  data  <- mtData()
  model <- mtModel(list(mtDef("d1", "x", "log"), mtDef("d2", "z", "scale")), terms = list(c("d1", "d2")))
  prepared <- prepareModelTerms(list(model), data)

  expect_equal(prepared$models[[1]]$terms$id, "t1")
  expect_equal(mtMatrix(mtDesign(prepared, 1)), cbind(log(data$x) * (data$z - mean(data$z)) / stats::sd(data$z)))
  mtExpectSameModel(prepared, 1, data)
})

test_that("the formula keeps the order of the model, the terms are ordered by degree like lm()", {
  data  <- mtData()
  model <- mtModel(list(mtDef("d1", "x", "none"), mtDef("d2", "z", "none")), terms = list(c("d1", "d2"), "d1", "d2"))
  prepared <- prepareModelTerms(list(model), data)

  expect_equal(prepared$models[[1]]$terms$id, c("t2", "t3", "t1"))
  expect_equal(modelTermsFormula(prepared, 1, response = "y"), y ~ x:z + x + z, ignore_formula_env = TRUE)
  mtExpectSameModel(prepared, 1, data)
})

test_that("the option is read the same from lists as from character vectors", {
  data  <- mtData()
  asList <- function(model) {
    model$components <- lapply(model$components, as.list)
    model$terms <- lapply(model$terms, function(term) {
      term$operands <- as.list(term$operands)
      term
    })
    model
  }
  vectors <- prepareModelTerms(list(mtExampleModel()), data)
  lists   <- prepareModelTerms(list(asList(mtExampleModel())), data)

  expect_equal(lists$models[[1]]$terms, vectors$models[[1]]$terms)
  expect_equal(mtDesign(lists, 1), mtDesign(vectors, 1))
})

test_that("the terms come from `terms` and the definitions, not from `components`", {
  data  <- mtData()
  model <- mtExampleModel()
  model$components <- list("q")
  expect_equal(mtDesign(prepareModelTerms(list(model), data), 1), mtDesign(prepareModelTerms(list(mtExampleModel()), data), 1))
})

test_that("missing optional keys take their defaults", {
  data  <- mtData()
  model <- list(name = "model0",
                components = list("x", "z", "f"),
                definitions = list(list(id = "d1", variable = "x", specification = "logBase", logBase = 10),
                                   list(id = "d2", variable = "z", specification = "arrhenius"),
                                   list(id = "d3", variable = "f", specification = "treatment")),
                terms = list(list(id = "t1", operands = "d1"), list(id = "t2", operands = "d2"), list(id = "t3", operands = "d3")))
  prepared <- prepareModelTerms(list(model), data)

  expect_equal(prepared$models[[1]]$title, "model0")
  expect_equal(prepared$models[[1]]$terms$restriction, rep("", 3))
  expect_equal(prepared$models[[1]]$terms$label, c("log(x, base = 10)", "Arrhenius(z, \u00B0C)", "f"))
  design <- mtDesign(prepared, 1)
  expect_equal(design$t1, log10(data$x))
  expect_equal(attr(design, "labels")[3:4], c("f (b)", "f (c)"))
})

test_that("definitions that no term uses are neither read nor checked", {
  data     <- mtData()
  data$w   <- abs(data$w) + 0.5
  data$w[1:3] <- c(-1, 0, -2)

  # log(w) would be invalid on the fitting data, the other definitions are no good either: no term uses them
  definitions <- list(mtDef("d1", "x", "none"), mtDef("d2", "w", "none"), mtDef("d3", "w", "log"),
                      mtDef("d4", "q", "logBase", logBase = "not a number", restriction = "this is no restriction"),
                      mtDef("d5", "f", "cubicRoot"), mtDef("d6", "f", "treatment"), mtDef("d7", "f", "sum"), mtDef("d8", "x", "none"))
  model <- mtModel(definitions, terms = list("d1", "d2", "d6"))
  model$definitions[[9]] <- list(id = "d9")
  prepared <- prepareModelTerms(list(model), data)
  expect_equal(prepared$models[[1]]$terms$id, c("t1", "t2", "t3"))
  expect_equal(length(prepared$models[[1]]$versions), 3)

  # a definition that a term uses is checked on the fitting data
  model <- mtModel(definitions, terms = list("d1", "d3"))
  expect_match(mtError(prepareModelTerms(list(model), data)),
               "Model 0: log(w) needs positive values of w; 3 values are zero or negative.", fixed = TRUE)
})

test_that("a model without terms has the intercept only", {
  data     <- mtData()
  prepared <- prepareModelTerms(list(mtModel(list()), mtModel(list(mtDef("d1", "x", "none")), title = "Model 1", name = "model1")), data)

  expect_true(prepared$advanced)
  expect_equal(modelTermsFormula(prepared, 1, response = "y"), y ~ 1, ignore_formula_env = TRUE)
  expect_equal(dim(mtDesign(prepared, 1)), c(nrow(data), 0L))
  expect_equal(modelTermsLabels(prepared, 1, "(Intercept)"), "(Intercept)")
  expect_equal(prepared$nested, TRUE)

  # the same when the definitions are there and no term uses them
  unused <- prepareModelTerms(list(mtModel(list(mtDef("d1", "x", "none")), terms = list())), data)
  expect_equal(modelTermsFormula(unused, 1), ~ 1, ignore_formula_env = TRUE)
})

# Factor codings --------------------------------------------------------------------------------------------------

test_that("every factor coding equals the contrasts of R, with labels", {
  data <- mtData()
  f    <- as.integer(data$f)
  g    <- as.integer(data$g)
  code <- function(specification, variable = "f", ...) {
    prepared <- prepareModelTerms(list(mtModel(list(mtDef("d1", variable, specification, ...)))), data)
    design   <- mtDesign(prepared, 1)
    list(values = mtMatrix(design), labels = attr(design, "labels"), term = prepared$models[[1]]$terms$label)
  }

  treatment <- code("treatment")
  expect_equal(treatment$values, unname(stats::contr.treatment(3)[f, ]))
  expect_equal(treatment$labels, c("f (b)", "f (c)"))
  expect_equal(treatment$term, "f")

  reference <- code("treatment", referenceLevel = "b")
  expect_equal(reference$values, cbind(as.numeric(data$f == "a"), as.numeric(data$f == "c")))
  expect_equal(reference$labels, c("f (a)", "f (c)"))
  expect_equal(code("treatment", referenceLevel = "c")$values, unname(stats::contr.treatment(3, base = 3)[f, ]))
  expect_equal(code("treatment", "g", referenceLevel = "high")$labels, c("g (low)", "g (mid)", "g (top)"))

  expect_equal(code("sum")$values, unname(stats::contr.sum(3)[f, ]))
  expect_equal(code("sum")$labels, c("f (a)", "f (b)"))
  expect_equal(code("helmert", "g")$values, unname(stats::contr.helmert(4)[g, ]))
  expect_equal(code("helmert", "g")$labels, c("g (mid vs. low)", "g (high vs. low, mid)", "g (top vs. low, mid, high)"))
  expect_equal(code("orthogonalPolynomial", "g")$values, unname(stats::contr.poly(4)[g, ]), ignore_attr = TRUE)
  expect_equal(code("orthogonalPolynomial", "g")$labels, c("g (linear)", "g (quadratic)", "g (cubic)"))
  expect_equal(code("sas")$values, unname(stats::contr.SAS(3)[f, ]))
  expect_equal(code("sas")$labels, c("f (a)", "f (b)"))
})

test_that("a factor coding gives the coefficients of lm() with the same contrasts", {
  data <- mtData()
  for (coding in c("treatment", "sum", "helmert", "orthogonalPolynomial", "sas")) {
    prepared <- prepareModelTerms(list(mtModel(list(mtDef("d1", "g", coding), mtDef("d2", "x", "none")))), data)
    contrasts <- c(treatment = "contr.treatment", sum = "contr.sum", helmert = "contr.helmert",
                   orthogonalPolynomial = "contr.poly", sas = "contr.SAS")[[coding]]
    reference <- stats::lm(y ~ g + x, data, contrasts = list(g = contrasts))
    fit       <- mtExpectSameModel(prepared, 1, data)
    expect_equal(unname(stats::coef(fit))[c(1, 5, 2:4)], unname(stats::coef(reference))[c(1, 5, 2:4)], info = coding)
  }
})

test_that("a nominal variable needs a coding and a continuous variable does not accept one", {
  data <- mtData()
  expect_match(mtError(prepareModelTerms(list(mtModel(list(mtDef("d1", "f", "log")))), data)),
               "Model 0: log(f) needs a continuous variable, but 'f' is nominal.", fixed = TRUE)
  expect_match(mtError(prepareModelTerms(list(mtModel(list(mtDef("d1", "f", "none")))), data)),
               "Model 0: f needs a continuous variable, but 'f' is nominal.", fixed = TRUE)
  expect_match(mtError(prepareModelTerms(list(mtModel(list(mtDef("d1", "x", "sum")))), data)),
               "Model 0: x (sum) needs a nominal variable, but 'x' is not nominal.", fixed = TRUE)
  expect_match(mtError(prepareModelTerms(list(mtModel(list(mtDef("d1", "x", "treatment", referenceLevel = "b")))), data)),
               "Model 0: x (reference b) needs a nominal variable, but 'x' is not nominal.", fixed = TRUE)
  expect_match(mtError(prepareModelTerms(list(mtModel(list(mtDef("d1", "x", "treatment")))), data)),
               "Model 0: x needs a nominal variable, but 'x' is not nominal.", fixed = TRUE)
})

test_that("the levels of the coding are those of the fitting data", {
  data   <- mtData()
  data$f <- factor(as.character(data$f), levels = c("a", "b", "c", "unused"))
  prepared <- prepareModelTerms(list(mtModel(list(mtDef("d1", "f", "treatment")))), data)

  mtExpectSameModel(prepared, 1, data)
  expect_equal(attr(mtDesign(prepared, 1), "labels"), c("f (b)", "f (c)"))
  expect_match(mtError(prepareModelTerms(list(mtModel(list(mtDef("d1", "f", "treatment", referenceLevel = "unused")))), data)),
               "Model 0: the reference level 'unused' of 'f' is not in the fitting data.", fixed = TRUE)
  expect_match(mtError(prepareModelTerms(list(mtModel(list(mtDef("d1", "f", "treatment", referenceLevel = "zzz")))), data)),
               "the reference level 'zzz' of 'f' is not in the fitting data.", fixed = TRUE)

  single <- data.frame(y = 1:4, f = factor(rep("a", 4)))
  expect_match(mtError(prepareModelTerms(list(mtModel(list(mtDef("d1", "f", "sum")))), single)),
               "Model 0: the nominal variable 'f' needs at least two levels in the fitting data.", fixed = TRUE)

  # characters are nominal too
  data$h <- as.character(data$f)
  prepared <- prepareModelTerms(list(mtModel(list(mtDef("d1", "h", "sum")))), data)
  expect_equal(mtMatrix(mtDesign(prepared, 1)), unname(stats::contr.sum(3)[as.integer(factor(data$h)), ]))
})

test_that("new data may contain some of the levels of the fitting data and no others", {
  data     <- mtData()
  prepared <- prepareModelTerms(list(mtModel(list(mtDef("d1", "f", "treatment", referenceLevel = "b"), mtDef("d2", "x", "none")))), data)

  new <- data.frame(f = factor(c("c", "a"), levels = c("a", "c")), x = c(1, 2))
  expect_equal(mtMatrix(mtDesign(prepared, 1, data = new)), cbind(c(0, 1), c(1, 0), c(1, 2)))
  expect_equal(mtMatrix(mtDesign(prepared, 1, data = data.frame(f = c("a", "c"), x = c(1, 2)))), cbind(c(1, 0), c(0, 1), c(1, 2)))
  expect_match(mtError(mtDesign(prepared, 1, data = data.frame(f = c("a", "d", "e"), x = 1))),
               "Model 0: the data has levels of 'f' that are not in the fitting data: d, e.", fixed = TRUE)
  expect_match(mtError(mtDesign(prepared, 1, data = data.frame(f = 1, x = 1))), "the variable 'f' must be nominal", fixed = TRUE)

  # in formula mode the same level is an error as well, not a silent NA
  fit <- stats::lm(modelTermsFormula(prepared, 1, response = "y"), data)
  expect_error(stats::predict(fit, data.frame(f = "d", x = 1)))
})

test_that("interactions are products of the coded columns, in the order of the model matrix", {
  data <- mtData()
  model <- mtModel(list(
    mtDef("d1", "x", "polynomial", polynomialDegree = 2),
    mtDef("d2", "f", "treatment"),
    mtDef("d3", "g", "sum")),
    terms = list("d1", "d2", "d3", c("d1", "d2"), c("d2", "d3")))
  prepared <- prepareModelTerms(list(model), data)
  fit      <- mtExpectSameModel(prepared, 1, data)

  columns <- mtByLabel(prepared)
  poly    <- unclass(stats::poly(data$x, 2))
  sum     <- stats::contr.sum(4)[as.integer(data$g), ]
  # the first operand varies fastest: Polynomial(x, 1) and (x, 2) for f (b), then for f (c)
  expect_equal(names(columns)[8:11], c(
    paste("Polynomial(x, 1)", "f (b)", sep = mtSeparator), paste("Polynomial(x, 2)", "f (b)", sep = mtSeparator),
    paste("Polynomial(x, 1)", "f (c)", sep = mtSeparator), paste("Polynomial(x, 2)", "f (c)", sep = mtSeparator)))
  expect_equal(columns[[8]], poly[, 1] * (data$f == "b"), ignore_attr = TRUE)
  expect_equal(columns[[9]], poly[, 2] * (data$f == "b"), ignore_attr = TRUE)
  expect_equal(columns[[10]], poly[, 1] * (data$f == "c"), ignore_attr = TRUE)
  expect_equal(columns[[11]], poly[, 2] * (data$f == "c"), ignore_attr = TRUE)
  expect_equal(names(columns)[12:17], c(
    paste("f (b)", "g (low)", sep = mtSeparator), paste("f (c)", "g (low)", sep = mtSeparator),
    paste("f (b)", "g (mid)", sep = mtSeparator), paste("f (c)", "g (mid)", sep = mtSeparator),
    paste("f (b)", "g (high)", sep = mtSeparator), paste("f (c)", "g (high)", sep = mtSeparator)))
  expect_equal(columns[[12]], (data$f == "b") * sum[, 1], ignore_attr = TRUE)
  expect_equal(columns[[13]], (data$f == "c") * sum[, 1], ignore_attr = TRUE)
  expect_equal(columns[[14]], (data$f == "b") * sum[, 2], ignore_attr = TRUE)
  expect_equal(columns[[17]], (data$f == "c") * sum[, 3], ignore_attr = TRUE)
  expect_equal(modelTermsLabels(prepared, 1, names(stats::coef(fit))), c("(Intercept)", names(columns)))
})

test_that("an interaction without its margins is still the product of the coded columns", {
  data  <- mtData()
  model <- mtModel(list(mtDef("d1", "f", "treatment"), mtDef("d2", "x", "none")), terms = list(c("d1", "d2")))
  prepared <- prepareModelTerms(list(model), data)
  design   <- mtDesign(prepared, 1)

  # two columns, not an indicator per level as lm() gives for the term f:x of a plain formula
  expect_equal(ncol(design), 2)
  expect_equal(mtMatrix(design), cbind((data$f == "b") * data$x, (data$f == "c") * data$x))
  mtExpectSameModel(prepared, 1, data)
})

test_that("two nominal variables keep their own levels and codings", {
  data  <- mtData()
  model <- mtModel(list(mtDef("d1", "f", "sum"), mtDef("d2", "g", "orthogonalPolynomial")))
  prepared <- prepareModelTerms(list(model), data)
  mtExpectSameModel(prepared, 1, data)
  expect_equal(attr(mtDesign(prepared, 1), "labels"), c("f (a)", "f (b)", "g (linear)", "g (quadratic)", "g (cubic)"))
})

test_that("a nominal variable with very many levels is rejected up front", {
  set.seed(1)
  levels <- sprintf("a rather long level label number %03d", 1:120)
  data   <- data.frame(y = rnorm(240), f = factor(rep(levels, 2), levels = levels))
  expect_match(mtError(prepareModelTerms(list(mtModel(list(mtDef("d1", "f", "treatment")))), data)),
               "Model 0: the term f is too long to be used in a model formula", fixed = TRUE)
  small <- data[data$f %in% levels[1:20], ]
  small$f <- droplevels(small$f)
  expect_silent(prepareModelTerms(list(mtModel(list(mtDef("d1", "f", "treatment")))), small))
})

# Labels ----------------------------------------------------------------------------------------------------------

test_that("labels decode the names of JASP columns", {
  assign(".decodeColNamesLax", function(x) gsub("JaspColumn_.1._Encoded", "Dose", gsub("JaspColumn_.2._Encoded", "Group", x, fixed = TRUE), fixed = TRUE),
         envir = globalenv())
  withr::defer(rm(".decodeColNamesLax", envir = globalenv()))

  data <- mtData()
  names(data)[names(data) == "x"] <- "JaspColumn_.1._Encoded"
  names(data)[names(data) == "f"] <- "JaspColumn_.2._Encoded"
  model <- mtModel(list(
    mtDef("d1", "JaspColumn_.1._Encoded", "log"),
    mtDef("d2", "JaspColumn_.1._Encoded", "scale"),
    mtDef("d3", "JaspColumn_.2._Encoded", "treatment")),
    terms = list("d1", "d2", "d3", c("d2", "d3")))
  prepared <- prepareModelTerms(list(model), data)
  fit      <- mtExpectSameModel(prepared, 1, data)

  expect_equal(modelTermsLabels(prepared, 1, names(stats::coef(fit))), c(
    "(Intercept)", "log(Dose)", "Scale(Dose)", "Group (b)", "Group (c)",
    paste("Scale(Dose)", "Group (b)", sep = mtSeparator), paste("Scale(Dose)", "Group (c)", sep = mtSeparator)))
  expect_equal(prepared$models[[1]]$terms$label[1:3], c("log(Dose)", "Scale(Dose)", "Group"))
  expect_match(prepared$models[[1]]$terms$expression[1], "log(JaspColumn_.1._Encoded)", fixed = TRUE)
})

test_that("labels work for glm() and for names that are not columns of the model", {
  data <- mtData()
  data$b <- as.numeric(data$y > 0)
  prepared <- prepareModelTerms(list(mtExampleModel()), data)
  fit <- stats::glm(modelTermsFormula(prepared, 1, response = "b"), data, family = stats::binomial())

  labels <- modelTermsLabels(prepared, 1, names(stats::coef(fit)))
  expect_equal(labels[1:5], c("(Intercept)", "x", "log(x)", "log(z, base = 10)", "f (a)"))
  expect_equal(modelTermsLabels(prepared, "model0", c("(Intercept)", "f (a)", "nothing")), c("(Intercept)", "f (a)", "nothing"))
})

test_that("R sees one variable per version, whatever constants the version carries", {
  data  <- mtData()
  model <- mtModel(list(
    mtDef("d1", "x", "polynomial", polynomialDegree = 2),
    mtDef("d2", "x", "naturalSpline", splineDf = 3),
    mtDef("d3", "z", "scale"),
    mtDef("d4", "f", "sum")),
    terms = list("d1", "d2", "d3", "d4", c("d1", "d3"), c("d2", "d3"), c("d1", "d4"), c("d3", "d2", "d4")))
  prepared <- prepareModelTerms(list(model), data)

  # four versions: the polynomial and spline of x, the scaled z and the coded f
  info <- stats::terms(modelTermsFormula(prepared, 1))
  expect_equal(length(attr(info, "variables")) - 1L, 4L)
  expect_equal(length(attr(info, "term.labels")), nrow(prepared$models[[1]]$terms))
  mtExpectSameModel(prepared, 1, data)
})

test_that("the labels of an interaction follow the order in which R orders its variables", {
  data  <- mtData()
  # z appears before x in the formula, so the interaction z:x has z as its first (fastest) operand, as lm() names it
  model <- mtModel(list(mtDef("d1", "x", "polynomial", polynomialDegree = 2), mtDef("d2", "z", "log")),
                   terms = list("d2", c("d1", "d2"), "d1"))
  prepared <- prepareModelTerms(list(model), data)
  fit      <- mtExpectSameModel(prepared, 1, data)

  columns <- mtByLabel(prepared)
  poly    <- unclass(stats::poly(data$x, 2))
  expect_equal(names(columns)[4:5], c(paste("log(z)", "Polynomial(x, 1)", sep = mtSeparator), paste("log(z)", "Polynomial(x, 2)", sep = mtSeparator)))
  expect_equal(columns[[4]], log(data$z) * poly[, 1], ignore_attr = TRUE)
  expect_equal(columns[[5]], log(data$z) * poly[, 2], ignore_attr = TRUE)
  expect_equal(prepared$models[[1]]$terms$label[3], paste("log(z)", "Polynomial(x, degree = 2)", sep = mtSeparator))
  expect_equal(modelTermsLabels(prepared, 1, names(stats::coef(fit))), c("(Intercept)", names(columns)))
})

# Invalid definitions and terms -----------------------------------------------------------------------------------

test_that("invalid options stop with a readable message", {
  data <- mtData()
  expectError <- function(model, message) {
    expect_match(mtError(prepareModelTerms(list(model), data)), message, fixed = TRUE)
    expect_s3_class(tryCatch(prepareModelTerms(list(model), data), error = identity), "validationError")
  }
  main <- function(...) mtModel(list(...))

  # the definitions a model uses are unique by variable, specification and settings
  expectError(main(mtDef("d1", "x", "log"), mtDef("d2", "x", "log")),
              "Model 0: 'x' has two term definitions with the same settings.")
  expectError(main(mtDef("d1", "x", "logBase", logBase = "10"), mtDef("d2", "x", "logBase", logBase = "5+5")),
              "'x' has two term definitions with the same settings.")
  expectError(main(mtDef("d1", "x", "polynomial", polynomialDegree = 2), mtDef("d2", "x", "polynomial", polynomialDegree = "2")),
              "'x' has two term definitions with the same settings.")
  expect_silent(prepareModelTerms(list(main(mtDef("d1", "x", "polynomial", polynomialDegree = 2), mtDef("d2", "x", "polynomial", polynomialDegree = 3))), data))

  # a nominal variable has one coding in a model; a second coding that no term of the model uses does not count
  expectError(main(mtDef("d1", "f", "treatment"), mtDef("d2", "f", "sum")),
              "Model 0: the nominal variable 'f' can have only one term definition.")
  expect_silent(prepareModelTerms(list(mtModel(list(mtDef("d1", "f", "treatment"), mtDef("d2", "f", "sum")), terms = list("d1"))), data))

  # operands are definitions of the model
  definitions <- list(mtDef("d1", "x", "none"), mtDef("d2", "z", "none"))
  expectError(mtModel(definitions, list("d1", c("d1", "d9"))),
              "Model 0: the term 't2' refers to 'd9', which is not a term definition of this model.")
  expectError(mtModel(list(), list("d1")), "Model 0: the term 't1' refers to 'd1', which is not a term definition of this model.")
  expectError(mtModel(definitions, list("d1", character(0))),
              "Model 0: the term 't2' needs one or more different definitions as operands.")
  expectError(mtModel(definitions, list(c("d1", "d1"))), "Model 0: the term 't1' needs one or more different definitions as operands.")

  # two terms of a model have different operands, in whatever order they are given
  expectError(mtModel(definitions, list("d1", "d2", c("d1", "d2"), c("d2", "d1"))),
              paste0("Model 0: two terms have the same operands: x", mtSeparator, "z."))
  expectError(mtModel(definitions, list("d1", "d1")), "Model 0: two terms have the same operands: x.")

  # a definition needs a variable and a specification
  expectError(main(mtDef("d1", "", "none")), "Model 0: the definition 'd1' needs a variable and a specification.")
  expectError(main(mtDef("d1", "x", "")), "Model 0: the definition 'd1' needs a variable and a specification.")
  missing <- main(mtDef("d1", "x", "none"))
  missing$definitions[[1]] <- list(id = "d1", variable = "x")
  expectError(missing, "Model 0: the definition 'd1' needs a variable and a specification.")

  # ids are unique and nonempty
  expectError(main(mtDef("d1", "x", "none"), mtDef("d1", "z", "none")), "Model 0: the term definitions need unique, nonempty ids.")
  expectError(main(mtDef("", "x", "none")), "the term definitions need unique, nonempty ids.")
  duplicate <- mtModel(definitions)
  duplicate$terms[[2]]$id <- "t1"
  expectError(duplicate, "Model 0: the terms need unique, nonempty ids.")
  duplicate$terms[[2]]$id <- ""
  expectError(duplicate, "the terms need unique, nonempty ids.")

  # unknown specifications and variables
  expectError(main(mtDef("d1", "x", "cubicRoot")), "Model 0: 'cubicRoot' is not a known term specification.")
  expectError(main(mtDef("d1", "q", "none")), "Model 0: the data has no variable 'q'.")
  expectError(main(mtDef("d1", "x", "arrhenius", temperatureUnit = "rankine")), "'rankine' is not a known temperature unit.")

  # the model title names the model
  expect_match(mtError(prepareModelTerms(list(mtModel(list(mtDef("d1", "x", "cubicRoot")), title = "My model")), data)),
               "^My model: ")

  # in an analysis with definitions, a model that has terms must have definitions too
  expect_match(mtError(prepareModelTerms(list(mtModel(definitions), mtBasic(list("x"), title = "Model 1", name = "model1")), data)),
               "Model 1: the model has terms but no term definitions.", fixed = TRUE)
})

test_that("a specification with settings that cannot be used stops with a readable message", {
  data <- mtData()
  expectError <- function(message, specification, ..., variable = "x", data. = data) {
    result <- mtError(prepareModelTerms(list(mtModel(list(mtDef("d1", variable, specification, ...)))), data.))
    expect_match(result, message, fixed = TRUE)
  }

  expectError("Model 0: the logarithm base for 'x' must be positive and different from 1.", "logBase", logBase = "1")
  expectError("must be positive and different from 1.", "logBase", logBase = "-2")
  expectError("must be positive and different from 1.", "logBase", logBase = "0")
  expectError("Model 0: the logarithm base for 'x' must be a single finite number or numeric expression, not 'abc'.", "logBase", logBase = "abc")
  expectError("not 'system(\"echo hi\")'.", "logBase", logBase = "system(\"echo hi\")")
  expectError("not 'Sys.getenv(\"HOME\")'.", "logBase", logBase = "Sys.getenv(\"HOME\")")
  expectError("not 'log(0)'.", "logBase", logBase = "log(0)")
  expectError("not '1; 2'.", "logBase", logBase = "1; 2")
  expectError("Model 0: the logarithm multiplier for 'x' must not be 0.", "logBase", logBase = "10", logMultiplier = "0")
  expectError("the exponent for 'x' must not be 0.", "power", exponent = "0")
  expectError("the exponent for 'x' must be a single finite number", "power", exponent = "1/0")
  expectError("the root degree for 'x' must not be 0.", "root", rootDegree = "0")
  expectError("the polynomial degree for 'x' must be a whole number of at least 1.", "polynomial", polynomialDegree = "0")
  expectError("the polynomial degree for 'x' must be a whole number of at least 1.", "polynomial", polynomialDegree = "1.5")
  expectError("the degrees of freedom of the spline for 'x' must be a whole number of at least 1.", "naturalSpline", splineDf = "0")
})

test_that("values outside the domain of a specification stop with a readable message", {
  data  <- mtData()
  error <- function(specification, ..., variable = "x", edit = NULL) {
    if (!is.null(edit)) data[[variable]] <- edit(data[[variable]])
    mtError(prepareModelTerms(list(mtModel(list(mtDef("d1", variable, specification, ...)))), data))
  }

  expect_equal(error("log", edit = function(x) replace(x, 1:3, c(0, -1, 0))),
               "Model 0: log(x) needs positive values of x; 3 values are zero or negative.")
  expect_equal(error("log", edit = function(x) replace(x, 4, 0)), "Model 0: log(x) needs positive values of x; 1 value is zero or negative.")
  expect_equal(error("logBase", logBase = "10", edit = function(x) replace(x, 1, -4)),
               "Model 0: log(x, base = 10) needs positive values of x; 1 value is zero or negative.")
  expect_equal(error("reciprocal", edit = function(x) replace(x, 5, 0)), "Model 0: 1/x needs nonzero values of x; 1 value is zero.")
  expect_equal(error("scale", edit = function(x) rep(5, length(x))), "Model 0: Scale(x) needs a standard deviation that is positive; 'x' is constant.")
  expect_equal(error("polynomial", polynomialDegree = 3, edit = function(x) rep(1:3, length.out = length(x))),
               "Model 0: a polynomial of degree 3 needs at least 4 different values of 'x'.")
  expect_equal(error("naturalSpline", splineDf = 3, edit = function(x) rep(1:3, length.out = length(x))),
               "Model 0: a natural cubic spline with 3 degrees of freedom needs at least 4 different values of 'x'.")
  expect_match(error("naturalSpline", splineDf = 3, edit = function(x) c(rep(1, 80), 2:11)),
               "the knots of the natural cubic spline of 'x' cannot be placed", fixed = TRUE)
  expect_equal(error("arrhenius", temperatureUnit = "celsius", edit = function(x) replace(x, 1, -300)),
               "Model 0: Arrhenius(x, °C) needs temperatures above absolute zero in the selected unit; 1 value of x is not.")
  expect_equal(error("arrhenius", temperatureUnit = "kelvin", edit = function(x) replace(x, 1:2, c(0, -5))),
               "Model 0: Arrhenius(x, K) needs temperatures above absolute zero in the selected unit; 2 values of x are not.")
  expect_equal(error("scale", edit = function(x) replace(x, 1, NA)), "Model 0: Scale(x) needs finite values of x; 1 value is missing or infinite.")
  expect_match(error("log", edit = function(x) replace(x, 1, Inf)), "needs finite values", fixed = TRUE)
  expect_match(error("log", variable = "f"), "needs a continuous variable, but 'f' is nominal", fixed = TRUE)
  expect_null(error("none", edit = function(x) replace(x, 1, NA)))
})

# Nesting ---------------------------------------------------------------------------------------------------------

test_that("nested models are detected by comparing the full specifications of the terms", {
  data    <- mtData()
  nested  <- function(...) prepareModelTerms(list(...), data)$nested
  basic   <- function(components, index) mtBasic(components, title = paste("Model", index), name = paste0("model", index))
  advanced <- function(definitions, terms = NULL, index = 0)
    mtModel(definitions, terms, title = paste("Model", index), name = paste0("model", index))

  # basic mode: subsets, and the order of the variables in a term does not matter
  expect_equal(nested(basic(list("x"), 0)), logical(0))
  expect_equal(nested(basic(list("x"), 0), basic(list("x", "z"), 1), basic(list("x", "z", c("x", "z")), 2)), c(TRUE, TRUE))
  expect_equal(nested(basic(list("x", "z"), 0), basic(list("x", "w"), 1)), FALSE)
  expect_equal(nested(basic(list("x", "z", c("x", "z")), 0), basic(list("x", "z"), 1)), FALSE)
  expect_equal(nested(basic(list("x", "z", c("z", "x")), 0), basic(list(c("x", "z"), "z", "x", "w"), 1)), TRUE)
  expect_equal(nested(basic(list(), 0), basic(list("x"), 1)), TRUE)

  # advanced mode: a subset of the versions, a non-subset, and the same variable with another specification
  x    <- mtDef("d1", "x", "none")
  z    <- mtDef("d2", "z", "none")
  w    <- mtDef("d3", "w", "none")
  logx <- mtDef("d4", "x", "log")
  one   <- advanced(list(x), index = 0)
  two   <- advanced(list(x, z), index = 1)
  other <- advanced(list(x, w), index = 2)
  only  <- advanced(list(logx), index = 3)
  both  <- advanced(list(x, logx), index = 4)
  expect_equal(nested(one, two), TRUE)
  expect_equal(nested(two, other), FALSE)
  expect_equal(nested(two, one), FALSE)
  expect_equal(nested(one, only), FALSE)
  expect_equal(nested(one, both), TRUE)
  expect_equal(nested(only, both), TRUE)
  expect_equal(nested(both, one), FALSE)
  expect_equal(nested(one, two, advanced(list(x, z, w), index = 2)), c(TRUE, TRUE))
  expect_equal(nested(one, only, both), c(FALSE, TRUE))

  # settings are part of the specification: parameters, reference levels, and the operands of an interaction
  power2  <- advanced(list(mtDef("d5", "x", "power", exponent = "2")), index = 5)
  power2b <- advanced(list(mtDef("d6", "x", "power", exponent = "4/2"), z), index = 6)
  power3  <- advanced(list(mtDef("d7", "x", "power", exponent = "3"), z), index = 7)
  expect_equal(nested(power2, power2b), TRUE)
  expect_equal(nested(power2, power3), FALSE)
  referenceA       <- advanced(list(mtDef("d8", "f", "treatment", referenceLevel = "a")), index = 8)
  referenceDefault <- advanced(list(mtDef("d9", "f", "treatment"), x), index = 9)
  referenceB       <- advanced(list(mtDef("d10", "f", "treatment", referenceLevel = "b"), x), index = 10)
  expect_equal(nested(referenceA, referenceDefault), TRUE)
  expect_equal(nested(referenceA, referenceB), FALSE)
  expect_equal(nested(referenceA, advanced(list(mtDef("d11", "f", "sum")), index = 11)), FALSE)

  versions <- list(x, logx, z)
  product  <- function(operand, extra = list(), index = 0)
    advanced(versions, c(list("d1", "d4", "d2", c(operand, "d2")), extra), index = index)
  expect_equal(nested(product("d1"), product("d1", index = 1)), TRUE)
  expect_equal(nested(product("d1"), product("d4", index = 1)), FALSE)
  expect_equal(nested(product("d1"), product("d1", list(c("d4", "d2")), index = 1)), TRUE)
  expect_equal(nested(product("d1", list(c("d4", "d2"))), product("d1", index = 1)), FALSE)
})

test_that("a restricted term is nested in the same term, free or restricted in the same way", {
  data <- mtData()
  model <- function(restriction, index) {
    mtModel(list(mtDef("d1", "x", "none", restriction = restriction), mtDef("d2", "z", "none")),
            title = paste("Model", index), name = paste0("model", index))
  }
  nested <- function(...) prepareModelTerms(list(...), data)$nested

  expect_equal(nested(model("1", 0), model("", 1)), TRUE)
  expect_equal(nested(model("1", 0), model("1", 1)), TRUE)
  expect_equal(nested(model("1", 0), model(" 1 ", 1)), TRUE)
  expect_equal(nested(model("1", 0), model("2", 1)), FALSE)
  expect_equal(nested(model("", 0), model("1", 1)), FALSE)
  expect_equal(nested(model("", 0), model("", 1)), TRUE)
})

# Restrictions ----------------------------------------------------------------------------------------------------

test_that("restrictions give the fixed coefficient of every column", {
  data  <- mtData()
  model <- mtModel(list(
    mtDef("d1", "x", "none", restriction = "1/2"),
    mtDef("d2", "z", "polynomial", polynomialDegree = 3, restriction = "1, exp(0), 3"),
    mtDef("d3", "f", "treatment", restriction = "0;  -1"),
    mtDef("d4", "w", "none")),
    terms = list("d1", "d2", "d3", "d4", c("d1", "d3")))
  prepared <- prepareModelTerms(list(model), data)
  design   <- mtDesign(prepared, 1, intercept = TRUE)

  # the interaction x:f is free, whatever the restrictions of x and f as main effects
  expect_equal(prepared$models[[1]]$terms$restriction, c("1/2", "1, exp(0), 3", "0;  -1", "", ""))
  fixed <- attr(design, "fixed")
  expect_type(fixed, "double")
  expect_equal(names(fixed), names(design))
  expect_equal(unname(fixed), c(NA, 0.5, 1, 1, 3, 0, -1, NA, NA, NA))
  expect_equal(attr(design, "term"), c(NA, "t1", "t2", "t2", "t2", "t3", "t3", "t4", "t5", "t5"))
  expect_equal(unname(attr(mtDesign(prepared, 1), "fixed")), c(0.5, 1, 1, 3, 0, -1, NA, NA, NA))
})

test_that("a restriction needs one value per column of the term, without recycling", {
  data <- mtData()
  error <- function(restriction, specification = "treatment", variable = "f", ...)
    mtError(prepareModelTerms(list(mtModel(list(mtDef("d1", variable, specification, restriction = restriction, ...)))), data))

  expect_null(error("1, 2"))
  expect_equal(error("1"), "Model 0: the restriction of f needs 2 values (one per coefficient), but 1 were given.")
  expect_equal(error("1, 2, 3"), "Model 0: the restriction of f needs 2 values (one per coefficient), but 3 were given.")
  expect_equal(error("1, 2", "none", "x"), "Model 0: the restriction of x needs 1 value (one per coefficient), but 2 were given.")
  expect_match(error("1", "polynomial", "x", polynomialDegree = 2), "needs 2 values (one per coefficient), but 1 were given.", fixed = TRUE)
  expect_match(error("1,2,3,4", "naturalSpline", "x", splineDf = 3), "needs 3 values (one per coefficient), but 4 were given.", fixed = TRUE)
  expect_match(error("abc"), "Model 0: the restriction 'abc' of f must be a finite number or numbers separated by commas or semicolons.", fixed = TRUE)
  expect_match(error("1, abc"), "must be a finite number or numbers", fixed = TRUE)
  expect_match(error("1; NA"), "must be a finite number or numbers", fixed = TRUE)
  expect_match(error("1, 1/0"), "must be a finite number or numbers", fixed = TRUE)
  expect_match(error("Sys.getenv('HOME'), 1"), "must be a finite number or numbers", fixed = TRUE)
  expect_match(error("1, 2,"), "must be a finite number or numbers", fixed = TRUE)
  expect_match(error("1,,2"), "must be a finite number or numbers", fixed = TRUE)
})

test_that("a restriction applies to a definition as a main effect, interaction terms are free", {
  data <- mtData()

  # a definition that no term uses is not read, so its restriction cannot fail
  model <- mtModel(list(mtDef("d1", "x", "none"), mtDef("d2", "x", "log", restriction = "not a number"), mtDef("d3", "z", "none")),
                   terms = list("d1", "d3"))
  prepared <- prepareModelTerms(list(model), data)
  expect_equal(unname(attr(mtDesign(prepared, 1), "fixed")), c(NA_real_, NA_real_))

  # z only appears in the interaction: its own restriction does not apply there, and neither does x's
  model <- mtModel(list(mtDef("d1", "x", "none", restriction = "3"), mtDef("d2", "z", "none", restriction = "5")),
                   terms = list("d1", c("d1", "d2")))
  prepared <- prepareModelTerms(list(model), data)
  expect_equal(prepared$models[[1]]$terms$restriction, c("3", ""))
  expect_equal(unname(attr(mtDesign(prepared, 1), "fixed")), c(3, NA_real_))
  expect_null(mtError(prepareModelTerms(list(mtModel(list(mtDef("d1", "x", "none", restriction = "1, 2, 3"), mtDef("d2", "z", "none")),
                                                     terms = list(c("d1", "d2")))), data)))
})

# Several models --------------------------------------------------------------------------------------------------

test_that("models are addressed by position or by name and each has its own terms", {
  data     <- mtData()
  prepared <- prepareModelTerms(list(
    mtModel(list(mtDef("d1", "x", "log")), name = "model0", title = "Model 0"),
    mtModel(list(mtDef("d2", "x", "scale"), mtDef("d3", "f", "sum")), name = "model1", title = "Model 1")), data)

  expect_equal(length(prepared$models), 2)
  expect_equal(prepared$nested, FALSE)
  expect_equal(modelTermsFormula(prepared, "model1"), modelTermsFormula(prepared, 2))
  expect_equal(ncol(mtDesign(prepared, 1)), 1)
  expect_equal(ncol(mtDesign(prepared, "model1")), 3)
  expect_equal(attr(mtDesign(prepared, 2), "labels"), c("Scale(x)", "f (a)", "f (b)"))
  expect_error(modelTermsFormula(prepared, 3), "`model` must be the name or the position")
  expect_error(modelTermsFormula(prepared, "nothing"), "`model` must be the name or the position")
  expect_error(modelTermsFormula(list(), 1), "`prepared` must be created by prepareModelTerms()", fixed = TRUE)
  expect_error(prepareModelTerms(list(), "data"), "`dataset` must be a data frame")
})

test_that("a definition used by several models gives the same columns, labels and restriction in each", {
  data <- mtData()
  logx <- mtDef("d1", "x", "log", restriction = "2")
  poly <- mtDef("d2", "z", "polynomial", polynomialDegree = 2)
  coded <- mtDef("d3", "f", "treatment", referenceLevel = "b")
  prepared <- prepareModelTerms(list(
    mtModel(list(logx), name = "model0", title = "Model 0"),
    mtModel(list(logx, poly, coded), terms = list("d1", "d2", "d3", c("d1", "d2")), name = "model1", title = "Model 1"),
    mtModel(list(poly, logx), terms = list(c("d2", "d1")), name = "model2", title = "Model 2")), data)

  first   <- mtDesign(prepared, 1)
  second  <- mtDesign(prepared, 2)
  third   <- mtDesign(prepared, 3)
  expect_equal(first$t1, log(data$x))
  expect_equal(second$t1, first$t1)
  expect_equal(attr(first, "labels"), "log(x)")
  expect_equal(attr(second, "labels"), c("log(x)", "Polynomial(z, 1)", "Polynomial(z, 2)", "f (a)", "f (c)",
                                         paste("log(x)", "Polynomial(z, 1)", sep = mtSeparator), paste("log(x)", "Polynomial(z, 2)", sep = mtSeparator)))

  # the restriction of the definition fixes its main effect in each model that has one; the interaction is free
  expect_equal(unname(attr(first, "fixed")), 2)
  expect_equal(unname(attr(second, "fixed")), c(2, NA, NA, NA, NA, NA, NA))
  expect_equal(unname(attr(third, "fixed")), c(NA_real_, NA_real_))
  expect_equal(attr(third, "labels"), c(paste("Polynomial(z, 1)", "log(x)", sep = mtSeparator), paste("Polynomial(z, 2)", "log(x)", sep = mtSeparator)))

  # the constants are learned once from the fitting data: model 1 contains the polynomial of a model of its own
  alone <- prepareModelTerms(list(mtModel(list(poly))), data)
  expect_equal(mtMatrix(second)[, 2:3], mtMatrix(mtDesign(alone, 1)))
  for (model in 1:3)
    mtExpectSameModel(prepared, model, data)

  # model 0 is nested in model 1 (same term, same restriction); model 1 is not nested in model 2 (the interaction only)
  expect_equal(prepared$nested, c(TRUE, FALSE))

  # a definition that is invalid on the fitting data stops the first model that uses it
  data$x[1] <- -1
  expect_equal(mtError(prepareModelTerms(list(
    mtModel(list(poly), name = "model0", title = "First"), mtModel(list(logx), name = "model1", title = "Second model"),
    mtModel(list(logx), name = "model2", title = "Third model")), data)),
    "Second model: log(x) needs positive values of x; 1 value is zero or negative.")
})

test_that("a model error names the model by its title", {
  data <- mtData()
  data$x[1] <- -1
  models <- list(mtModel(list(mtDef("d1", "z", "log")), name = "model0", title = "First"),
                 mtModel(list(mtDef("d2", "x", "log")), name = "model1", title = "Second model"))
  expect_equal(mtError(prepareModelTerms(models, data)), "Second model: log(x) needs positive values of x; 1 value is zero or negative.")
})

# Input from JSON -------------------------------------------------------------------------------------------------

test_that("the option as the engine reads it from JSON is accepted, including the type copies", {
  data <- mtData()
  json <- '[{
    "name": "model0", "title": "Model 0",
    "components": [["x"], ["z"], ["f"], ["x", "z"]],
    "components.types": [["scale"], ["scale"], ["nominal"], ["scale", "scale"]],
    "intercept": true,
    "definitions": [
      {"id": "d1", "variable": "x", "specification": "none", "restriction": "", "id.types": "unknown"},
      {"id": "d2", "variable": "x", "specification": "log", "restriction": ""},
      {"id": "d3", "variable": "z", "specification": "logBase", "logBase": "10", "logMultiplier": "1", "restriction": ""},
      {"id": "d4", "variable": "f", "specification": "treatment", "referenceLevel": "b", "restriction": ""}],
    "definitions.types": [],
    "terms": [
      {"id": "t1", "operands": ["d1"]}, {"id": "t2", "operands": ["d2"]}, {"id": "t3", "operands": ["d3"]},
      {"id": "t4", "operands": "d4"}, {"id": "t5", "operands": ["d1", "d3"]}, {"id": "t6", "operands": ["d2", "d3"]}],
    "terms.types": []
  },
  {
    "name": "model1", "title": "Model 1",
    "components": [["x"]], "definitions": [{"id": "d2", "variable": "x", "specification": "log", "restriction": ""}],
    "terms": [{"id": "t1", "operands": ["d2"]}]
  }]'
  option   <- jsonlite::fromJSON(json, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  prepared <- prepareModelTerms(option, data)

  expect_true(prepared$advanced)
  expect_equal(prepared$models[[1]]$terms$id, c("t1", "t2", "t3", "t4", "t5", "t6"))
  expect_equal(prepared$models[[2]]$terms$id, "t1")
  expect_equal(prepared$nested, FALSE)
  expect_equal(mtDesign(prepared, 1), mtDesign(prepareModelTerms(list(mtExampleModel()), data), 1))

  # a model that has no terms at all arrives as empty arrays
  empty <- jsonlite::fromJSON('[{"name": "model0", "title": "Model 0", "components": [], "definitions": [], "terms": []}]',
                              simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  expect_true(prepareModelTerms(empty, data)$advanced)
  expect_equal(modelTermsFormula(prepareModelTerms(empty, data), 1, response = "y"), y ~ 1, ignore_formula_env = TRUE)
  expect_equal(prepareModelTerms(NULL, data)$models, list())
})

test_that("nominal variables need complete data", {
  data <- mtData()
  data$f[2] <- NA
  expect_match(mtError(prepareModelTerms(list(mtModel(list(mtDef("d1", "f", "treatment")))), data)),
               "Model 0: the nominal variable 'f' has missing values.", fixed = TRUE)

  prepared <- prepareModelTerms(list(mtModel(list(mtDef("d1", "f", "treatment"), mtDef("d2", "x", "none")))), mtData())
  expect_match(mtError(mtDesign(prepared, 1, data = data.frame(f = c("a", NA), x = 1:2))),
               "the nominal variable 'f' has missing values.", fixed = TRUE)
})

# Intercept -------------------------------------------------------------------------------------------------------

# A model with the main effects of x and a factor f, with the intercept settings of the arguments
mtInterceptModel <- function(..., title = "Model 0", name = "model0") {
  mtModel(list(mtDef("d1", "x", "none"), mtDef("d2", "f", "treatment")), title = title, name = name, ...)
}

test_that("the intercept of a model is TRUE unless it says otherwise, and the caller can override it", {
  data     <- mtData()
  prepared <- prepareModelTerms(list(mtInterceptModel(), mtInterceptModel(intercept = TRUE, name = "model1"),
                                     mtInterceptModel(intercept = FALSE, name = "model2")), data)

  expect_equal(vapply(prepared$models, `[[`, TRUE, "intercept"), c(TRUE, TRUE, FALSE))
  expect_equal(vapply(prepared$models, `[[`, 1, "interceptFixed"), rep(NA_real_, 3))
  interceptOf <- function(formula) attr(stats::terms(formula), "intercept")
  expect_equal(interceptOf(prepared$models[[1]]$formula), 1L)
  expect_equal(interceptOf(prepared$models[[3]]$formula), 0L)

  # the formula
  expect_equal(modelTermsFormula(prepared, 1, response = "y")[[2]], as.name("y"))
  expect_equal(modelTermsFormula(prepared, 1), prepared$models[[1]]$formula, ignore_formula_env = TRUE)
  expect_equal(interceptOf(modelTermsFormula(prepared, 1)), 1L)
  expect_equal(interceptOf(modelTermsFormula(prepared, 2)), 1L)
  expect_equal(interceptOf(modelTermsFormula(prepared, 3)), 0L)
  expect_equal(interceptOf(modelTermsFormula(prepared, 3, intercept = TRUE)), 1L)
  expect_equal(interceptOf(modelTermsFormula(prepared, 1, intercept = FALSE)), 0L)
  expect_equal(modelTermsFormula(prepared, 3, response = "y", intercept = TRUE), modelTermsFormula(prepared, 1, response = "y"), ignore_formula_env = TRUE)
  expect_error(modelTermsFormula(prepared, 1, intercept = NA), "`intercept` must be TRUE, FALSE or NULL")
  expect_error(modelTermsFormula(prepared, 1, intercept = "yes"), "`intercept` must be TRUE, FALSE or NULL")

  # the design: the (Intercept) column of ones comes with the intercept of the model
  expect_equal(names(modelTermsDesign(prepared, 1))[1], "(Intercept)")
  expect_equal(modelTermsDesign(prepared, 1)[["(Intercept)"]], rep(1, nrow(data)))
  expect_equal(attr(modelTermsDesign(prepared, 1), "labels"), c("(Intercept)", "x", "f (b)", "f (c)"))
  expect_equal(attr(modelTermsDesign(prepared, 1), "term"), c(NA, "t1", "t2", "t2"))
  expect_equal(unname(attr(modelTermsDesign(prepared, 1), "fixed")), rep(NA_real_, 4))
  expect_equal(attr(modelTermsDesign(prepared, 3), "labels"), c("x", "f (b)", "f (c)"))
  expect_equal(names(modelTermsDesign(prepared, 3, intercept = TRUE))[1], "(Intercept)")
  expect_equal(names(modelTermsDesign(prepared, 1, intercept = FALSE)), c("t1", "t2.1", "t2.2"))
  expect_equal(modelTermsDesign(prepared, 1, intercept = FALSE), modelTermsDesign(prepared, 3))

  # basic mode has the intercept, exactly as before
  basic <- prepareModelTerms(list(mtBasic(list("x", "f"))), data)
  expect_true(basic$models[[1]]$intercept)
  expect_equal(modelTermsFormula(basic, 1, response = "y"), y ~ x + f, ignore_formula_env = TRUE)
  expect_equal(modelTermsFormula(basic, 1, response = "y", intercept = FALSE), y ~ 0 + x + f, ignore_formula_env = TRUE)
  expect_equal(names(modelTermsDesign(basic, 1))[1], "(Intercept)")
  expect_equal(names(modelTermsDesign(basic, 1, intercept = FALSE))[1], "t1")
  expect_equal(unname(attr(modelTermsDesign(basic, 1), "fixed")), rep(NA_real_, 4))
})

test_that("a model without intercept is the same model in formula mode and in data mode, and nominal variables keep their coding", {
  data     <- mtData()
  prepared <- prepareModelTerms(list(mtInterceptModel(intercept = FALSE)), data)

  fit    <- stats::lm(modelTermsFormula(prepared, 1, response = "y"), data)
  design <- modelTermsDesign(prepared, 1)
  expect_length(stats::coef(fit), 3)
  expect_equal(unname(as.matrix(design)), unname(stats::model.matrix(fit)), ignore_attr = TRUE)
  designFit <- stats::lm.fit(as.matrix(design), data$y)
  expect_equal(unname(designFit$coefficients), unname(stats::coef(fit)))

  # f is coded by two columns, as with an intercept, not by an indicator per level as a plain factor would be
  expect_equal(attr(design, "labels"), c("x", "f (b)", "f (c)"))
  expect_equal(unname(as.matrix(design))[, 2:3], unname(stats::contr.treatment(3)[as.integer(data$f), ]))
  expect_equal(modelTermsLabels(prepared, 1, names(stats::coef(fit))), c("x", "f (b)", "f (c)"))

  # the same columns as the design of the model with an intercept, without the column of ones
  withIntercept <- prepareModelTerms(list(mtInterceptModel()), data)
  expect_equal(unname(as.matrix(design)), unname(as.matrix(modelTermsDesign(withIntercept, 1, intercept = FALSE))))
})

test_that("an intercept restriction fixes the intercept in the design", {
  data <- mtData()
  fixedOf <- function(restriction, ...) {
    prepared <- prepareModelTerms(list(mtInterceptModel(interceptRestriction = restriction, ...)), data)
    list(prepared = prepared, design = modelTermsDesign(prepared, 1, intercept = TRUE))
  }

  free <- fixedOf("")
  expect_equal(unname(attr(free$design, "fixed")), rep(NA_real_, 4))
  expect_equal(free$prepared$models[[1]]$interceptFixed, NA_real_)

  fixed <- fixedOf("5")
  expect_equal(unname(attr(fixed$design, "fixed")), c(5, NA, NA, NA))
  expect_equal(names(attr(fixed$design, "fixed"))[1], "(Intercept)")
  expect_equal(fixed$prepared$models[[1]]$interceptFixed, 5)
  expect_equal(unname(attr(modelTermsDesign(fixed$prepared, 1, intercept = FALSE), "fixed")), rep(NA_real_, 3))

  expect_equal(fixedOf("exp(1)/2")$prepared$models[[1]]$interceptFixed, exp(1) / 2)
  expect_equal(fixedOf(" -0.5 ")$prepared$models[[1]]$interceptFixed, -0.5)
  expect_equal(unname(attr(fixedOf("0")$design, "fixed"))[1], 0)

  # it goes together with the restrictions of the terms
  model <- mtModel(list(mtDef("d1", "x", "none", restriction = "2"), mtDef("d2", "f", "treatment", restriction = "1, 3")),
                   interceptRestriction = "-1")
  prepared <- prepareModelTerms(list(model), data)
  expect_equal(unname(attr(modelTermsDesign(prepared, 1), "fixed")), c(-1, 2, 1, 3))

  # a restriction needs an intercept, so a model without one does not read it
  noIntercept <- fixedOf("not a number", intercept = FALSE)
  expect_equal(noIntercept$prepared$models[[1]]$interceptFixed, NA_real_)
  expect_equal(unname(attr(modelTermsDesign(noIntercept$prepared, 1), "fixed")), rep(NA_real_, 3))
  expect_equal(unname(attr(modelTermsDesign(noIntercept$prepared, 1, intercept = TRUE), "fixed")), rep(NA_real_, 4))

  # a restriction is one number
  expect_match(mtError(fixedOf("abc")), "Model 0: the restriction 'abc' of (Intercept) must be a finite number", fixed = TRUE)
  expect_match(mtError(fixedOf("1, 2")), "Model 0: the restriction of (Intercept) needs 1 value (one per coefficient), but 2 were given.", fixed = TRUE)
  expect_match(mtError(fixedOf("Sys.getenv('HOME')")), "must be a finite number", fixed = TRUE)
})

test_that("the intercept is one more term when models are checked for nesting", {
  data   <- mtData()
  nested <- function(first, second) {
    models <- list(mtInterceptModel(intercept = first[[1]], interceptRestriction = first[[2]], name = "model0"),
                   mtInterceptModel(intercept = second[[1]], interceptRestriction = second[[2]], name = "model1", title = "Model 1"))
    prepareModelTerms(models, data)$nested
  }
  free <- list(TRUE, "")
  none <- list(FALSE, "")
  five <- list(TRUE, "5")

  expect_true(nested(free, free))
  expect_true(nested(five, five))
  expect_true(nested(five, list(TRUE, " 5 ")))
  expect_true(nested(five, free))
  expect_false(nested(free, five))
  expect_false(nested(five, list(TRUE, "3")))
  expect_true(nested(none, none))
  expect_true(nested(none, free))
  expect_false(nested(free, none))
  expect_false(nested(none, five))
  expect_false(nested(five, none))
  expect_true(nested(none, list(TRUE, "0")))
  expect_true(nested(list(TRUE, "0"), none))
  expect_true(nested(list(TRUE, "2.5"), list(TRUE, "5/2")))

  # the terms still count
  x     <- mtDef("d1", "x", "none")
  one   <- mtModel(list(x), intercept = FALSE, name = "model0")
  two   <- mtModel(list(x, mtDef("d2", "z", "none")), intercept = TRUE, name = "model1", title = "Model 1")
  three <- mtModel(list(x, mtDef("d3", "w", "none")), intercept = TRUE, name = "model2", title = "Model 2")
  expect_equal(prepareModelTerms(list(one, two, three), data)$nested, c(TRUE, FALSE))
})

test_that("the intercept settings are read from JSON", {
  data <- mtData()
  json <- '[{"name": "model0", "title": "Model 0", "components": [["x"]], "intercept": false, "interceptRestriction": "3",
             "definitions": [{"id": "d1", "variable": "x", "specification": "none", "restriction": ""}],
             "terms": [{"id": "t1", "operands": ["d1"]}]},
            {"name": "model1", "title": "Model 1", "components": [["x"]], "intercept": true, "interceptRestriction": "3",
             "definitions": [{"id": "d1", "variable": "x", "specification": "none", "restriction": ""}],
             "terms": [{"id": "t1", "operands": ["d1"]}]},
            {"name": "model2", "title": "Model 2", "components": [["x"]],
             "definitions": [{"id": "d1", "variable": "x", "specification": "none", "restriction": ""}],
             "terms": [{"id": "t1", "operands": ["d1"]}]}]'
  prepared <- prepareModelTerms(jsonlite::fromJSON(json, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE), data)

  expect_equal(vapply(prepared$models, `[[`, TRUE, "intercept"), c(FALSE, TRUE, TRUE))
  expect_equal(vapply(prepared$models, `[[`, 1, "interceptFixed"), c(NA, 3, NA))
  expect_equal(names(modelTermsDesign(prepared, 1)), "t1")
  expect_equal(names(modelTermsDesign(prepared, 2)), c("(Intercept)", "t1"))
  expect_equal(unname(attr(modelTermsDesign(prepared, 2), "fixed")), c(3, NA))
})

test_that("two definitions that build the same term are refused, not merged by R", {
  model <- mtModel(list(mtDef("d1", "x", "power", exponent = "2"), mtDef("d2", "x", "root", rootDegree = "0.5"), mtDef("d3", "z", "none")))
  expect_match(mtError(prepareModelTerms(list(model), mtData())), "Model 0: two term definitions give the same term", fixed = TRUE)

  # also when they meet only inside an interaction: R would reduce x^2:x^2 to x^2
  interaction <- mtModel(list(mtDef("d1", "x", "power", exponent = "2"), mtDef("d2", "x", "root", rootDegree = "0.5")), terms = list(c("d1", "d2")))
  expect_match(mtError(prepareModelTerms(list(interaction), mtData())), "Model 0: two term definitions give the same term", fixed = TRUE)
})

test_that("variable names and level labels are taken as they are, also with spaces around them", {
  data <- mtData()
  data[[" x"]] <- data$x + 1
  levels(data$f) <- c(" a", "a", "c")

  prepared <- prepareModelTerms(list(mtModel(list(mtDef("d1", " x", "none"), mtDef("d2", "f", "treatment", referenceLevel = "a")))), data)
  expect_equal(mtByLabel(prepared)[[" x"]], data[[" x"]])
  expect_equal(prepared$models[[1]]$terms$label, c(" x", "f (reference a)"))   # the first level is " a"
  expect_equal(attr(mtDesign(prepared), "labels"), c(" x", "f ( a)", "f (c)"))
})

# Offsets ---------------------------------------------------------------------------------------------------------

# The offset of one continuous variable: the variable itself with the coefficient fixed at 1
mtOffset <- function(id, variable, ...) mtDef(id, variable, "offset", ...)

# A constant offset: a number and no variable
mtConstant <- function(id, value, ...) {
  definition <- list(id = id, specification = "offsetConstant", value = value)
  extra      <- list(...)
  definition[names(extra)] <- extra
  definition
}

# lm() on the formula of the model, where lm() takes the constant offset as an argument
mtLm <- function(prepared, model, data, response = "y", ...) {
  stats::lm(modelTermsFormula(prepared, model, response = response), data,
            offset = rep(modelTermsConstantOffset(prepared, model), nrow(data)), ...)
}

# Counts that depend on z (an exposure, log(z) is its own column) and x
mtCountData <- function() {
  data <- mtData()
  set.seed(3)
  data$count <- stats::rpois(nrow(data), 3 * data$z * exp(0.1 * data$x))
  data$logz  <- log(data$z)
  data
}

test_that("the offset of a variable enters the formula as offset(x) and the fit equals lm() with that offset", {
  data <- mtData()
  fitOf <- function(definitions, terms = NULL) {
    prepared <- prepareModelTerms(list(mtModel(definitions, terms)), data)
    list(prepared = prepared, fit = stats::lm(modelTermsFormula(prepared, 1, response = "y"), data))
  }
  x <- mtDef("d1", "x", "none")
  f <- mtDef("d2", "f", "treatment")

  plain <- fitOf(list(x, f, mtOffset("d3", "z")))
  expect_equal(unname(stats::coef(plain$fit)), unname(stats::coef(stats::lm(y ~ x + f + offset(z), data))))
  expect_equal(unname(plain$fit$offset), data$z)
  expect_false(isTRUE(all.equal(unname(stats::coef(plain$fit)), unname(stats::coef(stats::lm(y ~ x + f, data))))))
  mtExpectSameModel(plain$prepared, 1, data)

  # the same column reference as the other terms: the formula calls offset() on the variable, in the order of the model
  several <- prepareModelTerms(list(mtModel(list(x, mtOffset("d2", "z"), mtOffset("d3", "w")))), data)
  expect_equal(modelTermsFormula(several, 1, response = "y"), y ~ x + offset(z) + offset(w), ignore_formula_env = TRUE)
  expect_equal(modelTermsFormula(several, 1, response = "y", intercept = FALSE), y ~ 0 + x + offset(z) + offset(w), ignore_formula_env = TRUE)
  expect_equal(modelTermsFormula(several, 1), ~ x + offset(z) + offset(w), ignore_formula_env = TRUE)
  first <- prepareModelTerms(list(mtModel(list(mtOffset("d2", "z"), x))), data)
  expect_equal(modelTermsFormula(first, 1, response = "y"), y ~ offset(z) + x, ignore_formula_env = TRUE)
  mtExpectSameModel(several, 1, data)
  mtExpectSameModel(first, 1, data)

  # a variable can be a term and an offset of the same model
  both <- fitOf(list(x, mtOffset("d2", "x")))
  expect_equal(unname(stats::coef(both$fit)), unname(stats::coef(stats::lm(y ~ x + offset(x), data))))
  mtExpectSameModel(both$prepared, 1, data)

  # names that are not syntactic work like the names of the other terms
  odd <- data
  names(odd)[names(odd) == "z"] <- "my var"
  prepared <- prepareModelTerms(list(mtModel(list(x, mtOffset("d2", "my var")))), odd)
  expect_equal(unname(stats::coef(stats::lm(modelTermsFormula(prepared, 1, response = "y"), odd))),
               unname(stats::coef(stats::lm(y ~ x + offset(`my var`), odd))))
  expect_equal(prepared$models[[1]]$terms$expression, c("x", "offset(`my var`)"))
  expect_equal(prepared$models[[1]]$terms$label, c("x", "offset(my var)"))
})

test_that("a constant offset is not in the formula: lm() takes it as offset = rep(constant, n)", {
  data <- mtData()
  x    <- mtDef("d1", "x", "none")
  f    <- mtDef("d2", "f", "treatment")
  prepared <- prepareModelTerms(list(mtModel(list(x, f, mtConstant("d3", 2)))), data)

  # R never sees the constant: not in the formula (with or without response and intercept), nor in the terms of the model
  expect_equal(all.vars(modelTermsFormula(prepared, 1, response = "y")), c("y", "x", "f"))
  expect_false(grepl("offset", paste(deparse(modelTermsFormula(prepared, 1)), collapse = "")))
  expect_false(grepl("offset", paste(deparse(prepared$models[[1]]$formula), collapse = "")))
  expect_equal(modelTermsConstantOffset(prepared, 1), 2)
  expect_equal(prepared$models[[1]]$terms$label, c("x", "f", "offset(2)"))
  expect_equal(prepared$models[[1]]$terms$offset, c(FALSE, FALSE, TRUE))
  expect_equal(prepared$models[[1]]$terms$type, c("main", "main", "main"))

  # the fit is lm() with the constant as offset; without intercept the constant matters, with one it moves the intercept
  fit <- mtLm(prepared, 1, data)
  expect_equal(unname(fit$offset), rep(2, nrow(data)))
  reference <- stats::lm(y ~ x + f, data, offset = rep(2, nrow(data)))
  expect_equal(unname(stats::coef(fit)), unname(stats::coef(reference)))
  expect_equal(unname(stats::fitted(fit)), unname(stats::fitted(reference)))
  plain <- stats::lm(y ~ x + f, data)
  expect_equal(unname(stats::coef(fit)[-1]), unname(stats::coef(plain)[-1]))
  expect_equal(unname(stats::coef(fit)[1]), unname(stats::coef(plain)[1]) - 2)
  noIntercept <- stats::lm(modelTermsFormula(prepared, 1, response = "y", intercept = FALSE), data, offset = rep(2, nrow(data)))
  designFit   <- stats::lm.fit(as.matrix(mtDesign(prepared, 1)), data$y, offset = rep(2, nrow(data)))
  expect_equal(unname(stats::coef(noIntercept)), unname(designFit$coefficients))
  expect_false(isTRUE(all.equal(unname(stats::coef(noIntercept)), unname(stats::coef(stats::lm(modelTermsFormula(prepared, 1, response = "y", intercept = FALSE), data))))))
  mtExpectSameModel(prepared, 1, data)

  # several constants add up
  several <- prepareModelTerms(list(mtModel(list(x, mtConstant("d2", 2), mtConstant("d3", -0.5), mtConstant("d4", 1e-3)))), data)
  expect_equal(modelTermsConstantOffset(several, 1), 2 - 0.5 + 1e-3)
  expect_equal(unname(stats::coef(mtLm(several, 1, data))), unname(stats::coef(stats::lm(y ~ x, data, offset = rep(1.501, nrow(data))))))
  expect_equal(several$models[[1]]$terms$label, c("x", "offset(2)", "offset(-0.5)", "offset(0.001)"))
  mtExpectSameModel(several, 1, data)

  # a model of a constant only: no term in the formula, no column but the intercept
  only <- prepareModelTerms(list(mtModel(list(mtConstant("d1", 3)))), data)
  expect_equal(modelTermsFormula(only, 1, response = "y"), y ~ 1, ignore_formula_env = TRUE)
  expect_equal(modelTermsFormula(only, 1, response = "y", intercept = FALSE), y ~ 0, ignore_formula_env = TRUE)
  expect_equal(names(modelTermsDesign(only, 1)), "(Intercept)")
  expect_equal(modelTermsConstantOffset(only, 1), 3)
  mtExpectSameModel(only, 1, data)
})

test_that("both kinds of offset together: the variable in the formula, the constant as the argument of lm()", {
  data <- mtData()
  x    <- mtDef("d1", "x", "none")
  f    <- mtDef("d2", "f", "treatment")
  prepared <- prepareModelTerms(list(mtModel(list(x, mtOffset("d3", "z"), mtConstant("d4", 1.5), f, mtOffset("d5", "w"), mtConstant("d6", 2)))), data)

  expect_equal(all.vars(modelTermsFormula(prepared, 1, response = "y")), c("y", "x", "z", "f", "w"))
  expect_equal(modelTermsConstantOffset(prepared, 1), 3.5)
  fit <- mtLm(prepared, 1, data)
  expect_equal(unname(stats::coef(fit)), unname(stats::coef(stats::lm(y ~ x + f, data, offset = z + w + 3.5))))
  expect_equal(unname(stats::fitted(fit)), unname(stats::fitted(stats::lm(y ~ x + f + offset(z) + offset(w), data, offset = rep(3.5, nrow(data))))))
  expect_equal(modelTermsOffset(prepared, 1), data$z + data$w + 3.5)
  mtExpectSameModel(prepared, 1, data)

  # the order of the terms in the table: offsets last, the others by degree; the constants are offsets, too
  terms <- prepared$models[[1]]$terms
  expect_equal(terms$label, c("x", "f", "offset(z)", "offset(1.5)", "offset(w)", "offset(2)"))
  expect_equal(terms$offset, c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE))
})

test_that("glm() with the offset of a variable and a constant equals the fit on the design and the offset vector", {
  data     <- mtCountData()
  prepared <- prepareModelTerms(list(mtModel(list(mtDef("d1", "x", "none"), mtOffset("d2", "logz"), mtConstant("d3", 0.25)))), data)

  fit <- stats::glm(modelTermsFormula(prepared, 1, response = "count"), data, family = stats::poisson(),
                    offset = rep(modelTermsConstantOffset(prepared, 1), nrow(data)))
  expect_equal(unname(stats::coef(fit)), unname(stats::coef(stats::glm(count ~ x + offset(logz), data, family = stats::poisson(), offset = rep(0.25, nrow(data))))))

  # an engine that takes a matrix: the design without a column for the offsets, and the offset as a vector
  design    <- as.matrix(mtDesign(prepared, 1, intercept = TRUE))
  offset    <- modelTermsOffset(prepared, 1)
  expect_equal(offset, data$logz + 0.25)
  designFit <- stats::glm.fit(design, data$count, offset = offset, family = stats::poisson())
  expect_equal(unname(designFit$coefficients), unname(stats::coef(fit)))
  expect_equal(unname(drop(design %*% stats::coef(fit)) + offset), unname(fit$linear.predictors))

  # an exposure: the coefficient of the intercept is the log rate when the offset is the log of the exposure
  rate <- prepareModelTerms(list(mtModel(list(mtOffset("d1", "logz")))), data)
  expect_equal(unname(stats::coef(stats::glm(modelTermsFormula(rate, 1, response = "count"), data, family = stats::poisson()))),
               unname(stats::coef(stats::glm(count ~ offset(logz), data, family = stats::poisson()))))
})

test_that("an offset has no design column and no coefficient: design %*% coef + offset is fitted()", {
  data     <- mtData()
  prepared <- prepareModelTerms(list(mtModel(list(mtDef("d1", "x", "none"), mtDef("d2", "f", "treatment"), mtOffset("d3", "z"),
                                                  mtOffset("d4", "w"), mtConstant("d5", 4)))), data)
  fit      <- mtExpectSameModel(prepared, 1, data)
  design   <- mtDesign(prepared, 1, intercept = TRUE)

  expect_equal(attr(design, "labels"), c("(Intercept)", "x", "f (b)", "f (c)"))
  expect_equal(attr(design, "term"), c(NA, "t1", "t2", "t2"))
  expect_equal(names(design), c("(Intercept)", "t1", "t2.1", "t2.2"))
  expect_equal(names(attr(design, "fixed")), names(design))
  expect_length(stats::coef(fit), 4)
  expect_equal(modelTermsLabels(prepared, 1, names(stats::coef(fit))), c("(Intercept)", "x", "f (b)", "f (c)"))

  offset <- modelTermsOffset(prepared, 1)
  expect_equal(offset, data$z + data$w + 4)
  expect_equal(unname(drop(as.matrix(design) %*% stats::coef(fit)) + offset), unname(stats::fitted(fit)))
  expect_equal(unname(stats::fitted(fit)), unname(stats::fitted(stats::lm(y ~ x + f + offset(z + w), data, offset = rep(4, nrow(data))))))

  # without the intercept column
  expect_equal(names(mtDesign(prepared, 1)), c("t1", "t2.1", "t2.2"))

  # a model of offsets only: the intercept (if any) is the only column, the offsets are all there is besides
  only <- prepareModelTerms(list(mtModel(list(mtOffset("d1", "x"), mtOffset("d2", "z"), mtConstant("d3", -1)))), data)
  expect_equal(names(modelTermsDesign(only, 1)), "(Intercept)")
  expect_equal(dim(mtDesign(only, 1)), c(nrow(data), 0L))
  expect_equal(dim(modelTermsDesign(only, 1, intercept = FALSE)), c(nrow(data), 0L))
  expect_equal(modelTermsOffset(only, 1), data$x + data$z - 1)
  expect_equal(modelTermsFormula(only, 1, response = "y"), y ~ offset(x) + offset(z), ignore_formula_env = TRUE)
  expect_equal(modelTermsFormula(only, 1, response = "y", intercept = FALSE), y ~ 0 + offset(x) + offset(z), ignore_formula_env = TRUE)
  expect_equal(unname(stats::coef(mtLm(only, 1, data))), unname(stats::coef(stats::lm(y ~ offset(x + z - 1), data))))
  mtExpectSameModel(only, 1, data)
  expect_equal(modelTermsLabels(only, 1, "(Intercept)"), "(Intercept)")
})

test_that("modelTermsOffset() gives the offset vector for the fitting data and for new data", {
  data   <- mtData()
  data$k <- seq_len(nrow(data))
  new    <- data.frame(x = c(1.5, 2, 9), z = c(2, 3, 4), w = c(-1, 0, 1.5), k = c(3L, 4L, 5L), f = c("a", "c", "b"))

  # a nominal variable in the model does not matter for the offset
  offsetOf <- function(definitions) {
    prepared <- prepareModelTerms(list(mtModel(c(list(mtDef("d0", "f", "treatment")), definitions))), data)
    list(fitting = modelTermsOffset(prepared, 1), new = modelTermsOffset(prepared, 1, data = new))
  }

  none <- offsetOf(list(mtOffset("d1", "z")))
  expect_equal(none$fitting, data$z)
  expect_equal(none$new, new$z)
  expect_type(none$fitting, "double")
  expect_null(attributes(none$fitting))

  # a constant is repeated for every row, of the fitting data and of new data
  constant <- offsetOf(list(mtConstant("d1", 3)))
  expect_equal(constant$fitting, rep(3, nrow(data)))
  expect_equal(constant$new, rep(3, 3))
  expect_type(constant$new, "double")
  expect_equal(modelTermsOffset(prepareModelTerms(list(mtModel(list(mtConstant("d1", 3)))), data), 1, data = data.frame(q = 1:5)), rep(3, 5))

  # several offsets add up: negative values, an integer variable, constants
  several <- offsetOf(list(mtOffset("d1", "z"), mtConstant("d2", 2), mtOffset("d3", "w"), mtOffset("d4", "k"), mtConstant("d5", -0.25)))
  expect_equal(several$fitting, data$z + data$w + data$k + 1.75)
  expect_equal(several$new, new$z + new$w + new$k + 1.75)
  expect_type(several$fitting, "double")

  # the values are taken as they are, also when they are missing
  missing <- data
  missing$z[2] <- NA
  expect_equal(modelTermsOffset(prepareModelTerms(list(mtModel(list(mtOffset("d1", "z"), mtConstant("d2", 1)))), missing), 1), missing$z + 1)
  prepared <- prepareModelTerms(list(mtModel(list(mtOffset("d1", "z")))), data)
  expect_equal(modelTermsOffset(prepared, 1, data = data.frame(z = c(NA, 2))), c(NA, 2))
  expect_equal(modelTermsOffset(prepared, 1, data = data.frame(z = c(-3, 0, Inf))), c(-3, 0, Inf))

  # the model is addressed by position or name, and each model has its own offset
  prepared <- prepareModelTerms(list(mtModel(list(mtOffset("d1", "z"))),
                                     mtModel(list(mtOffset("d2", "x"), mtConstant("d3", 1)), name = "model1", title = "Model 1"),
                                     mtModel(list(mtDef("d4", "x", "none")), name = "model2", title = "Model 2")), data)
  expect_equal(modelTermsOffset(prepared, "model1"), data$x + 1)
  expect_equal(modelTermsOffset(prepared, 1), data$z)

  # no offset: NULL, in advanced and in basic mode
  expect_null(modelTermsOffset(prepared, 3))
  expect_null(modelTermsOffset(prepared, "model2", data = new))
  expect_null(modelTermsOffset(prepareModelTerms(list(mtBasic(list("x", "z"))), data), 1))
  expect_null(modelTermsOffset(prepareModelTerms(list(mtModel(list())), data), 1))

  # new data: the formula of the fit gives the same linear predictor as the design and the offset
  model    <- mtModel(list(mtDef("d1", "x", "scale"), mtDef("d2", "f", "treatment"), mtOffset("d3", "z"), mtOffset("d4", "w"), mtConstant("d5", 2)))
  prepared <- prepareModelTerms(list(model), data)
  fit      <- mtLm(prepared, 1, data)
  linear   <- drop(as.matrix(mtDesign(prepared, 1, data = new, intercept = TRUE)) %*% stats::coef(fit)) + modelTermsOffset(prepared, 1, data = new)
  terms    <- stats::delete.response(stats::terms(modelTermsFormula(prepared, 1)))
  frame    <- stats::model.frame(terms, new)
  expect_equal(unname(linear), unname(drop(stats::model.matrix(terms, frame) %*% stats::coef(fit)) + stats::model.offset(frame) + modelTermsConstantOffset(prepared, 1)))

  expect_error(modelTermsOffset(prepared, 2), "`model` must be the name or the position")
  expect_error(modelTermsOffset(list(), 1), "`prepared` must be created by prepareModelTerms()", fixed = TRUE)
  expect_match(mtError(modelTermsOffset(prepared, 1, data = data.frame(z = 1))), "Model 0: the data has no variable", fixed = TRUE)
  expect_match(mtError(modelTermsOffset(prepared, 1, data = "new")), "`data` must be a data frame", fixed = TRUE)
  expect_match(mtError(modelTermsOffset(prepared, 1, data = replace(new, "z", list(c("a", "b", "c"))))), "Model 0: the variable 'z' must be numeric.", fixed = TRUE)
})

test_that("modelTermsConstantOffset() is the sum of the constant offsets of the model, 0 without", {
  data     <- mtData()
  prepared <- prepareModelTerms(list(
    mtModel(list(mtDef("d1", "x", "none"), mtConstant("d2", 2), mtOffset("d3", "z"), mtConstant("d4", 0.5))),
    mtModel(list(mtOffset("d5", "z")), name = "model1", title = "Model 1"),
    mtModel(list(mtDef("d6", "x", "none")), name = "model2", title = "Model 2"),
    mtModel(list(mtConstant("d7", -1e-8)), name = "model3", title = "Model 3"),
    mtModel(list(), name = "model4", title = "Model 4")), data)

  expect_equal(modelTermsConstantOffset(prepared, 1), 2.5)
  expect_equal(modelTermsConstantOffset(prepared, "model3"), -1e-8)
  expect_identical(modelTermsConstantOffset(prepared, 2), 0)
  expect_identical(modelTermsConstantOffset(prepared, "model2"), 0)
  expect_identical(modelTermsConstantOffset(prepared, 5), 0)
  expect_type(modelTermsConstantOffset(prepared, 1), "double")
  expect_length(modelTermsConstantOffset(prepared, 1), 1L)
  expect_identical(modelTermsConstantOffset(prepareModelTerms(list(mtBasic(list("x", "z"))), data), 1), 0)

  # only the constants of the terms of the model count
  unused <- mtModel(list(mtDef("d1", "x", "none"), mtConstant("d2", 7)), terms = list("d1"))
  expect_identical(modelTermsConstantOffset(prepareModelTerms(list(unused), data), 1), 0)

  expect_error(modelTermsConstantOffset(prepared, 6), "`model` must be the name or the position")
  expect_error(modelTermsConstantOffset(list(), 1), "`prepared` must be created by prepareModelTerms()", fixed = TRUE)
})

test_that("the terms information marks an offset: no columns, listed after the terms of lm()", {
  data  <- mtData()
  # the offsets come first and in between in the model, the interaction before its main effects
  model <- mtModel(list(mtDef("d1", "x", "none"), mtDef("d2", "w", "none"), mtOffset("d3", "z"), mtOffset("d4", "x"), mtDef("d5", "f", "sum"),
                        mtConstant("d6", 2)),
                   terms = list("d3", c("d1", "d2"), "d6", "d1", "d4", "d2", "d5"))
  prepared <- prepareModelTerms(list(model), data)
  terms    <- prepared$models[[1]]$terms

  expect_equal(terms$id, c("t4", "t6", "t7", "t2", "t1", "t3", "t5"))
  expect_equal(terms$offset, c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE))
  expect_equal(terms$label[5:7], c("offset(z)", "offset(2)", "offset(x)"))
  expect_equal(terms$expression[5:7], c("offset(z)", "offset(2)", "offset(x)"))
  expect_equal(terms$type[5:7], c("main", "main", "main"))
  expect_equal(terms$restriction[5:7], c("", "", ""))
  expect_type(terms$offset, "logical")
  expect_equal(names(terms), c("id", "type", "label", "expression", "restriction", "offset"))

  # R takes the offsets of variables for offsets, not for terms, and does not see the constant
  info <- stats::terms(modelTermsFormula(prepared, 1))
  expect_length(attr(info, "offset"), 2L)
  expect_length(attr(info, "term.labels"), 4L)

  # the columns of the other terms keep their terms
  design <- mtDesign(prepared, 1, intercept = TRUE)
  expect_equal(attr(design, "term"), c(NA, "t4", "t6", "t7", "t7", "t2"))
  expect_equal(attr(design, "labels"), c("(Intercept)", "x", "w", "f (a)", "f (b)", paste("x", "w", sep = mtSeparator)))
  fit <- mtExpectSameModel(prepared, 1, data)
  expect_equal(modelTermsLabels(prepared, 1, names(stats::coef(fit))), attr(design, "labels"))
  reference <- stats::lm(y ~ x + w + f + x:w, data, offset = z + x + 2, contrasts = list(f = "contr.sum"))
  expect_equal(unname(stats::coef(fit)), unname(stats::coef(reference)))

  # basic mode has the column, and it is FALSE
  basic <- prepareModelTerms(list(mtBasic(list("x", "f", c("x", "f")))), data)
  expect_equal(basic$models[[1]]$terms$offset, c(FALSE, FALSE, FALSE))
  expect_equal(prepareModelTerms(list(mtBasic(list())), data)$models[[1]]$terms$offset, logical(0))
  expect_equal(prepareModelTerms(list(mtModel(list())), data)$models[[1]]$terms$offset, logical(0))
})

test_that("an offset counts as a term when models are checked for nesting", {
  data   <- mtData()
  nested <- function(...) prepareModelTerms(list(...), data)$nested
  model  <- function(definitions, terms = NULL, index = 0, ...)
    mtModel(definitions, terms, title = paste("Model", index), name = paste0("model", index), ...)
  x       <- mtDef("d1", "x", "none")
  w       <- mtDef("d2", "w", "none")
  offsetZ <- mtOffset("d3", "z")
  offsetX <- mtOffset("d5", "x")
  two     <- mtConstant("d6", 2)

  # a model with an offset that the other lacks is not nested, in either direction
  expect_false(nested(model(list(x)), model(list(x, offsetZ), index = 1)))
  expect_false(nested(model(list(x, offsetZ)), model(list(x), index = 1)))
  expect_false(nested(model(list()), model(list(offsetZ), index = 1)))
  expect_false(nested(model(list(x, w)), model(list(x, w, offsetZ), index = 1)))
  expect_false(nested(model(list(x)), model(list(x, two), index = 1)))
  expect_false(nested(model(list(x, two)), model(list(x), index = 1)))

  # the same offset and the usual rule for the other terms
  expect_true(nested(model(list(x, offsetZ)), model(list(x, w, offsetZ), index = 1)))
  expect_true(nested(model(list(x, offsetZ)), model(list(x, offsetZ), index = 1)))
  expect_true(nested(model(list(offsetZ)), model(list(offsetZ, x), index = 1)))
  expect_true(nested(model(list(offsetZ)), model(list(offsetZ), index = 1)))
  expect_false(nested(model(list(x, w, offsetZ)), model(list(x, offsetZ), index = 1)))
  expect_false(nested(model(list(x, offsetZ)), model(list(w, offsetZ), index = 1)))

  # another variable is another offset
  expect_false(nested(model(list(x, offsetZ)), model(list(x, offsetX), index = 1)))

  # constants are compared by value, and several offsets are a set, in any order
  expect_true(nested(model(list(x, two)), model(list(x, w, mtConstant("d7", 2)), index = 1)))
  expect_true(nested(model(list(x, two)), model(list(x, w, mtConstant("d7", 2L)), index = 1)))
  expect_false(nested(model(list(x, two)), model(list(x, w, mtConstant("d7", 3)), index = 1)))
  expect_false(nested(model(list(x, two)), model(list(x, w, mtConstant("d7", 2.0000001)), index = 1)))
  expect_false(nested(model(list(x, two)), model(list(x, w, mtConstant("d7", -2)), index = 1)))
  expect_true(nested(model(list(offsetZ, offsetX)), model(list(offsetX, offsetZ, w), index = 1)))
  expect_false(nested(model(list(offsetZ, offsetX)), model(list(offsetZ, w), index = 1)))
  expect_false(nested(model(list(offsetZ)), model(list(offsetZ, offsetX), index = 1)))
  expect_true(nested(model(list(two, mtConstant("d7", 3), offsetZ)), model(list(mtConstant("d8", 3), offsetZ, mtConstant("d9", 2), w), index = 1)))
  expect_false(nested(model(list(two, mtConstant("d7", 3))), model(list(mtConstant("d8", 3), mtConstant("d9", 5)), index = 1)))
  expect_false(nested(model(list(two)), model(list(two, mtConstant("d7", 3)), index = 1)))

  # a term, the offset of its variable and a constant are different terms
  expect_false(nested(model(list(x)), model(list(offsetX), index = 1)))
  expect_false(nested(model(list(offsetX)), model(list(x), index = 1)))
  expect_false(nested(model(list(offsetX)), model(list(mtConstant("d7", 1)), index = 1)))

  # consecutive pairs
  expect_equal(nested(model(list(x)), model(list(x, w), index = 1), model(list(x, w, offsetZ), index = 2)), c(TRUE, FALSE))
  expect_equal(nested(model(list(x, offsetZ)), model(list(x, w, offsetZ), index = 1), model(list(x, w, offsetZ, two), index = 2)), c(TRUE, FALSE))
  expect_equal(nested(model(list(x, offsetZ, two)), model(list(x, w, offsetZ, two), index = 1), model(list(two, offsetZ, x, w), index = 2)), c(TRUE, TRUE))

  # restrictions and the intercept still count
  restricted <- mtDef("d6", "x", "none", restriction = "1")
  expect_true(nested(model(list(restricted, offsetZ)), model(list(x, offsetZ), index = 1)))
  expect_false(nested(model(list(x, offsetZ)), model(list(restricted, offsetZ), index = 1)))
  expect_true(nested(model(list(x, offsetZ), intercept = FALSE), model(list(x, w, offsetZ), index = 1)))
  expect_false(nested(model(list(x, offsetZ)), model(list(x, w, offsetZ), index = 1, intercept = FALSE)))
})

test_that("invalid offsets stop with a readable message that names the model", {
  data <- mtData()
  expectError <- function(model, message) {
    expect_equal(mtError(prepareModelTerms(list(model), data)), message)
    expect_s3_class(tryCatch(prepareModelTerms(list(model), data), error = identity), "validationError")
  }
  x <- mtDef("d1", "x", "none")

  # an offset is a main effect: no operand of an interaction, wherever it stands
  expectError(mtModel(list(x, mtOffset("d2", "z")), list("d1", c("d1", "d2"))),
              "Model 0: the term 't2' is an interaction with offset(z); an offset can only be a main effect.")
  expectError(mtModel(list(x, mtOffset("d2", "z")), list("d2", "d1", c("d2", "d1"))),
              "Model 0: the term 't3' is an interaction with offset(z); an offset can only be a main effect.")
  expectError(mtModel(list(mtOffset("d1", "x"), mtOffset("d2", "z")), list(c("d1", "d2"))),
              "Model 0: the term 't1' is an interaction with offset(x); an offset can only be a main effect.")
  expectError(mtModel(list(x, mtConstant("d2", 2)), list("d1", c("d1", "d2"))),
              "Model 0: the term 't2' is an interaction with offset(2); an offset can only be a main effect.")
  expectError(mtModel(list(mtConstant("d1", 2.5), mtConstant("d2", 3)), list(c("d1", "d2"))),
              "Model 0: the term 't1' is an interaction with offset(2.5); an offset can only be a main effect.")
  expect_match(mtError(prepareModelTerms(list(mtModel(list(x, mtOffset("d2", "z")), list(c("d2", "d1")), title = "My model")), data)),
               "^My model: the term 't1' is an interaction with offset")

  # an offset has no coefficient to restrict; a restriction of a definition that no term uses is not read
  expectError(mtModel(list(x, mtOffset("d2", "z", restriction = "1"))), "Model 0: offset(z) has no coefficient, so it cannot have a restriction.")
  expectError(mtModel(list(x, mtConstant("d2", 3, restriction = "0"))), "Model 0: offset(3) has no coefficient, so it cannot have a restriction.")
  expect_null(mtError(prepareModelTerms(list(mtModel(list(x, mtOffset("d2", "z", restriction = "  "), mtConstant("d3", 1, restriction = " ")))), data)))
  expect_null(mtError(prepareModelTerms(list(mtModel(list(x, mtOffset("d2", "z", restriction = "1")), terms = list("d1"))), data)))

  # only continuous variables have an offset
  expectError(mtModel(list(x, mtOffset("d2", "f"))), "Model 0: the offset of 'f' needs a continuous variable, but 'f' is nominal.")
  expectError(mtModel(list(mtOffset("d2", "g"))), "Model 0: the offset of 'g' needs a continuous variable, but 'g' is nominal.")
  logical <- data
  logical$b <- logical$y > 0
  expect_equal(mtError(prepareModelTerms(list(mtModel(list(mtOffset("d1", "b")))), logical)),
               "Model 0: the offset of 'b' needs a continuous variable, but 'b' is nominal.")
  text <- data
  text$s <- as.character(text$f)
  expect_match(mtError(prepareModelTerms(list(mtModel(list(mtOffset("d1", "s")))), text)), "needs a continuous variable", fixed = TRUE)
  expect_match(mtError(prepareModelTerms(list(mtModel(list(mtOffset("d1", "f")), title = "Second model")), data)), "^Second model: the offset of 'f'")

  # the value of a constant offset is one finite number
  for (value in list("a", "2", NA, NA_real_, Inf, -Inf, NaN, TRUE, c(1, 2), list(1, 2), "", character(0)))
    expect_match(mtError(prepareModelTerms(list(mtModel(list(x, mtConstant("d2", value)))), data)),
                 "^Model 0: the value of a constant offset must be a finite number, not '", info = deparse(value))
  expectError(mtModel(list(x, mtConstant("d2", "a"))), "Model 0: the value of a constant offset must be a finite number, not 'a'.")
  expectError(mtModel(list(x, mtConstant("d2", Inf))), "Model 0: the value of a constant offset must be a finite number, not 'Inf'.")
  expectError(mtModel(list(x, mtConstant("d2", NA_real_))), "Model 0: the value of a constant offset must be a finite number, not 'NA'.")
  expectError(mtModel(list(x, mtConstant("d2", c(1, 2)))), "Model 0: the value of a constant offset must be a finite number, not '1, 2'.")
  absent <- mtModel(list(x, mtConstant("d2", 1)))
  absent$definitions[[2]]$value <- NULL
  expectError(absent, "Model 0: the value of a constant offset must be a finite number, not ''.")
  expect_match(mtError(prepareModelTerms(list(mtModel(list(mtConstant("d1", NaN)), title = "Third model")), data)), "^Third model: the value")
  # whole numbers, integers, small and large numbers are fine; an unused constant is not read
  for (value in list(0, 2L, -3, 1e-300, 1e300, 0.1))
    expect_null(mtError(prepareModelTerms(list(mtModel(list(x, mtConstant("d2", value)))), data)), info = deparse(value))
  expect_null(mtError(prepareModelTerms(list(mtModel(list(x, mtConstant("d2", "a")), terms = list("d1"))), data)))

  # two offsets of one variable, or two constants of one value, are one too many; other settings are fine
  expectError(mtModel(list(mtOffset("d1", "z"), mtOffset("d2", "z"))), "Model 0: 'z' has two term definitions with the same settings.")
  expectError(mtModel(list(mtConstant("d1", 2), mtConstant("d2", 2))), "Model 0: two term definitions give the same term offset(2); use only one of them.")
  expectError(mtModel(list(mtConstant("d1", 2), mtConstant("d2", 2L))), "Model 0: two term definitions give the same term offset(2); use only one of them.")
  expect_null(mtError(prepareModelTerms(list(mtModel(list(mtOffset("d1", "z"), mtOffset("d2", "w"), mtConstant("d3", 2), mtConstant("d4", 3),
                                                          mtDef("d5", "z", "none"), mtDef("d6", "z", "log")))), data)))

  # the variable must exist; the specifications are spelled `offset` and `offsetConstant`
  expectError(mtModel(list(mtOffset("d1", "q"))), "Model 0: the data has no variable 'q'.")
  expectError(mtModel(list(mtDef("d1", "z", "Offset"))), "Model 0: 'Offset' is not a known term specification.")
  expectError(mtModel(list(mtConstant("d1", 2), mtDef("d2", "z", "offsetconstant"))), "Model 0: 'offsetconstant' is not a known term specification.")
  expectError(mtModel(list(mtDef("d1", "", "offset"))), "Model 0: the definition 'd1' needs a variable and a specification.")
  expectError(mtModel(list(list(id = "d1", specification = "", value = 2))), "Model 0: the definition 'd1' needs a variable and a specification.")
})

test_that("the offset of a variable needs numeric values in new data; missing values are taken as they are", {
  data  <- mtData()
  model <- mtModel(list(mtDef("d1", "x", "none"), mtOffset("d2", "z"), mtConstant("d3", 1)))
  prepared <- prepareModelTerms(list(model), data)

  expect_equal(mtError(modelTermsOffset(prepared, 1, data = data.frame(x = 1, z = "a"))), "Model 0: the variable 'z' must be numeric.")
  expect_equal(mtError(mtDesign(prepared, 1, data = data.frame(x = 1, z = "a"))), "Model 0: the variable 'z' must be numeric.")
  expect_equal(mtError(modelTermsOffset(prepared, 1, data = data.frame(x = 1))), "Model 0: the data has no variable 'z'.")
  expect_equal(modelTermsOffset(prepared, 1, data = data.frame(x = c(1, 2), z = c(NA, 5))), c(NA, 6))
  expect_equal(modelTermsOffset(prepared, 1, data = data.frame(x = c(1, 2), z = c(-5, 0))), c(-4, 1))

  # a nominal variable in new data where the fitting data had a number is not a number
  factorZ <- data.frame(x = 1, z = factor("a"))
  expect_match(mtError(modelTermsOffset(prepared, 1, data = factorZ)), "must be numeric", fixed = TRUE)
})

test_that("offsets are read from JSON: the variable offset has no parameters, the constant has a value and no variable", {
  data <- mtData()
  # keys that the reader does not know (`value.types`) are not read
  json <- '[{"name": "model0", "title": "Model 0", "components": [["x"], ["z"]], "intercept": true,
             "definitions": [{"id": "d1", "variable": "x", "specification": "none", "restriction": ""},
                             {"id": "d2", "variable": "z", "specification": "offset"},
                             {"id": "d3", "variable": "w", "specification": "offset", "restriction": ""},
                             {"id": "d4", "specification": "offsetConstant", "value": 2},
                             {"id": "d5", "specification": "offsetConstant", "value": 0.5, "restriction": "", "value.types": []},
                             {"id": "d6", "variable": "", "specification": "offsetConstant", "value": -3}],
             "terms": [{"id": "t1", "operands": ["d1"]}, {"id": "t2", "operands": ["d2"]}, {"id": "t3", "operands": "d3"},
                       {"id": "t4", "operands": ["d4"]}, {"id": "t5", "operands": ["d5"]}, {"id": "t6", "operands": ["d6"]}]},
            {"name": "model1", "title": "Model 1", "components": [["x"], ["z"]],
             "definitions": [{"id": "d1", "variable": "x", "specification": "none", "restriction": ""},
                             {"id": "d2", "variable": "z", "specification": "offset"},
                             {"id": "d3", "specification": "offsetConstant", "value": 7}],
             "terms": [{"id": "t1", "operands": ["d1"]}, {"id": "t2", "operands": ["d2"]}, {"id": "t3", "operands": ["d3"]}]}]'
  parse    <- function(json) jsonlite::fromJSON(json, simplifyVector = TRUE, simplifyDataFrame = FALSE, simplifyMatrix = FALSE)
  prepared <- prepareModelTerms(parse(json), data)

  expect_equal(modelTermsOffset(prepared, 1), data$z + data$w + 2 + 0.5 - 3)
  expect_equal(modelTermsConstantOffset(prepared, 1), -0.5)
  expect_equal(modelTermsOffset(prepared, 2), data$z + 7)
  expect_equal(modelTermsConstantOffset(prepared, 2), 7)
  expect_equal(prepared$models[[1]]$terms$label, c("x", "offset(z)", "offset(w)", "offset(2)", "offset(0.5)", "offset(-3)"))
  expect_equal(prepared$models[[1]]$terms$offset, c(FALSE, TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_equal(prepared$nested, FALSE)
  expect_equal(names(modelTermsDesign(prepared, 1)), c("(Intercept)", "t1"))
  expect_equal(modelTermsFormula(prepared, 2, response = "y"), y ~ x + offset(z), ignore_formula_env = TRUE)

  # a value that is a string is not a number, and neither is null
  string <- sub('"value": 7', '"value": "7"', json, fixed = TRUE)
  expect_equal(mtError(prepareModelTerms(parse(string), data)), "Model 1: the value of a constant offset must be a finite number, not '7'.")
  null <- sub('"value": 7', '"value": null', json, fixed = TRUE)
  expect_match(mtError(prepareModelTerms(parse(null), data)), "^Model 1: the value of a constant offset must be a finite number")
})
