# Each successive `jaspFormula` and `makeJaspFormula` give the same result

# standard lm-style formulas with interaction
jaspFormula(mpg ~ cyl * disp, mtcars)
makeJaspFormula(
  data = mtcars, response = "mpg",
  jaspFormulaRhs(terms = c("cyl", "disp", "cyl:disp"))
  )


# convert to json
jsonlite::toJSON(jaspFormula(mpg ~ (cyl * disp | am), mtcars), force = TRUE, pretty = TRUE)

# exclude intercept
jaspFormula(mpg ~  0 + cyl * disp, mtcars)
jaspFormula(mpg ~ -1 + cyl * disp, mtcars)
makeJaspFormula(
  data = mtcars, response = "mpg",
  jaspFormulaRhs(terms = "cyl * disp", intercept = FALSE)
  )

# combine multiple columns on the lhs and multiple columns on the rhs (without interaction)
jaspFormula(cbind(mpg, disp) ~ cyl + gear, mtcars)
makeJaspFormula(
  data = mtcars, response = c("mpg", "disp"),
  jaspFormulaRhs(terms = c("cyl", "gear"))
  )

# non-syntactic column names
df <- data.frame(x = rlnorm(10))
df[["log(x)"]] <- log(df$x)
df[["a b ~ y <- gamma("]] <- rnorm(10)
jaspFormula(`a b ~ y <- gamma(` ~ `log(x)`, df)
makeJaspFormula(
  data = df, response = "`a b ~ y <- gamma(`",
  jaspFormulaRhs("`log(x)`")
  )

# lme4 syntax for mixed models
jaspFormula(mpg ~ disp*hp + (0 + disp + hp | cyl) + (1 | carb), mtcars)
makeJaspFormula(
  data = mtcars, response = "mpg",
  # fixed effects
  jaspFormulaRhs(terms = "disp*hp"),
  # random effects by cyl
  jaspFormulaRhs(terms = c("disp", "hp"), group = "cyl", intercept = FALSE),
  # random intercept by carb
  jaspFormulaRhs(group = "carb")
)

# uncorrelated intercept and slopes
jaspFormula(mpg ~ disp + (1 + disp + hp || cyl), mtcars)
makeJaspFormula(
  data = mtcars, response = "mpg",
  jaspFormulaRhs("disp"),
  jaspFormulaRhs(c("disp", "hp"), group = "cyl", correlated = FALSE)
)

jaspFormula(mpg ~ disp + (1 | cyl) + (0 + disp | cyl) + (0 + hp | cyl), mtcars)
makeJaspFormula(
  data = mtcars, response = "mpg",
  jaspFormulaRhs("disp"),
  jaspFormulaRhs(group = "cyl"), # intercept
  jaspFormulaRhs("disp", "cyl", intercept = FALSE),
  jaspFormulaRhs("hp",   "cyl", intercept = FALSE)
)


# THESE EXAMPLES SHOW HOW JASP FORMULA DEVIATES FROM THE STANDARD BEHAVIOR OF FORMULAS

if(interactive()) {
  # It is not possible to use variable transformations in the formula
  jaspFormula(mpg + disp ~ cyl, mtcars)
  makeJaspFormula(data = mtcars, response = "mpg + disp", jaspFormulaRhs("cyl"))

  jaspFormula(mpg ~ exp(disp),  mtcars)
  makeJaspFormula(data = mtcars, response = "mpg", jaspFormulaRhs("exp(disp)"))

  # It is not possible to use `offset`
  jaspFormula(mpg ~ offset(disp) + cyl, mtcars)
  makeJaspFormula(data = mtcars, response = "mpg", jaspFormulaRhs("offset(disp)"))
}

# Specify mixture of correlated and uncorrelated random terms;
# disp and hp are allowed to covary, as well as the random intercept and drat.
# In this case the output gives $rhs$random$cyl$correlated == TRUE,
# the actual correlation structure can be accessed from the "correlations" attribute.
result <- jaspFormula(mpg ~ disp + (0 + disp + hp | cyl) + (1 + drat | cyl), mtcars)
isTRUE(result$rhs$random$cyl$correlated)
attr(result$rhs$random$cyl$correlated, "correlations")

makeJaspFormula(
  data = mtcars, response = "mpg",
  jaspFormulaRhs(terms = "disp"),
  jaspFormulaRhs(terms = c("disp", "hp"), group = "cyl", intercept = FALSE, correlated = TRUE),
  jaspFormulaRhs(terms = "drat",          group = "cyl", intercept = TRUE,  correlated = TRUE)
)
