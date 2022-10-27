# standard lm-style formulas with interaction
jaspFormula(mpg ~ cyl * disp, mtcars)

# exclude intercept
jaspFormula(mpg ~  0 + cyl * disp, mtcars)
jaspFormula(mpg ~ -1 + cyl * disp, mtcars)

# combine multiple columns on the lhs and multiple columns on the rhs (without interaction)
jaspFormula(cbind(mpg, disp) ~ cyl + gear, mtcars)

# non-syntactic column names
df <- data.frame(x = rlnorm(10))
df[["log(x)"]] <- log(df$x)
df[["a b ~ y <- gamma("]] <- rnorm(10)
jaspFormula(`a b ~ y <- gamma(` ~ `log(x)`, df)

# lme4 syntax for mixed models
jaspFormula(mpg ~ disp*hp + (0 + disp + hp | cyl) + (1 | carb), mtcars)

# uncorrelated intercept and slopes
# the following two lines give the same result
jaspFormula(mpg ~ disp + (1 + disp + hp || cyl), mtcars)
jaspFormula(mpg ~ disp + (1 | cyl) + (0 + disp | cyl) + (0 + hp | cyl), mtcars)


# THESE EXAMPLES SHOW HOW JASP FORMULA DEVIATES FROM THE STANDARD BEHAVIOR OF FORMULAS

if(interactive()) {
  # It is not possible to use variable transformations in the formula
  jaspFormula(mpg + disp ~ cyl, mtcars)
  jaspFormula(mpg ~ exp(disp),  mtcars)

  # It is not possible to use `offset`
  jaspFormula(mpg ~ offset(disp) + cyl, mtcars)
}

# Specify mixture of correlated and uncorrelated random terms; disp and hp are allowed to covary, as well as the random intercept and drat.
# In this case the output gives $rhs$random$cyl$correlated == TRUE, the actual correlation structure
# can be accessed from the "correlations attribute"
result <- jaspFormula(mpg ~ disp + (0 + disp + hp | cyl) + (1 + drat | cyl), mtcars)
isTRUE(result$rhs$random$cyl$correlated)
attr(result$rhs$random$cyl$correlated, "correlations")
