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
jaspFormula(mpg ~ disp + (1 | cyl) + (disp | cyl) + (hp | cyl), mtcars)


# THESE EXAMPLES SHOW HOW JASP FORMULA DEVIATES FROM THE STANDARD BEHAVIOR OF FORMULAS

# It is not possible to use variable transformations in the formula
jaspFormula(mpg + disp ~ cyl, mtcars)
jaspFormula(mpg ~ exp(disp),  mtcars)

# It is not possible to use `offset`
jaspFormula(mpg ~ offset(disp) + cyl, mtcars)

# It is not possible to correlate random effects of `disp` and `hp` but hold the intercept uncorrelated.
# in this case all random effects are assumed uncorrelated contrary to the typical use in lme4.
jaspFormula(mpg ~ disp + (1 | cyl) + (0 + disp + hp | cyl), mtcars)
