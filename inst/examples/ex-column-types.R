# load mtcars
df <- mtcars
str(df)

# by default numeric columns are converted to jaspScale
r2jasp(df) |> str()

# change cyl to an ordinal variable
df$cyl <- jaspOrdinal(df$cyl, values = c(2, 4, 6, 8), labels = c("two", "four", "six", "eight"))
# change vs and am to nominal variables
df$vs  <- jaspNominal(df$vs,  labels = c("No", "Yes"))
df$am  <- jaspNominal(df$am,  labels = c("No", "Yes"))
# create a new variable that is a nominal text
df$group <- jaspNominal(rep(LETTERS[1:2], 16))

# factor and ordered will be converted to Nominal and Ordinal, respectively
df$gear <- as.factor(df$gear)
df$carb <- as.ordered(df$carb)

str(df)

# pass the data set to JASP
setDataSet(df)
# and retrieve it back
getDataSet() |> str()
# get the column specification of the set data frame as a list
dataSetColumnSpecification()

# check how are these columns converted back to R types
getDataSet() |> jasp2r() |> str()
