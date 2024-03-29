library(jaspBase)

# otherwise jaspState crashes badly (should be fixed another time)
jaspResultsCPP        <- jaspBase:::loadJaspResults("a name")
jaspResultsCPP$title  <- "a title"
jaspResults           <- jaspBase:::jaspResultsR$new(jaspResultsCPP)

ctr <- createJaspContainer("ctr1")

result <- ctr[["ctr2"]] %setOrRetrieve%
  createJaspContainer("my name")

result               # the container
isRecomputed(result) # true
isRecomputed()       # true

# the same statement as before
result <- ctr[["ctr2"]] %setOrRetrieve%
  createJaspContainer("my name")

isRecomputed(result) # false
isRecomputed()       # false

set.seed(123)
computeFunction <- function() {
  print("working hard!")
  c(a = rnorm(1))
}

result <- ctr[["state"]] %setOrRetrieve% (
  computeFunction() |>
    createJaspState()
)

result                         # the random number
#\dontrun{isRecomputed(result)} # intentional error
#This is a nice intentional error but renv keeps messing up my builds because of this.
isRecomputed(ctr[["state"]])   # true
isRecomputed()                 # true

result2 <- ctr[["state"]] %setOrRetrieve% (
  computeFunction() |>
    createJaspState()
)

identical(result, result2)   # the random number is identical, so retrieved from the state
isRecomputed(ctr[["state"]]) # false
isRecomputed()               # false

# Note that only works when the left hand side indexes inside a jaspObject
a <- 1
#\dontrun{a %setOrRetrieve% 2} # intentional error
