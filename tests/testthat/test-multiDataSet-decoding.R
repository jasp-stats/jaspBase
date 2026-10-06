# The dataset-aware decoding of encoded column names (R/common.R): these mirror
# ColumnEncoder::dataSetIdFromEncoded in C++ and route option values of a multiDataSetAware
# analysis to their dataset in the `datasets` list.

makeDatasets <- function() {
  datasets <- list(data.frame(age = c(1, 2)), data.frame(age = c(3, 4), score = c(5, 6)))
  names(datasets) <- c("11", "12")
  attr(datasets, "dataSetNames") <- c("11" = "Alpha", "12" = "Beta")
  datasets
}

test_that("dataSetIdFromEncoded recovers the embedded dataset id", {
  expect_equal(dataSetIdFromEncoded("JASPColumn_12_3"), 12L)
  expect_equal(dataSetIdFromEncoded("JASPColumn_12_3.scale"), 12L)      # type suffix
  expect_equal(dataSetIdFromEncoded("JASPColumn_12_3_For_Replacement"), 12L)
  expect_equal(dataSetIdFromEncoded(c("JASPColumn_1_0", "JASPColumn_9_4")), c(1L, 9L))
})

test_that("dataSetIdFromEncoded gives NA when there is no id to recover", {
  expect_equal(dataSetIdFromEncoded("JASPColumn_7"), NA_integer_)       # legacy, no dataset id
  expect_equal(dataSetIdFromEncoded("age"), NA_integer_)                # plain column name
  expect_equal(dataSetIdFromEncoded("JASPColumn_x_3"), NA_integer_)     # not our format
  expect_equal(dataSetIdFromEncoded(NA_character_), NA_integer_)
  expect_equal(dataSetIdFromEncoded(NULL), integer(0))
})

test_that("dataSetNameFromEncoded maps ids through the dataSetNames attribute", {
  datasets <- makeDatasets()

  expect_equal(dataSetNameFromEncoded("JASPColumn_11_0", datasets), "Alpha")
  expect_equal(dataSetNameFromEncoded("JASPColumn_12_0.nominal", datasets), "Beta")
  expect_equal(dataSetNameFromEncoded("JASPColumn_99_0", datasets), NA_character_)  # dangling id
})

test_that("dataSetNameFromEncoded attributes id-less names to the primary dataset", {
  datasets <- makeDatasets()

  expect_equal(dataSetNameFromEncoded("age", datasets), "Alpha")        # plain name
  expect_equal(dataSetNameFromEncoded("JASPColumn_7", datasets), "Alpha") # legacy encoding
})

test_that("getDataSetFor routes values to the right dataframe", {
  datasets <- makeDatasets()

  expect_identical(getDataSetFor("JASPColumn_12_0", datasets), datasets[["12"]])
  expect_identical(getDataSetFor("JASPColumn_11_0.scale", datasets), datasets[["11"]])
  expect_identical(getDataSetFor("age", datasets), datasets[[1]])       # primary fallback
  expect_identical(getDataSetFor("JASPColumn_99_0", datasets), datasets[[1]]) # dangling -> primary
})

test_that("getDataSetFor falls back to default for columns the primary lacks", {
  datasets <- makeDatasets()

  fallback <- data.frame(x = 0)
  expect_identical(getDataSetFor("score", datasets, default = fallback), fallback)
  # ...while a column the primary does have still lands there:
  expect_identical(getDataSetFor("age", datasets, default = fallback), datasets[[1]])
})

test_that("the helpers work on the datasets list runJaspResults builds", {
  # same shape as the real handout: keyed by dataset id, titles as attribute
  datasets <- list(data.frame(Sepal.Length = 1:3), data.frame(Sepal.Length = 4:6))
  names(datasets) <- as.character(c(5, 12))
  attr(datasets, "dataSetNames") <- list("5" = "iris", "12" = "iris (2)")

  expect_equal(dataSetNameFromEncoded("JASPColumn_12_1", datasets), "iris (2)")

  routed <- getDataSetFor("JASPColumn_12_1", datasets)
  expect_identical(routed$Sepal.Length, 4:6)
})
