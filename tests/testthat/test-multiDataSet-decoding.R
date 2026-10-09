# The dataset-aware routing of encoded column names (R/common.R). Options and dataset slices meet
# in the ENCODED namespace (the engine encodes both with the same per-dataset encoder), so analyses
# index datasets[[id]][[encodedValue]] directly; results are decoded by the engine on the way out.
# These helpers recover the dataset id from the encoded name and route with it.

# A handout as the engine delivers it: keyed by dataset id, column names already encoded by that
# dataset's encoder ("JASPColumn_<id>_<counter>_Encoded", DataSet::setupEncoderPrefix).
makeDatasets <- function() {
  datasets <- list(
    data.frame(JASPColumn_11_0_Encoded = c(1, 2)),                                  # "age"
    data.frame(JASPColumn_12_1_Encoded = c(3, 4), JASPColumn_12_0_Encoded = c(5, 6))) # "age", "score"
  names(datasets) <- c("11", "12")
  attr(datasets, "dataSetNames") <- c("11" = "Alpha", "12" = "Beta")
  datasets
}

# A syntax-mode wrapper handover: plain names, no encoder involved.
makePlainDatasets <- function() {
  datasets <- list(data.frame(age = c(1, 2)), data.frame(age = c(3, 4), score = c(5, 6)))
  names(datasets) <- c("11", "12")
  attr(datasets, "dataSetNames") <- c("11" = "Alpha", "12" = "Beta")
  datasets
}

test_that("dataSetIdFromEncoded recovers the id from the REAL encoder output", {
  # the format DataSet::setupEncoderPrefix actually produces, postfix included:
  expect_equal(dataSetIdFromEncoded("JASPColumn_12_3_Encoded"), 12L)
  expect_equal(dataSetIdFromEncoded("JASPColumn_11_0_Encoded"), 11L)
  expect_equal(dataSetIdFromEncoded(c("JASPColumn_1_0_Encoded", "JASPColumn_9_4_Encoded")), c(1L, 9L))

  # tolerated variants (defensive; neither survives into a real option value):
  expect_equal(dataSetIdFromEncoded("JASPColumn_12_3"), 12L)                    # without postfix
  expect_equal(dataSetIdFromEncoded("JASPColumn_12_3.scale"), 12L)              # type suffix
  expect_equal(dataSetIdFromEncoded("JASPColumn_12_3_For_Replacement"), 12L)    # replacement pass
})

test_that("dataSetIdFromEncoded gives NA when there is no id to recover", {
  expect_equal(dataSetIdFromEncoded("JaspColumn_7_Encoded"), NA_integer_)   # legacy, no dataset id
  expect_equal(dataSetIdFromEncoded("age"), NA_integer_)                    # plain column name
  expect_equal(dataSetIdFromEncoded("JASPColumn_x_3_Encoded"), NA_integer_) # not our format
  expect_equal(dataSetIdFromEncoded(NA_character_), NA_integer_)
  expect_equal(dataSetIdFromEncoded(NULL), integer(0))
})

test_that("dataSetNameFromEncoded maps ids through the dataSetNames attribute", {
  datasets <- makeDatasets()

  expect_equal(dataSetNameFromEncoded("JASPColumn_11_0_Encoded", datasets), "Alpha")
  expect_equal(dataSetNameFromEncoded("JASPColumn_12_0_Encoded", datasets), "Beta")
  expect_equal(dataSetNameFromEncoded("JASPColumn_99_0_Encoded", datasets), NA_character_)  # dangling id
})

test_that("dataSetNameFromEncoded attributes id-less names to the primary dataset", {
  datasets <- makeDatasets()

  expect_equal(dataSetNameFromEncoded("age", datasets), "Alpha")             # plain name
  expect_equal(dataSetNameFromEncoded("JaspColumn_7_Encoded", datasets), "Alpha") # legacy encoding
})

test_that("getDataSetFor routes encoded values to the right dataframe", {
  datasets <- makeDatasets()

  expect_identical(getDataSetFor("JASPColumn_12_0_Encoded", datasets), datasets[["12"]])
  expect_identical(getDataSetFor("JASPColumn_11_0_Encoded", datasets), datasets[["11"]])
  expect_identical(getDataSetFor("age", datasets), datasets[[1]])                   # primary fallback
  expect_identical(getDataSetFor("JASPColumn_99_0_Encoded", datasets), datasets[[1]]) # dangling -> primary
})

test_that("getDataSetFor falls back through the other datasets for plain names", {
  datasets <- makePlainDatasets()  # syntax-mode handover, nothing encoded

  # 'score' is not in the primary but lives in the second dataset: plain-name lookup finds it there
  expect_identical(getDataSetFor("score", datasets), datasets[["12"]])
  # ...while 'age', which the primary has, stays routed to the primary
  expect_identical(getDataSetFor("age", datasets), datasets[[1]])
  # nowhere to be found -> default
  fallback <- data.frame(x = 0)
  expect_identical(getDataSetFor("nonexistent", datasets, default = fallback), fallback)
})

test_that("getDataSetColumn indexes the encoded namespace directly", {
  datasets <- makeDatasets()

  expect_identical(getDataSetColumn("JASPColumn_11_0_Encoded", datasets), c(1, 2)) # age of Alpha
  expect_identical(getDataSetColumn("JASPColumn_12_1_Encoded", datasets), c(3, 4)) # age of Beta
  expect_identical(getDataSetColumn("JASPColumn_12_0_Encoded", datasets), c(5, 6)) # score of Beta
  expect_null(getDataSetColumn("JASPColumn_99_0_Encoded", datasets))               # nowhere
})

test_that("getDataSetColumn scans when a dataset lost the column it was routed to", {
  datasets <- makeDatasets()

  # value says dataset 12, only dataset 11's frame has such a column after a (hypothetical) edit
  # deliberately lost the columns they had (names rewritten below)
  names(datasets[["11"]]) <- "JASPColumn_12_1_Encoded"
  names(datasets[["12"]]) <- "JASPColumn_12_9_Encoded"

  expect_identical(getDataSetColumn("JASPColumn_12_1_Encoded", datasets), c(1, 2))
})

test_that("the helpers work on the datasets list runJaspResults builds", {
  # same shape as the real handout: keyed by dataset id, titles as attribute, columns encoded
  datasets <- list(
    data.frame(JASPColumn_5_0_Encoded = 1:3),
    data.frame(JASPColumn_12_0_Encoded = 4:6))
  names(datasets) <- as.character(c(5, 12))
  attr(datasets, "dataSetNames") <- list("5" = "iris", "12" = "iris (2)")

  expect_equal(dataSetNameFromEncoded("JASPColumn_12_0_Encoded", datasets), "iris (2)")

  routed <- getDataSetFor("JASPColumn_12_0_Encoded", datasets)
  expect_identical(routed[[ "JASPColumn_12_0_Encoded" ]], 4:6)
  expect_identical(getDataSetColumn("JASPColumn_12_0_Encoded", datasets), 4:6)
})
