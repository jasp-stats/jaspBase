testthat::test_that("runWrappedAnalysis runs <analysis>Internal of a module with wrappers", {
  testthat::local_mocked_bindings(.moduleExports = function(moduleName) c("Descriptives", "DescriptivesInternal"))

  testthat::expect_identical(.wrappedAnalysisFunction("jaspDescriptives", "Descriptives"), "jaspDescriptives::DescriptivesInternal")
})

testthat::test_that("runWrappedAnalysis runs the analysis itself in a module without wrappers", {
  testthat::local_mocked_bindings(.moduleExports = function(moduleName) c("PetPeese", "SelectionModels"))

  testthat::expect_identical(.wrappedAnalysisFunction("jaspMetaAnalysis", "PetPeese"), "jaspMetaAnalysis::PetPeese")
})

testthat::test_that("runWrappedAnalysis says which functions a module lacks", {
  testthat::local_mocked_bindings(.moduleExports = function(moduleName) "SelectionModels")

  testthat::expect_error(
    .wrappedAnalysisFunction("jaspMetaAnalysis", "PetPeese"),
    "Module jaspMetaAnalysis exports neither PetPeeseInternal nor PetPeese"
  )
})

testthat::test_that(".moduleExports gives the exports of a module", {
  testthat::expect_true("runWrappedAnalysis" %in% .moduleExports("jaspBase"))
})
