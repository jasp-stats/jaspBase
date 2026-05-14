testthat::test_that("wrapped analysis QML paths prefer explicit files", {
  explicitFile <- tempfile(fileext = ".qml")
  writeLines("import QtQuick", explicitFile)

  qmlFile <- jaspBase:::.wrappedAnalysisQmlFile(
    moduleName = "jaspBase",
    qmlFileName = "Ignored.qml",
    qmlFile = explicitFile
  )

  testthat::expect_equal(qmlFile, normalizePath(explicitFile, winslash = "/", mustWork = FALSE))
})

testthat::test_that("wrapped analysis QML paths resolve checkout module paths", {
  modulePath <- tempfile("module")
  qmlDir <- file.path(modulePath, "inst", "qml")
  dir.create(qmlDir, recursive = TRUE)
  qmlPath <- file.path(qmlDir, "Analysis.qml")
  writeLines("import QtQuick", qmlPath)

  qmlFile <- jaspBase:::.wrappedAnalysisQmlFile(
    moduleName = "jaspBase",
    qmlFileName = "Analysis.qml",
    modulePath = modulePath
  )

  testthat::expect_equal(qmlFile, normalizePath(qmlPath, winslash = "/", mustWork = FALSE))
})

testthat::test_that("standalone bridge can read the full dataset callback", {
  oldCallback <- if (exists(".readFullDatasetToEnd", envir = .GlobalEnv, inherits = FALSE)) {
    get(".readFullDatasetToEnd", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  hadCallback <- exists(".readFullDatasetToEnd", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (hadCallback) {
      assign(".readFullDatasetToEnd", oldCallback, envir = .GlobalEnv)
    } else if (exists(".readFullDatasetToEnd", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".readFullDatasetToEnd", envir = .GlobalEnv)
    }
  }, add = TRUE)

  assign(
    ".readFullDatasetToEnd",
    function() data.frame(x = 1, check.names = FALSE),
    envir = .GlobalEnv
  )

  testthat::expect_equal(
    jaspBase:::.fromRCPP(".readFullDatasetToEnd"),
    data.frame(x = 1, check.names = FALSE)
  )
})

testthat::test_that("standalone state saving uses the callback file contract", {
  oldCallback <- if (exists(".requestStateFileNameNative", envir = .GlobalEnv, inherits = FALSE)) {
    get(".requestStateFileNameNative", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  hadCallback <- exists(".requestStateFileNameNative", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (hadCallback) {
      assign(".requestStateFileNameNative", oldCallback, envir = .GlobalEnv)
    } else if (exists(".requestStateFileNameNative", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".requestStateFileNameNative", envir = .GlobalEnv)
    }
  }, add = TRUE)

  stateFile <- tempfile("jasp-state-")
  assign(
    ".requestStateFileNameNative",
    function() list(root = dirname(stateFile), relativePath = basename(stateFile)),
    envir = .GlobalEnv
  )

  savedState <- list(figures = list(), other = list(answer = 42))
  result <- jaspBase:::.saveState(savedState)
  loaded <- load(stateFile)

  testthat::expect_equal(result$relativePath, basename(stateFile))
  testthat::expect_identical(loaded, "state")
  testthat::expect_equal(state, savedState)
})

testthat::test_that("standalone state retrieval uses the callback root", {
  oldCallback <- if (exists(".requestStateFileNameNative", envir = .GlobalEnv, inherits = FALSE)) {
    get(".requestStateFileNameNative", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  hadCallback <- exists(".requestStateFileNameNative", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (hadCallback) {
      assign(".requestStateFileNameNative", oldCallback, envir = .GlobalEnv)
    } else if (exists(".requestStateFileNameNative", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".requestStateFileNameNative", envir = .GlobalEnv)
    }
  }, add = TRUE)

  stateFile <- tempfile("jasp-state-")
  assign(
    ".requestStateFileNameNative",
    function() list(root = dirname(stateFile), relativePath = basename(stateFile)),
    envir = .GlobalEnv
  )

  state <- list(figures = list(), other = list(answer = 42))
  save(state, file = stateFile, compress = FALSE)
  rm(state)

  testthat::expect_equal(
    jaspBase:::.retrieveState(),
    list(figures = list(), other = list(answer = 42))
  )
})
