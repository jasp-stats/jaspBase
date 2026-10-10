localNamespaceBinding <- function(name, value, namespace) {
  oldValue <- get(name, envir = namespace, inherits = FALSE)
  wasLocked <- bindingIsLocked(name, namespace)

  if (wasLocked)
    unlockBinding(name, namespace)
  assign(name, value, envir = namespace)
  if (wasLocked)
    lockBinding(name, namespace)

  function() {
    if (bindingIsLocked(name, namespace))
      unlockBinding(name, namespace)
    assign(name, oldValue, envir = namespace)
    if (wasLocked)
      lockBinding(name, namespace)
  }
}

testthat::test_that("wrapped analysis QML paths prefer explicit files", {
  explicitFile <- tempfile(fileext = ".qml")
  writeLines("import QtQuick", explicitFile)

  qmlFile <- jaspBase:::.wrappedAnalysisQmlFile(
    moduleName = "jaspBase",
    qmlFileName = "Ignored.qml",
    qmlFile = explicitFile
  )

  testthat::expect_equal(qmlFile, as.character(fs::path_norm(explicitFile)))
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

  testthat::expect_equal(qmlFile, as.character(fs::path_norm(qmlPath)))
})

testthat::test_that("wrapped analysis verbosity separates analysis and JASP chatter", {
  noisyValue <- function() {
    cat("jasp bridge output\n")
    message("analysis message")
    warning("analysis warning", call. = FALSE)
    42
  }

  testthat::expect_silent(
    testthat::expect_equal(
      jaspBase:::.runWrappedAnalysisWithVerbosity(noisyValue(), verbose = "none"),
      42
    )
  )

  testthat::expect_output(
    testthat::expect_message(
      testthat::expect_warning(
        jaspBase:::.runWrappedAnalysisWithVerbosity(noisyValue(), verbose = "analysis"),
        "analysis warning"
      ),
      "analysis message"
    ),
    NA
  )

  testthat::expect_output(
    testthat::expect_warning(
      testthat::expect_message(
        testthat::expect_equal(
          jaspBase:::.runWrappedAnalysisWithVerbosity(noisyValue(), verbose = "jasp"),
          42
        ),
        NA
      ),
      NA
    ),
    "jasp bridge output"
  )

  testthat::expect_output(
    testthat::expect_message(
      testthat::expect_warning(
        jaspBase:::.runWrappedAnalysisWithVerbosity(noisyValue(), verbose = "all"),
        "analysis warning"
      ),
      "analysis message"
    ),
    "jasp bridge output"
  )
})

testthat::test_that("wrapped analysis verbosity normalizes legacy quiet options", {
  testthat::expect_equal(jaspBase:::.normalizeRunWrappedAnalysisVerbose(NULL), "analysis")
  testthat::expect_equal(jaspBase:::.normalizeRunWrappedAnalysisVerbose(NULL, quiet = TRUE), "analysis")
  testthat::expect_equal(jaspBase:::.normalizeRunWrappedAnalysisVerbose(NULL, quiet = FALSE), "all")
  testthat::expect_equal(jaspBase:::.normalizeRunWrappedAnalysisVerbose(TRUE), "all")
  testthat::expect_equal(jaspBase:::.normalizeRunWrappedAnalysisVerbose(FALSE), "none")
  testthat::expect_equal(jaspBase:::.normalizeRunWrappedAnalysisVerbose("jasp"), "jasp")
  testthat::expect_equal(jaspBase:::.normalizeRunWrappedAnalysisVerbose("off"), "none")
  testthat::expect_error(
    jaspBase:::.normalizeRunWrappedAnalysisVerbose("loud"),
    "`verbose` must be one of"
  )
})

testthat::test_that("wrapped analysis verbosity honors jaspSyntax default option", {
  oldOptions <- options(
    jaspBase.runWrappedAnalysis.verbose = NULL,
    jaspSyntax.verbose = "none"
  )
  on.exit(do.call(options, oldOptions), add = TRUE)

  testthat::expect_identical(
    eval(formals(jaspBase::runWrappedAnalysis)$verbose),
    "none"
  )

  options(jaspBase.runWrappedAnalysis.verbose = "jasp")
  testthat::expect_identical(
    eval(formals(jaspBase::runWrappedAnalysis)$verbose),
    "jasp"
  )
})

testthat::test_that("wrapped analysis verbosity decodes analysis conditions", {
  testthat::skip_if_not_installed("jaspSyntax")
  decodeContext <- jaspBase:::.jaspDecodeContext(
    columnEncoderContext = structure(
      list(columns = list(list(name = "angle", type = "unknown")), extra = list()),
      class = "jaspSyntaxColumnEncoderContext"
    )
  )
  restoreContext <- localNamespaceBinding(
    ".currentJaspDecodeContext",
    function() decodeContext,
    asNamespace("jaspBase")
  )
  restoreDecoder <- localNamespaceBinding(
    "decodeColumnText",
    function(text, encoderContext = NULL) {
      gsub("JaspColumn_0_Encoded", "angle", text, fixed = TRUE)
    },
    asNamespace("jaspSyntax")
  )
  on.exit(restoreContext(), add = TRUE)
  on.exit(restoreDecoder(), add = TRUE)

  noisyValue <- function() {
    message("analysis message: JaspColumn_0_Encoded")
    warning("analysis warning: JaspColumn_0_Encoded", call. = FALSE)
    42
  }

  testthat::expect_output(
    testthat::expect_message(
      testthat::expect_warning(
        jaspBase:::.runWrappedAnalysisWithVerbosity(noisyValue(), verbose = "analysis"),
        "analysis warning: angle"
      ),
      "analysis message: angle"
    ),
    NA
  )

  testthat::expect_error(
    jaspBase:::.runWrappedAnalysisWithVerbosity(
      stop("analysis error: JaspColumn_0_Encoded", call. = FALSE),
      verbose = "analysis"
    ),
    "analysis error: angle"
  )
})

testthat::test_that("wrapped analysis condition decoding propagates native decoder failures", {
  testthat::skip_if_not_installed("jaspSyntax")
  decodeContext <- jaspBase:::.jaspDecodeContext(
    columnEncoderContext = structure(
      list(columns = list(list(name = "angle", type = "unknown")), extra = list()),
      class = "jaspSyntaxColumnEncoderContext"
    )
  )
  restoreContext <- localNamespaceBinding(
    ".currentJaspDecodeContext",
    function() decodeContext,
    asNamespace("jaspBase")
  )
  restoreDecoder <- localNamespaceBinding(
    "decodeColumnText",
    function(text, encoderContext = NULL) {
      stop("native decode failure", call. = FALSE)
    },
    asNamespace("jaspSyntax")
  )
  on.exit(restoreContext(), add = TRUE)
  on.exit(restoreDecoder(), add = TRUE)

  testthat::expect_error(
    jaspBase:::.runWrappedAnalysisWithVerbosity(
      stop("analysis error: JaspColumn_0_Encoded", call. = FALSE),
      verbose = "analysis"
    ),
    "native decode failure",
    fixed = TRUE
  )
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

testthat::test_that("state file locations reject unsupported callback shapes", {
  testthat::expect_error(
    jaspBase:::.stateFilePath(list(root = tempdir())),
    "non-empty `relativePath`"
  )
  testthat::expect_error(
    jaspBase:::.stateFilePath(tempfile()),
    "non-empty `relativePath`"
  )
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
