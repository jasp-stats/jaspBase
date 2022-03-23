test_that("package installation from scratch works", {

  tempRoot        <- withr::local_tempdir(pattern = "jaspBase-test-")
  mockJaspRoot    <- normalizePath(testthat::test_path("mock-jasp-desktop"))
  moduleName      <- "jaspDescriptives"
  tempBuildFolder <- file.path(tempRoot, "jasp-desktop-build")
  moduleLibrary   <- file.path(tempBuildFolder, "Modules", moduleName)
  modulePkg       <- file.path(mockJaspRoot,    "Modules", moduleName)

  # let's not polute anybodies cache/ root
  renvRootPath  <- file.path(tempRoot, "renv-root")
  renvCachePath <- file.path(tempRoot, "renv-cache")
  withr::local_envvar(c("RENV_PATHS_ROOT" = renvRootPath, "RENV_PATHS_CACHE" = renvCachePath))

  # compute the hashes that are expected in the lockfile
  precomputedHashes <- c(
    "jaspDescriptives" = jaspBase:::getModuleHash(modulePkg),
    "jaspGraphs"       = jaspBase:::getModuleHash(file.path(mockJaspRoot, "Engine", "jaspGraphs")),
    "jaspBase"         = jaspBase:::getModuleHash(file.path(mockJaspRoot, "Engine", "jaspBase"))
  )
  # TODO: use snapshot for this?
  expect_identical(precomputedHashes,
    c(jaspDescriptives = "e1226f32540dfd6fdebbabbdc1086dff",
      jaspGraphs       = "fb30f27b6a5c8b3eac68d53c97904b06",
      jaspBase         = "4f3784d2c99bb9849772593ecf213f2d"),
    info = if (getOS() == "windows") "This test only passes when line endings consist of \\n (LF), not \\r\\n (CRLF), which is the default on windows."
  )

  optionsBefore <- options()
  for (recordPackages in c("localJasp", "all")) {

    mkdirs(tempBuildFolder, tempBuildFolder, moduleLibrary, renvRootPath, renvCachePath)

    jaspBase::installJaspModuleNew(modulePkg = modulePkg, jaspRoot = mockJaspRoot, moduleLibrary = moduleLibrary, recordPackages = recordPackages)

    installedDir <- file.path(moduleLibrary, moduleName)

    expect_dir    (installedDir, failure_message = "Failed to install jaspDescriptives - No folder.")
    expect_symlink(installedDir, failure_message = "Failed to cache jaspDescriptives - Folder is not a symlink.")

    lockfilePath <- file.path(moduleLibrary, "renv.lock")
    expect_file(lockfilePath, "Lockfile for jaspDescriptives does not exist")

    lockfile <- renv:::renv_lockfile_read(lockfilePath)

    for (pkg in c(moduleName, "jaspGraphs", "jaspBase")) {

      descriptionPath <- file.path(
        mockJaspRoot,
        if (pkg %in% c("jaspGraphs", "jaspBase")) "Engine" else "Modules",
        pkg, "DESCRIPTION"
      )

      depDescription <- renv:::renv_description_read(descriptionPath)
      depVersion <- depDescription$Version
      depHash <- lockfile$Packages[[pkg]]$Hash
      cacheDir <- file.path(renv::paths$cache(), pkg, depVersion, depHash, pkg)

      hashReference <- precomputedHashes[[pkg]]

      expect_identical(depHash, hashReference, label = sprintf("hash of %s in lockfile (%s) does not match the renv hash.", pkg, recordPackages))
      expect_dir(cacheDir, sprintf("Failed to cache %s - Folder does not exist in the renv cache", pkg))

      if (recordPackages == "localJasp") {
        expect_identical(lockfile$Packages[[pkg]]$Source, "Local")
      } else {
        expect_identical(lockfile$Packages[[pkg]]$Source, "GitHub")
      }

    }
  }

  expect_identical(options(), optionsBefore, label = "options changed were reset")

})

test_that("switching between recordPackages correctly updates the lockfile", {

  # NOTE: this test is essentially identical to the previous one, except that we only call
  # mkdirs(tempBuildFolder, tempBuildFolder, moduleLibrary, renvRootPath, renvCachePath)
  # ONCE, so that the same folders (renv-cache, etc.) are reused across the runs.
  # In addition, this test does not test precomputed hashes or option leakage
  # instead it tests that the cache is reused and that ???

  tempRoot        <- withr::local_tempdir(pattern = "jaspBase-test-")
  mockJaspRoot    <- normalizePath(testthat::test_path("mock-jasp-desktop"))
  moduleName      <- "jaspDescriptives"
  tempBuildFolder <- file.path(tempRoot, "jasp-desktop-build")
  moduleLibrary   <- file.path(tempBuildFolder, "Modules", moduleName)
  modulePkg       <- file.path(mockJaspRoot,    "Modules", moduleName)

  # let's not polute anybodies cache/ root
  renvRootPath  <- file.path(tempRoot, "renv-root")
  renvCachePath <- file.path(tempRoot, "renv-cache")
  withr::local_envvar(c("RENV_PATHS_ROOT" = renvRootPath, "RENV_PATHS_CACHE" = renvCachePath))

  # compute the hashes that are expected in the lockfile
  precomputedHashes <- c(
    "jaspDescriptives" = jaspBase:::getModuleHash(modulePkg),
    "jaspGraphs"       = jaspBase:::getModuleHash(file.path(mockJaspRoot, "Engine", "jaspGraphs")),
    "jaspBase"         = jaspBase:::getModuleHash(file.path(mockJaspRoot, "Engine", "jaspBase"))
  )

  # we repeat this multiple times to ensure that lockfile updates are correct
  opts <- c("localJasp", "all", "localJasp", "all")
  for (opt in list(opts, rev(opts))) {

    # this line is the big difference with the previous test
    mkdirs(tempBuildFolder, tempBuildFolder, moduleLibrary, renvRootPath, renvCachePath)

    idx <- 0L # to track the iterations
    fileinfo <- list()

    for (recordPackages in opts) {

      idx <- idx + 1L
      jaspBase::installJaspModuleNew(modulePkg = modulePkg, jaspRoot = mockJaspRoot, moduleLibrary = moduleLibrary, recordPackages = recordPackages)

      installedDir <- file.path(moduleLibrary, moduleName)

      expect_dir    (installedDir, failure_message = "Failed to install jaspDescriptives - No folder.")
      expect_symlink(installedDir, failure_message = "Failed to cache jaspDescriptives - Folder is not a symlink.")

      lockfilePath <- file.path(moduleLibrary, "renv.lock")
      expect_file(lockfilePath, "Lockfile for jaspDescriptives does not exist")

      lockfile <- renv:::renv_lockfile_read(lockfilePath)
      for (pkg in c(moduleName, "jaspGraphs", "jaspBase")) {

        descriptionPath <- file.path(
          mockJaspRoot,
          if (pkg %in% c("jaspGraphs", "jaspBase")) "Engine" else "Modules",
          pkg, "DESCRIPTION"
        )

        depDescription <- renv:::renv_description_read(descriptionPath)
        depVersion <- depDescription$Version
        depHash <- lockfile$Packages[[pkg]]$Hash
        cacheDir <- file.path(renv::paths$cache(), pkg, depVersion, depHash, pkg)

        hashReference <- precomputedHashes[[pkg]]

        expect_identical(depHash, hashReference, label = sprintf("hash of %s in lockfile (%s) does not match the renv hash.", pkg, recordPackages))
        expect_dir(cacheDir, sprintf("Failed to cache %s - Folder does not exist in the renv cache", pkg))

        if (recordPackages == "localJasp") {
          expect_identical(lockfile$Packages[[pkg]]$Source, "Local")
        } else {
          expect_identical(lockfile$Packages[[pkg]]$Source, "GitHub")
        }

        # the first time we record the file info of the package in the module library
        # the second run we assert that this is unchanged (which implies the package was retrieved from the cache)
        if (idx == 1L) {
          fileinfo[[pkg]] <- file.info(file.path(moduleLibrary, pkg))
        } else {
          fileinfo_test <- file.info(file.path(moduleLibrary, pkg))
          expect_identical(fileinfo[[pkg]], fileinfo_test)
        }
      }
    }
  }
})

test_that("package installation recognizes modifications in jasp modules and jasp module dependencies", {

  tempRoot        <- withr::local_tempdir(pattern = "jaspBase-test-")
  mockJaspRoot0   <- normalizePath(testthat::test_path("mock-jasp-desktop"))
  moduleName      <- "jaspDescriptives"
  tempBuildFolder <- file.path(tempRoot, "jasp-desktop-build")
  moduleLibrary   <- file.path(tempBuildFolder, "Modules", moduleName)

  if (dir.exists(file.path(tempRoot, "mock-jasp-desktop")))
    unlink(file.path(tempRoot, "mock-jasp-desktop"), recursive = TRUE)
  file.copy(mockJaspRoot0, tempRoot, recursive = TRUE, copy.mode = FALSE)
  mockJaspRoot    <- file.path(tempRoot, "mock-jasp-desktop")
  modulePkg       <- file.path(mockJaspRoot,    "Modules", moduleName)

  # let's not polute anybodies cache/ root
  renvRootPath  <- file.path(tempRoot, "renv-root")
  renvCachePath <- file.path(tempRoot, "renv-cache")
  withr::local_envvar(c("RENV_PATHS_ROOT" = renvRootPath, "RENV_PATHS_CACHE" = renvCachePath))

  mkdirs(tempBuildFolder, tempBuildFolder, moduleLibrary, renvRootPath, renvCachePath)
  lockfilePath <- file.path(moduleLibrary, "renv.lock")
  installedModulePath <- file.path(moduleLibrary, moduleName)

  # 1. Install the package
  jaspBase::installJaspModuleNew(modulePkg = modulePkg, jaspRoot = mockJaspRoot, moduleLibrary = moduleLibrary)

  lockfile1 <- renv:::renv_lockfile_read(lockfilePath)
  fileinfo1 <- file.info(installedModulePath)

  # 2. Modify jaspGraphs
  jaspGraphsDescriptionPath <- file.path(mockJaspRoot, "Engine", "jaspGraphs", "DESCRIPTION")
  jaspGraphsDescription <- readLines(jaspGraphsDescriptionPath)
  jaspGraphsDescription[4] <- "Version: 0.5.2.14"
  writeLines(paste(jaspGraphsDescription, collapse = "\n"), jaspGraphsDescriptionPath)

  jaspBase::installJaspModuleNew(modulePkg = modulePkg, jaspRoot = mockJaspRoot, moduleLibrary = moduleLibrary)

  lockfile2 <- renv:::renv_lockfile_read(lockfilePath)
  fileinfo2 <- file.info(installedModulePath)

  # 3. Modify jaspDescriptives
  jaspDescriptivesDescriptionPath <- file.path(modulePkg, "DESCRIPTION")
  jaspDescriptivesDescription <- readLines(jaspDescriptivesDescriptionPath)
  jaspDescriptivesDescription[4] <- "Version: 0.15.1"
  writeLines(paste(jaspDescriptivesDescription, collapse = "\n"), jaspDescriptivesDescriptionPath)

  jaspBase::installJaspModuleNew(modulePkg = modulePkg, jaspRoot = mockJaspRoot, moduleLibrary = moduleLibrary)

  lockfile3 <- renv:::renv_lockfile_read(lockfilePath)
  fileinfo3 <- file.info(installedModulePath)

  expect_identical(lockfile1$Packages$jaspGraphs$Version, "0.5.2.13")
  expect_identical(lockfile2$Packages$jaspGraphs$Version, "0.5.2.14")
  expect_identical(lockfile1$Packages$jaspDescriptives$Hash, lockfile2$Packages$jaspDescriptives$Hash)
  expect_identical(fileinfo1, fileinfo2)

  expect_identical(lockfile3$Packages$jaspDescriptives$Version, "0.15.1")
  expect_failure(expect_identical(
    lockfile1$Packages$jaspDescriptives$Version,
    lockfile3$Packages$jaspDescriptives$Version
  ))
  expect_failure(expect_identical(fileinfo1, fileinfo3))

})

test_that("installing a package with a lockfile works", {

  skip("We need to use real jasp Modules before the SHA in the lockfile makes any sense!")

  tempRoot <- withr::local_tempdir(pattern = "jaspBase-test-")

  mockJaspRoot0    <- normalizePath(testthat::test_path("mock-jasp-desktop"))

  if (dir.exists(file.path(tempRoot, "mock-jasp-desktop")))
    unlink(file.path(tempRoot, "mock-jasp-desktop"), recursive = TRUE)
  file.copy(mockJaspRoot0, tempRoot, recursive = TRUE, copy.mode = FALSE)
  mockJaspRoot    <- file.path(tempRoot, "mock-jasp-desktop")

  moduleName      <- "jaspDescriptivesLockfile"
  tempBuildFolder <- file.path(tempRoot, "jasp-desktop-build")
  moduleLibrary   <- file.path(tempBuildFolder, "Modules", moduleName)
  modulePkg       <- file.path(mockJaspRoot,    "Modules", moduleName)

  # let's not polute anybodies cache/ root
  renvRootPath  <- file.path(tempRoot, "renv-root")
  renvCachePath <- file.path(tempRoot, "renv-cache")
  withr::local_envvar(c("RENV_PATHS_ROOT" = renvRootPath, "RENV_PATHS_CACHE" = renvCachePath))

  # compute the hashes that are expected in the lockfile
  precomputedHashes <- c(
    "jaspDescriptivesLockfile" = jaspBase:::getModuleHash(modulePkg),
    "jaspGraphs"               = jaspBase:::getModuleHash(file.path(mockJaspRoot, "Engine", "jaspGraphs")),
    "jaspBase"                 = jaspBase:::getModuleHash(file.path(mockJaspRoot, "Engine", "jaspBase"))
  )

  mkdirs(tempBuildFolder, tempBuildFolder, moduleLibrary, renvRootPath, renvCachePath)

  jaspBase::installJaspModuleNew(modulePkg = modulePkg, jaspRoot = mockJaspRoot, moduleLibrary = moduleLibrary)

  expect_dir    (installedDir, failure_message = "Failed to install jaspDescriptivesLockfile - No folder.")
  expect_symlink(installedDir, failure_message = "Failed to cache jaspDescriptivesLockfile - Folder is not a symlink.")

  lockfilePath <- file.path(moduleLibrary, "renv.lock")
  expect_file(lockfilePath, "Lockfile for jaspDescriptivesLockfile does not exist")

  lockfile <- renv:::renv_lockfile_read(lockfilePath)

  for (pkg in c(moduleName, "jaspGraphs", "jaspBase")) {

    descriptionPath <- file.path(
      mockJaspRoot,
      if (pkg %in% c("jaspGraphs", "jaspBase")) "Engine" else "Modules",
      pkg, "DESCRIPTION"
    )

    depDescription <- renv:::renv_description_read(descriptionPath)
    depVersion <- depDescription$Version
    depHash <- lockfile$Packages[[pkg]]$Hash
    cacheDir <- file.path(renv::paths$cache(), pkg, depVersion, depHash, pkg)

    hashReference <- precomputedHashes[[pkg]]

    expect_identical(depHash, hashReference, label = sprintf("hash of %s in lockfile (%s) does not match the renv hash.", pkg, recordPackages))
    expect_dir(cacheDir, sprintf("Failed to cache %s - Folder does not exist in the renv cache", pkg))

    if (recordPackages == "localJasp") {
      expect_identical(lockfile$Packages[[pkg]]$Source, "Local")
    } else {
      expect_identical(lockfile$Packages[[pkg]]$Source, "GitHub")
    }

  }
})
