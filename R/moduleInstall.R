
postInstallFixes <- function(folderToFix) {
  if(length(ls(all.names=TRUE,pattern="\\.postProcessLibraryModule")) > 0 ) #We seem to be running in JASP (but this won't be used because renv starts a separate R process)
  {
    #print("we are *in* jasp, so we use .postProcessLibraryModule!")
    .postProcessLibraryModule(folderToFix)
  }
  else
  {
    #We do not have that function available so we will need to start JASPEngine ourselves, but where is it?
    old_PATH <- Sys.getenv("PATH")

    #sometimes R.dll is not in the path on windows, despite this being called from R...
    if(getOS() == "windows")
        Sys.setenv("PATH"=paste(R.home(component='bin'), ';', old_PATH, sep="", collapse=""))

    jaspEngineLocation <- Sys.getenv("JASPENGINE_LOCATION", unset = file.path(getwd(), "..", "JASPEngine"))
    jaspEngineCall     <- paste0(jaspEngineLocation, ' "', folderToFix ,'"')
    #print(paste0("Not *in* JASP so calling JASPEngine as: '", jaspEngineCall ,"'"))

    if(getOS() == "osx")
      system(jaspEngineCall)

    if(getOS() == "windows")
        Sys.setenv("PATH"=old_PATH);
  }
}

isModulePkgArchive <- function(modulePkg) { return(any(endsWith(modulePkg, c(".tar.gz", ".zip", ".tgz")))) }

#' @export
installJaspModule <- function(modulePkg, libPathsToUse, moduleLibrary, repos, onlyModPkg, force = FALSE, cacheAble=TRUE, frameworkLibrary=NULL) {

  isPkgArchive <- isModulePkgArchive(modulePkg)

  assertValidJASPmodule(modulePkg)

  r <- getOption("repos")
  r["CRAN"] <- repos
  options(repos = r)

  if (!isPkgArchive && !(force || md5SumsChanged(modulePkg, moduleLibrary))) {
    moduleName <- getModuleInfo(modulePkg)[["Package"]]
    if (dir.exists(file.path(moduleLibrary, moduleName))) {
      print(sprintf("Nothing changed according to md5sums, not reinstalling %s.", moduleName))
      return("succes!")
    } else {
      print(sprintf("Checksums exist for %s but the package is missing, installing anyway!", moduleName))
    }
  }

  return(
    pkgbuild::with_build_tools(
    {
      if (hasRenvLockFile(modulePkg)) installJaspModuleFromRenv(       modulePkg, libPathsToUse, moduleLibrary, repos, onlyModPkg, cacheAble=cacheAble)
      else                            installJaspModuleFromDescription(modulePkg, libPathsToUse, moduleLibrary, repos, onlyModPkg, cacheAble=cacheAble, frameworkLibrary=frameworkLibrary)
    },
    required=FALSE )
  )
}

installJaspModuleFromRenv <- function(modulePkg, libPathsToUse, moduleLibrary, repos, onlyModPkg, prompt = interactive(), cacheAble=TRUE) {

  print(sprintf("Installing module with renv. installJaspModuleFromRenv('%s', c(%s), '%s', '%s', %s)",
                modulePkg, paste0("'", libPathsToUse, "'", collapse = ", "), moduleLibrary, repos, onlyModPkg))

  renv::consent() #Consent to doing stuff in certain paths: https://rstudio.github.io/renv/reference/consent.html We've changed at least the root and the cache so hopefully that means it won't be changing stuff in the locations it mentions
  options(renv.verbose=TRUE) # More feedback wanted although this seems to do little
  #Ill let it depend on whether logging to file or something is on or off. options(renv.config.install.verbose=FALSE) #Turning this to TRUE will show buildoutput in qtcreator but not in nightlies etc! So leave it FALSE.

  if (!dir.exists(moduleLibrary))
    if (!dir.create(moduleLibrary))
      stop("failed to create moduleLibrary!")

  # ensure this starts with a ., otherwise it's picked up by renv "helpers"
  moduleLibraryTemp <- file.path(moduleLibrary, ".renv_temp")

  if (!dir.exists(file.path(moduleLibraryTemp, "renv")))
    dir.create(   file.path(moduleLibraryTemp, "renv"), recursive = TRUE)

  lockFileModule <- getFileFromModule(modulePkg,  "renv.lock")
  lockFileTemp   <- file.path(moduleLibraryTemp,  "renv.lock")
  file.copy(from = lockFileModule, to = lockFileTemp, overwrite = TRUE)

  setupRenv(moduleLibrary, modulePkg)

  # TODO: unclear whether this is necessary within JASP, or just within Rstudio.
  # renv must be unaware of any other libPaths than the cache and the directory designated for the module.
  # if there are other libPaths then renv may reuse pkgs from those other libPaths
  # it does copy those pkg to the cache before symlinking them
  # inspired by https://stackoverflow.com/a/36873741/4917834
  # it does appear to be necessary within rstudio and when the pkgs from jasp-required-files are present in a libPath
  #old.lib.loc <- .libPaths()
  #on.exit(assign(".lib.loc", old.lib.loc,   envir = environment(.libPaths)))
  #        assign(".lib.loc", moduleLibrary, envir = environment(.libPaths))


  lib <- renv::paths[["library"]](project = moduleLibrary)
  if (!dir.exists(lib))
    dir.create(lib, recursive = TRUE)

  renv::restore(project  = moduleLibraryTemp,
                library  = moduleLibrary,
                lockfile = lockfileTemp, clean = TRUE,
                prompt   = prompt)

  moduleInfo         <- getModuleInfo(modulePkg)
  correctlyInstalled <- installModulePkg(modulePkg, moduleLibrary, prompt, moduleInfo, cacheAble=cacheAble)

  if (!isPkgArchive && correctlyInstalled)
    writeMd5Sums(modulePkg, moduleLibrary)

  renv::snapshot(
    project  = moduleLibraryTemp,
    lockfile = lockfileTemp,
    packages = moduleInfo[["Package"]],
    prompt   = prompt,
    force    = TRUE # force is "safe" here because we only update the new module
  )

  # some checks here to assert that everything got installed correctly
  if (!libraryMatchesLockfile(moduleLibraryTemp)) {

    # do it again!
    renv::restore(
      project  = moduleLibraryTemp,
      library  = moduleLibrary,
      lockfile = lockfileTemp,
      clean    = TRUE,
      prompt   = prompt
    )

    if (!libraryMatchesLockfile(moduleLibraryTemp))
      warning("Failed to recreate lock file of module!")

  }

  if (unlink(moduleLibraryTemp, recursive = TRUE)) # 0/ FALSE for success
    warning(sprintf("Failed to remove temporary module libary at %s", moduleLibraryTemp))

  return("succes!")
}

installJaspModuleFromDescription <- function(modulePkg, libPathsToUse, moduleLibrary, repos, onlyModPkg, prompt = interactive(), cacheAble=TRUE, frameworkLibrary=NULL) {

  print("Installing module with DESCRIPTION file")

  if (!dir.exists(moduleLibrary))
    if (!dir.create(moduleLibrary))
      stop("failed to create moduleLibrary!")

  moduleLibraryTemp <- file.path(moduleLibrary, ".renv_temp")

  if (!dir.exists(file.path(moduleLibraryTemp, "renv")))
    dir.create(file.path(moduleLibraryTemp, "renv"), recursive = TRUE)

  setupRenv(moduleLibrary, modulePkg)

  # make renv blind for other libPaths
  #old.lib.loc <- .libPaths()
  #on.exit(assign(".lib.loc", old.lib.loc, envir = environment(.libPaths)))
  #assign(".lib.loc", moduleLibrary, envir = environment(.libPaths))

  # TODO: this is not very efficient because renv::install looks up the remotes on github...
  # there is a better way but it requires us to mess with renv's internals or to be more explicit about pkgs
  renv::hydrate(library = moduleLibrary, project = modulePkg, sources=c(moduleLibrary, frameworkLibrary))

  correctlyInstalled <- installModulePkg(modulePkg, moduleLibrary, prompt, cacheAble=cacheAble)
  if (!isModulePkgArchive(modulePkg) && correctlyInstalled)
    writeMd5Sums(modulePkg, moduleLibrary)

  if (unlink(moduleLibraryTemp, recursive = TRUE)) # 0/ FALSE for success
    warning(sprintf("Failed to remove temporary module libary at %s", moduleLibraryTemp))

  return("succes!")

}

installModulePkg <- function(modulePkg, moduleLibrary, prompt = interactive(), moduleInfo = NULL, cacheAble=TRUE) {

  if (is.null(moduleInfo))
    moduleInfo <- getModuleInfo(modulePkg)
  record <- recordFromModule(modulePkg, moduleInfo, cacheAble=cacheAble)

  print(paste0("Im telling renv to install to '", moduleLibrary, "' from '", modulePkg, "' which is a pkg", ifelse(isModulePkgArchive(modulePkg), "archive", "module")))

  renv::install(record, library = moduleLibrary, rebuild = TRUE, prompt = prompt)
  TRUE

}

assertValidJASPmodule <- function(modulePkg) {

  if(isModulePkgArchive(modulePkg))
    return() #Let R and JASP handle it

  if (!file.exists(file.path(modulePkg, "DESCRIPTION")))
    stop("Your module is missing a 'DESCRIPTION' file!")

  if (!file.exists(file.path(modulePkg, "inst", "Description.qml")))
    stop("Your module is missing 'inst/Description.qml'!")

  if (!dir.exists(file.path(modulePkg, "R")))
    stop("Your module is missing an 'R' directory!")

  if (!dir.exists(file.path(modulePkg, "inst", "qml")))
    stop("Your module is missing the 'inst/qml' directory!")

}

hasRenvLockFile <- function(modulePkg) {
  return(file.exists(file.path(modulePkg, "renv.lock")))
}

recordFromModule <- function(modulePkg, moduleInfo, cacheAble=TRUE) {

  record <- list(list(
    Package   = moduleInfo[["Package"]],
    Version   = moduleInfo[["Version"]],
    Path      = modulePkg,
    Source    = "Local",
    Cacheable = cacheAble
  ))
  names(record) <- moduleInfo[["Package"]]

  return(record)
}

getFileFromModule <- function(modulePkg, filename) {
  hereItGoes <- file.path(modulePkg, filename)

  if(isModulePkgArchive(modulePkg))
  {
    temp <- tempdir()

    #The archive contains a folder first, which has the name of the package, which we could or could not guess here.
    #lets just look at all the files
    files <- untar(tarfile=modulePkg, list=TRUE)
    found <- endsWith(files, filename)

    if(!any(found))
      stop(paste0("Can't find file '", filename, "' in archive '", modulePkg, "'"))

    #this will only work properly if the requested file is in there only once but for things like DESCRIPTION that should be no problem
    filename <- files[found]

    untar(tarfile=modulePkg, files=filename, exdir=temp)
    hereItGoes <- file.path(temp, filename)
  }

    if (!file.exists(hereItGoes))
      stop(paste("Your module contains no ",filename," file"))

  return(hereItGoes)
}

getModuleInfo <- function(modulePkg) {
  return(read.dcf(getFileFromModule(modulePkg, "DESCRIPTION"))[1, ])
}

renv_diagnostics_packages_as_df <- function(project) {

  lockfile  <- renv:::renv_diagnostics_packages_lockfile(project)
  libstate  <- renv:::renv_diagnostics_packages_library(project)
  used      <- unique(renv:::renv_diagnostics_packages_dependencies(project)$Package)
  recdeps   <- renv:::renv_package_dependencies(packages = used, project = project)
  all       <- c(names(lockfile$Packages), names(libstate$Packages),
                 names(recdeps))

  renv:::renv_scope_locale(category = "LC_COLLATE", locale = "C")

  all                        <- sort(unique(all))
  deps                       <- rep.int(NA_character_, length(all))
  names(deps)                <- all
  deps[names(recdeps)]       <- "indirect"
  deps[used]                 <- "direct"
  libpaths                   <- dirname(renv:::map_chr(all, renv:::renv_package_find))
  flibpaths                  <- factor(libpaths, levels = .libPaths())
  libcodes                   <- as.integer(flibpaths)
  libcodes[!is.na(libcodes)] <- sprintf("[%i]", libcodes[!is.na(libcodes)])

  return(data.frame(
    Library          = renv:::renv_diagnostics_packages_version(libstate, all),
    Source           = renv:::renv_diagnostics_packages_sources(libstate, all),
    Lockfile         = renv:::renv_diagnostics_packages_version(lockfile, all),
    Source           = renv:::renv_diagnostics_packages_sources(lockfile, all),
    Path             = libcodes,
    Dependency       = deps,
    stringsAsFactors = FALSE,
    check.names      = FALSE
  ))
}

libraryMatchesLockfile <- function(project = NULL) {
  project <- renv:::renv_project_resolve(project)
  df      <- renv_diagnostics_packages_as_df(project)
  notNA   <- which(complete.cases(df[, c("Library", "Lockfile")]))
  idxDiff <- which(df[notNA, "Library"] != df[notNA, "Lockfile"])
  hasDiff <- length(idxDiff) > 0L

  if (hasDiff) {
    print("Found the following mismatches between Library and the lockfile!")
    print(df[notNA[idxDiff], ])
  }

  return(!hasDiff)
}

#' @export
addRenvBeforeAfterDispatch <- function() {

  renBeforeAfterInstallStruct <- structure(list(
    before.install = function(x) {
      #print("BEFORE INSTALLING")
      #print(sprintf("Path = %s", mget("path", envir = parent.frame(1),
      #                                ifnotfound = "unknown path")))
      0 #do nothing
    },

    after.install = function(x)
    {
      installPath <- mget("installpath", envir = parent.frame(1), ifnotfound = "unknown")

      if(installPath != "unknown")
      {
        print(sprintf("Installed %s to '%s', now running post install fixes.", x, installPath))
        postInstallFixes(installPath)
      }
      else
        print(sprintf("Installing %s did not work immediately, but renv might still look at remotes for this.", x))

    }),
    class = "renvInstallHookOverride"
  )

  options(renv.install.package.options = renBeforeAfterInstallStruct)
}

#' @export
`[[.renvInstallHookOverride` <- function(x, ...) {
  return(unclass(x))
}

setupRenv <- function(moduleLibrary, modulePkg) {

  # renv adds e.g,. "R-3.6/x86_64-pc-linux-gnu" to all paths (R-version/os) and we don't need that
  assignFunctionInPackage(
    fun     = function() return(""),
    name    = "renv_bootstrap_platform_prefix",
    package = "renv"
  )
  # only necessary because we overwrite renv_bootstrap_platform_prefix, avoids complaints about renv misinterpreting the lockfile as a package
  renv::settings$ignored.packages("renv.lock")

  # renv_package_find crashes when package is base.
  # for some terrible reason, people explicitly do base:: even though this isn't necessary.
  renv::settings$ignored.packages(project = modulePkg, value = "base", persist = FALSE)

  cachePaths <- strsplit(Sys.getenv("RENV_PATHS_CACHE"), .Platform$path.sep)

  for(cachePath in cachePaths[[1]]) #strsplit is vectorized but we only give it a single string, so index to that single first one
    if (!dir.exists(cachePath))
     stop(sprintf("A cache is supposed to be at '%s' but it does not exist!", cachePath))

  Sys.setenv("RENV_PATHS_LIBRARY" = moduleLibrary)

  cat("Using the following paths:\n")
  for(name in names(renv::paths))
    cat(sprintf("%s:%s%s\n", name, strrep(" ", 12 - nchar(name)), renv::paths[[name]]()))

  options(install.opts = "--no-multiarch --no-docs --no-test-load"); #make sure we do not do a test-load, because this will not work on mac. the rpaths still need to be fixed

  #Try to nudge renv towards installing binaries when possible
  if(getOS() == "windows" || getOS() == "osx")
    options(install.packages.compile.from.source = "never")

  # do not do this when running unit tests
  if (!identical(Sys.getenv("TESTTHAT"), "true"))
    addRenvBeforeAfterDispatch()
}

#' @export
installJaspModuleNew <- function(modulePkg, jaspRoot, moduleLibrary, repos = getOption("repos"), updatePackages = Sys.getenv("JASP_UPDATE_PKGS", unset = "false"),
                                 recordPackages = "localJasp", respectModuleLockfile = TRUE) {
  assertValidJASPmodule(modulePkg)

  # avoid contaminating options
  oldOptions <- options()
  on.exit({
    newOptions <- options()
    oldOptions[setdiff(names(newOptions), names(oldOptions))] <- list(NULL)
    options(oldOptions)
  }, add = TRUE, after = FALSE)

  r <- getOption("repos")
  r["CRAN"] <- repos
  options(repos = r)

  setupRenv(moduleLibrary, modulePkg)

  return(pkgbuild::with_build_tools(
    installModuleNew(modulePkg, jaspRoot, moduleLibrary, updatePackages = updatePackages, recordPackages = recordPackages,
                     respectModuleLockfile = respectModuleLockfile),
    required = FALSE
  ))

}

map <- function(x, f, ...) {
  res <- lapply(x, f, ...)
  names(res) <- x
  res
}


#' Install a Jasp module and update its dependencies.
#'
#' @param modulePath local path to jasp module, e.g., ~/github/jasp-desktop/Modules/jaspDescriptives (note that this does not need to be inside jasp-desktop)
#' @param jaspRoot local path to clone of jasp-desktop, e.g., ~/github/jasp-desktop
#' @param moduleLibrary where to install the module
#' @param lockfilePath optional, path to a lockfile.
#' @param updatePackages optional, whether to update the non JASP R package dependencies. Defaults to false, but see Details.
#' @param recordPackages optional, whether to record all package dependencies as they are installed ("all"), or whether to change records of Jasp modules to the locally checked out version ("localJasp").
#' @param recurseJaspDependencies optional, whether to use the local versions of indirect Jasp module dependencies. Defaults to TRUE.
#' @param useLocalLockfile optional, whether to use the lockfile present in modulePath. Defaults to false.
#' @param prompt optional, show renv promts. Ignored when not running in interactive mode (e.g., inside Jasp).
#'
#' @details \code{installModuleNew} does four things:
#' 1. Install a Jasp module from scratch using locally checked out jasp modules. When `recurseJaspDependencies` is TRUE (the default), then it is ensured that any indirect dependencies on other jasp modules als use the locally checked out versions.
#' 2. Update the Jasp module dependencies when locally checked out sources change. For example, when jaspBase is updated but jaspDescriptives not, this function ensures that jaspDescriptives will use the new version of jaspBase.
#' 3. Optionally, update non Jasp R package dependencies.
#' 4. TODO: Optionally, install a Jasp module using the lockfile specified in the source location. This ignores any options set for the previous three options.
#'
#' Possible values for \code{updatePackages} are "true", "false", "daily", "triweekly", "biweekly", "weekly", "fortnightly", "monthly",
#' "monday", "tuesday", ... "sunday", or a combination of weekdays, "monday;wednesday;friday".
#' Anything value that is not understood is interpreted as FALSE (with a warning).
#' The date of the previous update is determined by looking at the last date where \code{lockfilePath} was modified.
#'
#' @rdname installModuleNew
#'
#' @return returns \code{NULL}.
#'
installModuleNew <- function(
    modulePath, jaspRoot, moduleLibrary,
    lockfilePath            = NULL,
    updatePackages          = Sys.getenv("JASP_UPDATE_PKGS", unset = "false"),
    recordPackages          = c("localJasp", "all"),
    recurseJaspDependencies = TRUE,
    useLocalLockfile        = FALSE,
    respectModuleLockfile   = TRUE,
    prompt                  = FALSE,
    verbose                 = 1L
  ) {

  # # avoid contamination
  # originalOptions <- options()
  # on.exit({
  #   options("JASP_LOCAL_PATHS" = NULL)
  #   options("JASP_LOCAL_COMMIT_HASHES" = NULL)
  #   # .Options <- .Options[setdiff(names(.Options), c("renv.cache.linkable", "JASP_LOCAL_PATHS", "JASP_LOCAL_COMMIT_HASHES"))]
  #   # options(originalOptions)
  # }, add = TRUE, after = FALSE)

  recordPackages <- match.arg(recordPackages)
  moduleLibrary <- normalizePath(moduleLibrary) # simplify "Modules/../Modules/"

  if (file.exists(file.path(modulePath, "renv.lock")) && respectModuleLockfile) {
    return(installModuleNewFromModuleLockfile(modulePath, moduleLibrary))
  }

  moduleName   <- basename(modulePath)
  localPaths   <- getLocalPaths(jaspRoot)
  deps         <- renv::dependencies(file.path(modulePath, "DESCRIPTION"), progress = FALSE)
  jaspPkgs     <- c(moduleName, intersect(deps$Package, names(localPaths)))
  commitHashes <- getModuleHashes(localPaths)
  updatePkgs   <- parseUpdatePkgs(jaspRoot, updatePackages)
  maybeSilence <- if (verbose >= 2) identity else silence

  # Could be useful to manually add github records
  # commitSHas   <- getModuleShas(localPaths)

  # unclear if setting .libPaths is necessary (probably necessary, otherwise spurious packages are recorded!)
  oldLibPaths <- .libPaths()
  .libPaths(moduleLibrary)
  on.exit(.libPaths(oldLibPaths), add = TRUE, after = FALSE)

  if (recurseJaspDependencies)
    jaspPkgs <- findRecursiveJaspDependencies(moduleName, localPaths, jaspPkgs)

  # TODO: filter localPaths using jaspPkgs and then only compute hashes/ shas for those


  if (verbose >= 1)
    cat("\nLocal jasp dependencies: ", paste(jaspPkgs, collapse = ", "), ".\n", sep = "")

  # perhaps we want to just keep the default though
  # if (is.null(lockfilePath)) lockfilePath <- file.path(moduleLibrary, sprintf("%s.renv.lock", moduleName))
  if (is.null(lockfilePath)) lockfilePath <- file.path(moduleLibrary, "renv.lock")

  reusingLockfile <- FALSE
  df <- data.frame(identical = logical(length(jaspPkgs)), row.names = jaspPkgs) # only exists for pretty printing
  if (file.exists(lockfilePath)) {

    lockfileData <- renv:::renv_lockfile_read(lockfilePath)

    `%||%` <- function(x, y) if (is.null(x)) y else x
    df$lockfile  <- vapply(jaspPkgs, FUN.VALUE = character(1L), function(pkg) lockfileData$Packages[[pkg]]$Hash %||% "missing")
    df$local     <- vapply(jaspPkgs, FUN.VALUE = character(1L), function(pkg) commitHashes[[pkg]] %||% "missing")
    # df$local <- character(nrow(df))
    # for (i in seq_len(nrow(df))) {
    #   df$local[i] <- if (recordPackages == "localJasp") {
    #     commitHashes[[pkg]] %||% "missing"
    #   } else {
    #     if (df$lockfile[i] == "missing") {
    #       "missing"
    #     } else {
    #       # TODO: if the cache exists for the custom hash but not for the hash renv creates
    #       # we could copy the directory (rather than reinstalling)
    #       # I'm not sure if that works on macOS, so for now I'm not doing this
    #       descriptionPath <- file.path(localPaths[rownames(df)[i]], "DESCRIPTION")
    #       moduleDescription <- renv:::renv_description_read(descriptionPath)
    #       moduleVersion <- moduleDescription$Version
    #       moduleHash <- df$lockfile[i]
    #       cacheDir <- file.path(renv::paths$cache(), moduleName, moduleVersion, moduleHash, moduleName)
    #       if (dir.exists(cacheDir) && file.exists(file.path(cacheDir, "DESCRIPTION"))) {
    #         renv:::renv_hash_description(file.path(cacheDir, "DESCRIPTION"))
    #       } else {
    #         "missing"
    #       }
    #     }
    #   }
    # }
    df$identical <- df$lockfile == df$local & df$lockfile != "missing"
    if (any(df$identical)) # ensure the folders also exist
      df$identical[df$identical] <- df$identical[df$identical] & dir.exists(file.path(moduleLibrary, jaspPkgs[df$identical]))

    # if the module didn't change then we can reuse the lockfile, otherwise we reinstall everything from scratch
    reusingLockfile <- df[[moduleName, "identical"]]

    if (reusingLockfile) {

      if (verbose >= 1)
        cat("Package hash in lockfile identical to local folder, reusing already existing lockfile\n")

    } else {

      if (verbose >= 1)
        cat("Package hash in lockfile different from local folder, reinstalling from scratch\n")

      # hacky but leaves no trails
      unlink(moduleLibrary, recursive = TRUE, expand = FALSE)
      dir.create(moduleLibrary)
      # fs::dir_delete(moduleLibrary)
      # fs::dir_create(moduleLibrary)
      # file.remove
      # fs::link_delete(fs::dir_ls(moduleLibrary, type = "link"))
      # fs::link_delete(fs::dir_ls(moduleLibrary, type = "all"))
      # fs::link_delete(fs::dir_ls(moduleLibrary, type = "all"))
      #
      # fs::file_delete(c(
      #   list.dirs(moduleLibrary, full.names = TRUE, recursive = FALSE),
      #   # checking for existence avoids warnings in file.remove
      #   if (file.exists(lockfilePath)) lockfilePath,
      #   if (file.exists(file.path(moduleLibrary, ".renv"))) file.path(moduleLibrary, ".renv")
      # ))
    }
    allIdenticalJaspPkgs <- all(df$identical)

    lockfileMatchesRecordPackages <- identical(
      lockfileData$Packages[[moduleName]]$Source,
      if (recordPackages == "localJasp") "Local" else "GitHub"
    )

  } else {
    allIdenticalJaspPkgs <- FALSE
    lockfileMatchesRecordPackages <- FALSE
  }


  if (verbose >= 1) {
    prettyCat(c("allIdenticalJaspPkgs", "updatePkgs", "lockfileMatchesRecordPackages"), c(allIdenticalJaspPkgs, updatePkgs, lockfileMatchesRecordPackages))
    oldWidth <- getOption("width")
    options(width = 200)
    cat("hashes per module\n")
    print(df)
    options(width = oldWidth)
  }


  if (allIdenticalJaspPkgs && !updatePkgs && lockfileMatchesRecordPackages)
    return("succes")

  options("renv.cache.linkable"      = TRUE)
  options("JASP_LOCAL_PATHS"         = localPaths)
  options("JASP_LOCAL_COMMIT_HASHES" = commitHashes)
  hackRenv()

  if (file.exists(file.path(modulePath, ".RBuildignore"))) {
    rBuildIgnorePath <- file.path(modulePath, ".RBuildignore")
    tempRBuildIgnorePath <- tempfile()
    copyOfRBuildIgnore <- file.copy(rBuildIgnorePath, tempRBuildIgnorePath)
    on.exit(file.copy(tempRBuildIgnorePath, rBuildIgnorePath), add = TRUE, after = FALSE)
  }

  if (!allIdenticalJaspPkgs) {

    if (verbose >= 1)
      cat("Updating and installing jasp modules and new R package dependencies but not (yet) updating older dependencies\n")


    tempLockfilePath <- tempfile(fileext = "renv.lock")
    on.exit(file.remove(tempLockfilePath), add = TRUE, after = FALSE)
    records <- maybeSilence(renv::snapshot(project = modulePath, library = moduleLibrary, lockfile = tempLockfilePath, type = "explicit", prompt = prompt, force = !interactive()))

    hasExistingLockfile <- file.exists(lockfilePath)
    if (hasExistingLockfile) {
      currentLockfile <- renv:::renv_lockfile_read(file = lockfilePath)
      pkgs2skip <- setdiff(names(currentLockfile$Packages), jaspPkgs)
      pkgs2skip <- intersect(names(records$Packages), pkgs2skip)
      records$Packages[pkgs2skip] <- currentLockfile$Packages[pkgs2skip]
    } else {
      file.copy(from = tempLockfilePath, to = lockfilePath)
    }

    # construct custom entries for lockfile
    descriptionInfo <- map(jaspPkgs, function(x) renv:::renv_description_read(localPaths[x]))
    jaspRecords <- map(jaspPkgs, function(pkg) {
      list(
        Package      = descriptionInfo[[pkg]]$Package,
        Version      = descriptionInfo[[pkg]]$Version,
        Source       = "Local",
        RemoteType   = "local",
        RemoteUrl    = localPaths[[pkg]],
        Cacheable    = TRUE,
        Hash         = commitHashes[[pkg]],
        Requirements = records$Packages[[pkg]]$Requirements
      )
    })

    recordsOriginal <- records
    records$Packages[names(jaspRecords)] <- jaspRecords
    maybeSilence(renv::record(records = records$Packages, lockfile = lockfilePath))

    cat("restoring library\n")
    renv::restore(library = moduleLibrary, lockfile = lockfilePath, project = moduleLibrary,
                  clean = TRUE, prompt = prompt)

  }

  if (updatePkgs) {

    if (verbose >= 1)
      cat("updating R package dependencies\n")
    renv::update(library = .libPaths(), exclude = jaspPkgs, project = moduleLibrary)

  } else {

    if (verbose >= 1)
      cat("not updating R package dependencies\n")

  }

  lockfile <- maybeSilence(renv::snapshot(lockfile = lockfilePath, type = "all", project = moduleLibrary, library = moduleLibrary, prompt = prompt, force = !interactive()))
  if (recordPackages == "all") {

    records2update <- createGitHubRecordFromLocalJaspPkgs(localPaths[jaspPkgs], lockfile, commitHashes)
    lockfile <- renv::record(records2update, lockfile = lockfile, project = moduleLibrary)
    # TODO: consider not recording the module itself inside the lockfile?

    # ideally we only call renv:::renv_lockfile_write(lockfile, lockfilePath), but see https://github.com/rstudio/renv/issues/1035
    if (packageVersion("renv") <= package_version("0.15.5"))
      file.remove(lockfilePath)

    renv:::renv_lockfile_write(lockfile, lockfilePath)

  }

  return("success")
}

getModuleHashes <- function(paths) {

  res <- sapply(paths, getModuleHash)
  names(res) <- basename(paths)
  return(res)
  #
  # jaspRoot <- normalizePath(jaspRoot)
  # hashes <- character()
  #
  # hashes["jaspBase"]   <- getModuleHash(file.path(jaspRoot, "Engine", "jaspBase"))
  # hashes["jaspGraphs"] <- getModuleHash(file.path(jaspRoot, "Engine", "jaspGraphs"))
  #
  # modulePaths <- getModulesPaths(jaspRoot)
  # for (path in modulePaths)
  #   hashes[basename(path)] <- getModuleHash(path)
  #
  # hashes
}

getModuleShas <- function(paths) {

  res <- lapply(paths, getModuleSha)
  names(res) <- basename(paths)
  return(res)

}

getModuleSha <- function(path) {

  cmd0 <- sprintf("git -C %s rev-parse HEAD", path)
  resCmd0 <- system(cmd0, intern = TRUE)

}

getModulesPaths <- function(jaspRoot) {
  candidates <- dir(file.path(jaspRoot, "Modules"), full.names = TRUE)
  Filter(function(path) file.exists(file.path(path, "DESCRIPTION")) && file.exists(file.path(path, "inst", "Description.qml")), candidates)
}

getLocalPaths <- function(jaspRoot) {

  # for dynamic modules
  if (isFALSE(jaspRoot) || !dir.exists(jaspRoot)) {
    if (!dir.exists(jaspRoot))
      warning("getLocalPaths got path \"", jaspRoot, "\" but it does not exist!", domain = NA)
    return(FALSE)
  }

  jaspRoot <- normalizePath(jaspRoot)
  paths <- character()
  paths["jaspBase"]   <- file.path(jaspRoot, "Engine", "jaspBase")
  paths["jaspGraphs"] <- file.path(jaspRoot, "Engine", "jaspGraphs")

  modulePaths <- getModulesPaths(jaspRoot)
  paths[basename(modulePaths)] <- modulePaths
  paths

}

getModuleHash <- function(path) {
  return(createMd5Sums(path, individual = FALSE, includeQML = TRUE))
  # system(sprintf("cd %s && git rev-parse HEAD", path), intern = TRUE)
}

parseUpdatePkgs <- function(lockfilePath, updatePackages = Sys.getenv("JASP_UPDATE_PKGS", unset = "false")) {

  if (is.logical(updatePackages))
    return(isTRUE(updatePackages))
  if (!is.character(updatePackages)) {
    warning("updatePackages or JASP_UPDATE_PKGS was set to something other than a string or boolean, and is thus ignored.", domain = NA)
    return(FALSE)
  }

  updatePackages <- tolower(updatePackages)
  if (updatePackages == "false")
    return(FALSE)
  else if (updatePackages == "true")
    return(TRUE)

  # so that R always returns the same date information. This mainly matters for weekdays(), which otherwise returns the translated day
  oldValue <- Sys.getlocale(category = "LC_TIME")
  Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF-8")
  on.exit(Sys.setlocale(category = "LC_TIME", locale = oldValue), add = TRUE, after = FALSE)

  oldDate <- as.Date(file.mtime(lockfilePath))
  now <- Sys.Date()
  diffInDays <- (now - oldDate)

  switch(updatePackages,
    "daily"       = return(diffInDays >= 1L),
    "triweekly"   = return(diffInDays >= 2L),
    "biweekly"    = return(diffInDays >= 4L),
    "weekly"      = return(diffInDays >= 7L),
    "fortnightly" = return(diffInDays >= 14L),
    "monthly"     = return(dateToMonth(now) - dateToMonth(oldDate) >= 1L)
  )

  currentDay <- tolower(weekdays(now))
  splitUpdatePackages <- strsplit(updatePackages, ";", fixed = TRUE)[[1L]]
  if (currentDay %in% splitUpdatePackages)
    return(diffInDays >= 1L)

  warning("updatePackages or JASP_UPDATE_PKGS was set to \"", updatePackages, "\" but this value was not understood and thus ignored.", domain = NA)
  return(FALSE)

}

dateToMonth <- function(x) {
  as.numeric(format(x, format="%m"))
}

installModuleNewFromModuleLockfile <- function(modulePath, moduleLibrary, prompt = FALSE) {

  renv::restore(library = moduleLibrary, lockfile = file.path(moduleLibrary, "renv.lock"),
                project = moduleLibrary, clean = TRUE, prompt = prompt)

  return("success")

}

hackRenv <- function() {
  # ensures that JASP packages are cached
  renv_remotes_resolve_path_impl_override <- function(path) {
    desc <- renv:::renv_description_read(path)

    # start of changes
    cat(sprintf("path = %s\n", path))
    Cacheable <- !isFALSE(getOption("JASP_LOCAL_PATHS", FALSE)) && basename(path) %in% names(getOption("JASP_LOCAL_PATHS"))
    list(Package = desc$Package, Version = desc$Version, Source = "Local",
         RemoteType = "local", RemoteUrl = path, Cacheable = TRUE)
    # end of changes
  }

  # ensures the hash for JASP packages is the commit hash and not based on the DESCRIPTION file
  renv_snapshot_description_override <- function(path = NULL, package = NULL) {
    `%||%` <- renv:::`%||%`
    path <- path %||% renv:::renv_package_find(package)

    dcf <- renv:::catch(renv:::renv_description_read(path, package))
    if (inherits(dcf, "error"))
      return(dcf)

    source <- renv:::renv_snapshot_description_source(dcf)
    dcf[names(source)] <- source
    required <- c("Package", "Version", "Source")
    missing <- renv:::renv_vector_diff(required, names(dcf))
    if (length(missing)) {
      fmt <- "required fields %s missing from DESCRIPTION at path '%s'"
      msg <- sprintf(fmt, paste(shQuote(missing), collapse = ", "), path)
      return(simpleError(msg))
    }
    # start of changes
    commitHashes <- getOption("JASP_LOCAL_COMMIT_HASHES", FALSE)
    if (!isFALSE(commitHashes) && dcf$Package %in% names(commitHashes)) {
      cat(sprintf("renv_snapshot_description: Package: %shash: %s\n", format(dcf$Package, width = 20), commitHashes[[dcf$Package]]))
      dcf[["Hash"]] <- commitHashes[[dcf$Package]]
    } else {
      dcf[["Hash"]] <- renv:::renv_hash_description(path)
    }
    # end of changes

    fields <- c("Depends", "Imports", "LinkingTo")
    for (field in fields) {
      if (!is.null(dcf[[field]])) {
        parts <- strsplit(dcf[[field]], "\\s*,\\s*", perl = TRUE)[[1L]]
        parts <- gsub("\\s+", " ", parts, perl = TRUE)
        dcf[[field]] <- parts[nzchar(parts)]
      }
    }
    git <- grep("^git", names(dcf), value = TRUE)
    remotes <- grep("^Remote", names(dcf), value = TRUE)
    extra <- c("Repository", "OS_type")
    all <- c(required, fields, extra, remotes, git, "Hash")
    keep <- renv:::renv_vector_intersect(all, names(dcf))
    as.list(dcf[keep])
  }

  renv_retrieve_explicit_override <- function(record) {
    `%||%` <- renv:::`%||%`
    source <- record$Path %||% record$RemoteUrl %||% ""
    resolved <- renv:::catch(renv:::renv_remotes_resolve_path(source))
    if (inherits(resolved, "error"))
      return(FALSE)
    normalized <- renv:::renv_path_normalize(source, winslash = "/", mustWork = TRUE)
    resolved$Source <- "Local"
    # start of changes
    commitHashes <- getOption("JASP_LOCAL_COMMIT_HASHES", FALSE)
    if (!isFALSE(commitHashes) && record$Package %in% names(commitHashes)) {
      cat(sprintf("renv_retrieve_explicit: Package: %shash: %s\n", format(record$Package, width = 20), commitHashes[[record$Package]]))
      record$Hash <- commitHashes[[record$Package$Package]]
    }
    # end of changes
    renv:::renv_retrieve_successful(resolved, normalized)
  }

  # hijack renv:::renv_description_read to add the commits to use
  # renv_description_read_override <- function (path = NULL, package = NULL, subdir = NULL, ...) {
  #   `%||%` <- renv:::`%||%`
  #   path <- path %||% find.package(package)
  #   if (!file.exists(path)) {
  #     fmt <- "%s does not exist"
  #     renv:::stopf(fmt, renv:::renv_path_pretty(path))
  #   }
  #   if (!renv:::renv_path_absolute(path))
  #     path <- renv:::renv_path_normalize(path)
  #   info <- renv:::renv_file_info(path)
  #   if (identical(info$isdir, TRUE)) {
  #     components <- c(path, if (nzchar(subdir %||% "")) subdir,
  #                     "DESCRIPTION")
  #     path <- paste(components, collapse = "/")
  #   }
  #   res <- renv:::filebacked(scope = "DESCRIPTION", path = path, callback = renv:::renv_description_read_impl,
  #              subdir = subdir, ...)
  #   browser()
  #   commitShas <- getOption("JASP_LOCAL_COMMIT_SHAS", FALSE)
  #   # assumes only jasp modules depend on jasp modules
  #   if (!isFALSE(commitShas) && res$Package %in% names(commitShas)) {
  #     for (i in seq_along(commitShas))
  #       res$Remotes <- gsub(pattern = names(commitShas)[i], replacement = commitShas[i], x = res$Remotes, fixed = TRUE)
  #   }
  #   res
  # }

  assignFunctionInPackage(renv_remotes_resolve_path_impl_override, "renv_remotes_resolve_path_impl", "renv")
  assignFunctionInPackage(renv_snapshot_description_override,      "renv_snapshot_description",      "renv")
  assignFunctionInPackage(renv_retrieve_explicit_override,         "renv_retrieve_explicit",         "renv")
  # assignFunctionInPackage(renv_description_read_override,          "renv_description_read",          "renv")
}

renv_remotes_resolve_path_impl_backup <- renv:::renv_remotes_resolve_path_impl
renv_snapshot_description_backup      <- renv:::renv_snapshot_description
renv_retrieve_explicit_backup         <- renv:::renv_retrieve_explicit

unhackRenv <- function() {
  # TODO: this does not appear to work
  assignFunctionInPackage(renv_remotes_resolve_path_impl_backup, "renv_remotes_resolve_path_impl", "renv")
  assignFunctionInPackage(renv_snapshot_description_backup,      "renv_snapshot_description",      "renv")
  assignFunctionInPackage(renv_retrieve_explicit_backup,         "renv_retrieve_explicit",         "renv")
}

findRecursiveJaspDependencies <- function(moduleName, localPaths, jaspPkgs) {
  seen <- moduleName
  for (i in 1:30) {

    pkgsToGetDescriptionFrom <- localPaths[names(localPaths) %in% setdiff(jaspPkgs, seen)]

    newJaspPkgs <- Reduce(union, sapply(pkgsToGetDescriptionFrom, function(pkg) {
      intersect(renv::dependencies(file.path(pkg, "DESCRIPTION"), progress = FALSE)$Package, names(localPaths))
    }))
    newJaspPkgs <- setdiff(newJaspPkgs, c(jaspPkgs, seen))
    if (length(newJaspPkgs) == 0L)
      break

    seen <- c(jaspPkgs, seen)
    jaspPkgs <- c(jaspPkgs, newJaspPkgs)

  }

  if (i == 30)
    warning("Failed to recursively resolve all jasp dependencies after 30 iterations!")

  return(jaspPkgs)
}

createGitHubRecordFromLocalJaspPkgs <- function(paths, lockfile, commitHashes) {
  result <- list()
  for (path in paths)
    result[[basename(path)]] <- createGitHubRecordFromLocalJaspPkg(path, lockfile, commitHashes)
  return(result)
}

createGitHubRecordFromLocalJaspPkg <- function(path, lockfile, commitHashes) {

  description <- renv:::renv_description_read(file.path(path, "DESCRIPTION"))
  record <- lockfile$Packages[[description$Package]]

  newRecord <- c(
    record[setdiff(names(record), c("Source", "RemoteType", "RemoteUrl", "Remotes", "Hash"))],
    list(
      Source         = "GitHub",
      RemoteType     = "github",
      RemoteRepo     = description$Package,
      RemoteSha  = getModuleSha(path),
      # TODO: these should be obtained dynamically
      RemoteRef      = "master",
      RemoteUsername = "jasp-stats",
      Hash           = commitHashes[[record$Package]]
    )
  )

  return(newRecord)

}

silence <- function(expr) {
  ff <- tempfile()
  f <- file(ff, open = "wt")
  sink(f, type = "output")
  sink(f, type = "message", append = TRUE)
  on.exit({
    sink(type = "output")
    sink(type = "message")
    close(f)
    file.remove(ff)
  })
  force(expr)
}

prettyCat <- function(keys, values = NULL) {
  if (is.null(values)) {
    keys   <- names(keys)
    values <- unname(keys)
  }
  nchars <- nchar(keys)
  pads <- strrep(" ", max(nchars) - nchars)
  for (i in seq_along(keys))
    cat(sprintf("%s:%s %s\n", keys[i], pads[i], values[[i]]))

}
