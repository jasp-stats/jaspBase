postInstallFixes <- function(folderToFix) {
  if(length(ls(all.names=TRUE,pattern="\\..postProcessLibraryModule")) > 0 ) #We seem to be running in JASP
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
    #print(paste0("Calling JASPEngine as: '", jaspEngineCall ,"'"))
    system(jaspEngineCall)

    if(getOS() == "windows")
        Sys.setenv("PATH"=old_PATH);
  }
}

#' @export
installJaspModule <- function(modulePkg, libPathsToUse, moduleLibrary, repos, onlyModPkg, force = FALSE, cacheAble=TRUE) {
  assertValidJASPmodule(modulePkg)

  r <- getOption("repos")
  r["CRAN"] <- repos
  options(repos = r)

  if (!(force || md5SumsChanged(modulePkg, moduleLibrary))) {
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
      else                            installJaspModuleFromDescription(modulePkg, libPathsToUse, moduleLibrary, repos, onlyModPkg, cacheAble=cacheAble)
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

  lockFileModule <- file.path(modulePkg,         "renv.lock")
  lockFileTemp   <- file.path(moduleLibraryTemp, "renv.lock")
  file.copy(from = lockFileModule, to = lockFileTemp, overwrite = TRUE)

  setupRenv(moduleLibrary)

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
                lockfile = lockFileTemp, clean = TRUE,
                prompt   = prompt)

  moduleInfo         <- getModuleInfo(modulePkg)
  correctlyInstalled <- installModulePkg(modulePkg, moduleLibrary, prompt, moduleInfo, cacheAble=cacheAble)

  if (correctlyInstalled)
    writeMd5Sums(modulePkg, moduleLibrary)

  renv::snapshot(
    project  = moduleLibraryTemp,
    lockfile = lockFileTemp,
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
      lockfile = lockFileTemp,
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

installJaspModuleFromDescription <- function(modulePkg, libPathsToUse, moduleLibrary, repos, onlyModPkg, prompt = interactive(), cacheAble=TRUE) {

  print("Installing module with DESCRIPTION file")

  if (!dir.exists(moduleLibrary))
    if (!dir.create(moduleLibrary))
      stop("failed to create moduleLibrary!")

  moduleLibraryTemp <- file.path(moduleLibrary, ".renv_temp")

  if (!dir.exists(file.path(moduleLibraryTemp, "renv")))
    dir.create(file.path(moduleLibraryTemp, "renv"), recursive = TRUE)

  setupRenv(moduleLibrary)

  # make renv blind for other libPaths
  #old.lib.loc <- .libPaths()
  #on.exit(assign(".lib.loc", old.lib.loc, envir = environment(.libPaths)))
  #assign(".lib.loc", moduleLibrary, envir = environment(.libPaths))

  # TODO: this is not very efficient because renv::install looks up the remotes on github...
  # there is a better way but it requires us to mess with renv's internals or to be more explicit about pkgs
  renv::hydrate(library = moduleLibrary, project = modulePkg)
  renv::install(project = modulePkg, library = moduleLibrary, prompt = prompt)

  correctlyInstalled <- installModulePkg(modulePkg, moduleLibrary, prompt, cacheAble=cacheAble)
  if (correctlyInstalled)
    writeMd5Sums(modulePkg, moduleLibrary)

  if (unlink(moduleLibraryTemp, recursive = TRUE)) # 0/ FALSE for success
    warning(sprintf("Failed to remove temporary module libary at %s", moduleLibraryTemp))

  return("succes!")

}

installModulePkg <- function(modulePkg, moduleLibrary, prompt = interactive(), moduleInfo = NULL, cacheAble=TRUE) {

  if (is.null(moduleInfo))
    moduleInfo <- getModuleInfo(modulePkg)
  record <- recordFromModule(modulePkg, moduleInfo, cacheAble=cacheAble)

 # print(paste0("Im telling renv to install to '", moduleLibrary, "'"))

  renv::install(record, library = moduleLibrary, rebuild = TRUE, prompt = prompt)
  TRUE

}

assertValidJASPmodule <- function(modulePkg) {

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

getModuleInfo <- function(modulePkg) {
  pkgDescr <- file.path(modulePkg, "DESCRIPTION")
  if (!file.exists(pkgDescr))
    stop("Your module contains no DESCRIPTION file")

  return(read.dcf(file.path(modulePkg, "DESCRIPTION"))[1, ])

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

setupRenv <- function(moduleLibrary) {

  # renv adds e.g,. "R-3.6/x86_64-pc-linux-gnu" to all paths (R-version/os) and we don't need that
  assignFunctionInPackage(
    fun     = function() return(""),
    name    = "renv_bootstrap_platform_prefix",
    package = "renv"
  )

  cachePaths <- strsplit(Sys.getenv("RENV_PATHS_CACHE"), .Platform$path.sep)

  for(cachePath in cachePaths[[1]]) #strsplit is vectorized but we only give it a single string, so index to that single first one
    if (!dir.exists(cachePath))
     stop(sprintf("A cache is supposed to be at '%s' but it does not exist!", cachePath))
  
  Sys.setenv("RENV_PATHS_LIBRARY" = moduleLibrary)

  print("Using the following paths:")
  for(name in names(renv::paths))
    print(sprintf("%s:%s%s", name, paste0(rep(" ", 12 - nchar(name)), collapse=""), renv::paths[[name]]()))

  options(install.opts = "--no-docs --no-test-load"); #make sure we do not do a test-load, because this will not work on mac. the rpaths still need to be fixed

  #Try to nudge renv towards installing binaries when possible
   if(getOS() == "windows" || getOS() == "osx")
    options(pkgType = "binary");

  addRenvBeforeAfterDispatch()
}
