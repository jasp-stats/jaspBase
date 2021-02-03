
installJaspModule <- function(modulePkg, libPathsToUse, moduleLibrary, repos, onlyModPkg) {
  pkgDescr <- file.path(modulePkg, "DESCRIPTION")
  if (!file.exists(pkgDescr))
    stop("Your module contains no DESCRIPTION file")

  moduleInfo <- read.dcf(file.path(modulePkg, "DESCRIPTION"))[1, ]
  pkgName <- moduleInfo["Package"]
  modulePkg <- paste0(modulePkg, "/.")

  if (!onlyModPkg) {
    # Install dependencies:
    withr::with_libpaths(
      new  = libPathsToUse,
      code = remotes::install_deps(pkg=modulePkg, lib=moduleLibrary,  INSTALL_opts=c("--no-test-load --no-multiarch"), upgrade="never", repos=repos)
    )

    # And fix Mac OS libraries of dependencies:
    .postProcessLibraryModule(moduleLibrary)
  }

  # Remove old copy of library (because we might be reinstalling and want the find.package check on the end to fail if something went wrong)
  tryCatch(
    expr = {
      withr::with_libpaths(
        new = libPathsToUse,
        code = {
          find.package(package=pkgName)
          remove.packages(pkg=pkgName, lib=moduleLibrary)
        }
      )
    },
    error=function(e) {}
  )

  print("Module library now looks like: ")
  print(list.files(path=moduleLibrary, recursive=FALSE))

  pkgPath <- sub("\\\\", "/", modulePkg, fixed=TRUE)
  print(paste0("pkgPath: '", pkgPath, "'"))

  if (length(libPathsToUse) > 1)
    strlibPathsToUse <- paste0("c(", paste0("'", libPathsToUse, collapse = "', "), "')")
  else
    strlibPathsToUse <- paste0("'", libPathsToUse, "'")

  loadLog <- .runSeparateR(paste0("withr::with_libpaths(new=", strlibPathsToUse, ", pkgbuild::with_build_tools(install.packages(pkgs='", pkgPath, "', lib='", moduleLibrary, "', type='source', repos=NULL, INSTALL_opts=c('--no-multiarch')), required=FALSE))"))

  # Check if install worked and through loadlog as error otherwise
  tryCatch(
    expr = {
      withr::with_libpaths(
        new = moduleLibrary,
        code = {
          find.package(package=pkgName)
          return("succes!")
        }
      )
    },
    error=function(e) {
      .setLog(loadLog)
      return('fail')
    }
  )

  return(NULL)
}

# Retrieve package dependencies by parsing a DESCRIPTION file or a DESCRIPTION string
#
# Returns a named list, e.g.:
# type         package      version               remote
# Depends        R          >= 3.0.2
# Imports       stringr     >= 0.5         svn::https://github.com/hadley/stringr
# Imports       brew            *
# Imports       digest          *          foo/digest
# Imports       methods         *
# Imports       Rcpp         >= 0.11.0
# Suggests      testthat     >= 0.8.0      local::/pkgs/testthat
# Suggests      knitr           *
# LinkingTo     Rcpp
#
# Or a json string, e.g.:
# [{"type":"Depends","package":"R","version":">= 3.0.2","remote":""},
#  {"type":"Imports","package":"stringr","version":">= 0.5","remote":"svn::https://github.com/hadley/stringr"},
#  {"type":"Imports","package":"brew","version":"*","remote":""},
#  {"type":"Imports","package":"digest","version":"*","remote":"foo/digest"},
# etc]
getDepsFromDescription <- function(unparsedDescr, asJson = TRUE) {
  deps <- NULL

  descr <- .parseDescription(unparsedDescr)
  if (any(descr$has_fields(c("Imports", "Depends", "LinkingTo")))) {
    deps <- descr$get_deps()
    pkgs <- deps[["package"]]
    remotePerPkg <- character(length(pkgs))
    if (descr$has_fields("Remotes")) {
      remotes <- descr$get_remotes()
      for (i in seq_along(remotePerPkg)) {
        isRemoteForPkg <- endsWith(remotes, paste0("/", pkgs[i]))
        if (any(isRemoteForPkg))
          remotePerPkg[i] <- remotes[isRemoteForPkg]
      }
    }
    deps[["remote"]] <- remotePerPkg
  }

  if (asJson)
    deps <- jsonlite::toJSON(deps)

  return(deps)
}

.parseDescription <- function(unparsedDescr) {
  if (!nzchar(unparsedDescr))
    stop("The description contains no data")

  if (file.exists(unparsedDescr)) {
    parsedDescr <- desc::description$new(file = unparsedDescr)
  } else {
    parsedDescr <- desc::description$new(text = unparsedDescr)
  }

  return(parsedDescr)
}
