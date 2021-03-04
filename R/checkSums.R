
createMd5Sums <- function(modulePkg) {

  srcFiles <- c(
    list.files(modulePkg,                   full.names = TRUE, pattern = "(NAMESPACE|DESCRIPTION)$"),
    list.files(file.path(modulePkg, "src"), full.names = TRUE, pattern = "(\\.(cpp|c|hpp|h)|(Makevars|Makevars\\.win))$"),
    list.files(file.path(modulePkg, "R"),   full.names = TRUE, pattern = "\\.R$"),
    list.files(modulePkg,                   full.names = TRUE, pattern = "renv.lock")
  )
  newMd5Sums <- tools::md5sum(srcFiles)
  newMd5Sums
}

makeMd5SumsFilename <- function(modulePkg, moduleLibrary) {
  file.path(moduleLibrary, paste(basename(modulePkg), "md5sums.rds", sep = "_"))
}

writeMd5Sums <- function(modulePkg, moduleLibrary) {
  saveRDS(createMd5Sums(modulePkg), file = makeMd5SumsFilename(modulePkg, moduleLibrary))
}

readMd5Sums <- function(modulePkg, moduleLibrary) {
  readRDS(file = makeMd5SumsFilename(modulePkg, moduleLibrary))
}

md5SumsChanged <- function(modulePkg, moduleLibrary) {

  file <- makeMd5SumsFilename(modulePkg, moduleLibrary)
  if (!file.exists(file))
    return(TRUE)

  oldMd5Sums <- readRDS(file)
  newMd5Sums <- createMd5Sums(modulePkg)
  !identical(oldMd5Sums, newMd5Sums)

}
