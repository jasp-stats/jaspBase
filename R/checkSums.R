createMd5Sums <- function(modulePkg, individual = FALSE, includeQML = TRUE) {

  srcFiles <- c(
    list.files(modulePkg,                       recursive = FALSE, full.names = TRUE, pattern = "(NAMESPACE|DESCRIPTION)$"),
    list.files(file.path(modulePkg, "src"),     recursive = FALSE, full.names = TRUE, pattern = "(\\.(cpp|c|hpp|h)|(Makevars|Makevars\\.win))$"),
    list.files(file.path(modulePkg, "R"),       recursive = FALSE, full.names = TRUE, pattern = "\\.R$"),
    list.files(modulePkg,                       recursive = FALSE, full.names = TRUE, pattern = "renv\\.lock")
  )

  if (includeQML)
    srcFiles <- c(
      srcFiles,
      list.files(file.path(modulePkg, "inst"),  recursive = TRUE, full.names = TRUE, pattern = "\\.(qml|po|svg|png|jpg|md)$"),
      list.files(file.path(modulePkg, "inst"),  recursive = TRUE, full.names = TRUE, pattern = "\\qmldir$")
    )

  hashes <- rlang::hash_file(srcFiles)

  if (individual)
    return(hashes)
  else
    return(rlang::hash(hashes))

}

makeMd5SumsFilename <- function(modulePkg, moduleLibrary) {
  file.path(moduleLibrary, "..", paste(basename(modulePkg), "md5sums.rds", sep = "_"))
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

# hashRstring <- function(contents) {

  # return(rlang::hash(contents))

  # the code below is based on renv:::renv_hash_description_impl
  # but is (sadly) not reproducible acros unix <-> windows.
  # using the stuff below only makes sense if we are able to drop rlang as a dependency

  # concatenate hashes to single string
  # contents <- paste(contents, collapse = "")

  # tempfile <- tempfile("jaspBase-hash-")
  # create the file connection (use binary so that unix newlines are used
  # across platforms, for more stable hashing)
  # con <- file(tempfile, open = "wb")

  # write to the file
  # writeLines(enc2utf8(contents), con = con, useBytes = TRUE)

  # flush to ensure we've written to file
  # flush(con)

  # close the connection and remove the file
  # close(con)

  # ready for hashing
  # hash <- unname(tools::md5sum(tempfile))

  # remove the old file
  # unlink(tempfile)

  # return(hash)
# }
