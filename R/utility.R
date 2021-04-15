getOS <- function()
{
  os <- NULL
  if (!is.null(Sys.info())) {
    os <- Sys.info()["sysname"]
    if (os == "Darwin")
      os <- "osx"
  }
  else {
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  return(tolower(os))
}