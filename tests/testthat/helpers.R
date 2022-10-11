mkdir <- function(x, clean = TRUE) {
  if (dir.exists(x))
    unlink(x, recursive = TRUE)
  dir.create(x, recursive = TRUE)
}

mkdirs <- function(..., clean = TRUE) {
  sapply(list(...), mkdir, clean = clean)
}

# order is intentional since y may be missing
expect_file_test <- function(failure_message, op, x, y) {
  testthat::expect(
  if (op == "-L" && getOS() == "windows") {

    fs::is_link(x)
    # negation of renv:::renv_file_broken_win32
    # res <- if (getRversion() < "4.2.0") {
    #   info <- suppressWarnings(file.info(x))
    #   (info$isdir %in% TRUE) & is.na(info$mtime)
    # } else {
    #   file.access(x, mode = 0L) == 0L & !file.exists(x)
    # }
    # !res
  } else {
    utils::file_test(op, x, y)
  }, failure_message)
}

expect_file    <- function(path, failure_message) expect_file_test(failure_message, "-f", path)
expect_dir     <- function(path, failure_message) expect_file_test(failure_message, "-d", path)
expect_symlink <- function(path, failure_message) expect_file_test(failure_message, "-L", path)
