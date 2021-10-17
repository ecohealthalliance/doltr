#' Find and check for the presence of a dolt binary.
#' @export
#' @rdname dolt-binary
#' @importFrom processx run
is_dolt_installed <- function() {
  ret <- run(dolt_path(), "version")
  out <- isTRUE(ret$status == 0)
  if (out) {
    stdout <- rawToChar(ret$stdout)
    attr(out, "version") <-
      numeric_version(regmatches(stdout, regexpr("[0-9\\.]+", stdout)))
  }
  out
}

#' @export
#' @rdname dolt-binary
dolt_version <- function() {
  installed <- is_dolt_installed()
  stopifnot(installed)
  attr(installed, "version")
}

#' @export
#' @rdname dolt-binary
dolt_path <- function() {
  Sys.getenv("DOLT_PATH", Sys.which("dolt"))
}
