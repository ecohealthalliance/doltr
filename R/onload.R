#' Check for installation on startup
#' @noRd
.onLoad <- function(libname, pkgname) {
  if (!is_dolt_installed()) packageStartupMessage(paste0(
    "No dolt binary found. Install dolt from ",
    "<https://docs.dolthub.com/getting-started/installation>, or provide a ",
    "path to the binary with the `DOLT_PATH` environment variable."
  ))
}
