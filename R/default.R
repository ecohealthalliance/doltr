#' Check if default dolt database is available.
#'
#' RMariaDB examples and tests connect to a database defined by the
#' `rs-dbi` group in `~/.my.cnf`. This function checks if that
#' database is available, and if not, displays an informative message.
#' `mariadbDefault()` works similarly but throws a testthat skip condition
#' on failure, making it suitable for use in tests.
#'
#' @export
#' @examples
#' if (doltHasDefault()) {
#'   db <- dbConnect(doltr::dolt_local())
#'   dbListTables(db)
#'   dbDisconnect(db)
#' }
doltHasDefault <- function() {
  tryCatch({
    dolt_default()
    TRUE
  }, error = function(...) {
    message(
      "Could not initialise default dolt database. If dolt is running\n",
      "check that you have a ~/.my.cnf file that contains a [rs-dbi] section\n",
      "describing how to connect to a test database."
    )
    FALSE
  })
}

#' @export
#' @rdname doltHasDefault
doltDefault <- function() {
  tryCatch({
    dolt_default()
  }, error = function(...) {
    testthat::skip("Test database not available")
  })
}

dolt_default <- function(...) {
  dbConnect(doltdb(), ...)
}
