#' Get information about a Dolt Database
#'
#' The Dolt `dbGetInfo()` returns standard information about a database connection
#' according to the [DBI specification][DBI::dbGetInfo()], as well as information
#' about the version-control status of the repository such as current branch, last
#' commit, and modified tables.  This information is
#' also displayed in the print method for a dolt connection object and in the
#' [RStudio connection pane][dolt_pane()].
#'
#' @export
#' @rdname dolt-info
#' @param dbObj the database connection
#' @param ... Other arguments to methods
#' @seealso dolt_state dolt_status dolt_last_commit dolt_pane
setMethod("dbGetInfo", "DoltConnection", function(dbObj, ...) {
  minfo <- getMethod(dbGetInfo, "MariaDBConnection")(dbObj, ...)
  minfo$port <- dbObj@port
  last_commit <- dolt_last_commit()
  state <- dolt_state(dbObj)
  status <- dolt_status(dbObj)
  c(minfo, list(last_commit = last_commit, state = state, status = status))
})

#' @export
#' @param object the database connection
#' @importFrom cli cli_h1 cli_ul cli_li cli_end cli_alert_warning
#' @rdname dolt-info
setMethod("show", "DoltConnection", function(object) {
  if (dbIsValid(object)) {
    info <- dbGetInfo(object)
    cli_h1("<DoltConnection> {info$dbname}")
    l <- cli_ul()
    cli_li("Connected at: {info$username}@{info$host}:{info$port}")
    cli_end(l)
    print(info$state)
    print(info$last_commit)
    print(info$status)
  } else {
    cli_alert_warning("DISCONNECTED")
  }
})

