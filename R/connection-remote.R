#' @title Connect to a dolt database
#'
#' @description `dolt_remote()` is a DBI Driver to connect to a dolt server via
#' a port. It, `DoltDriver`,
#' and `DoltConnection` class are wrappers around the around classes and methods
#' from the [`RMariaDB`][RMariaDB::MariaDB] package.
#'
#' Most parameters can be specified with environment variables See [config].
#'
#' @usage DBI::dbConnect(doltr::dolt_remote(), ...)
#' @param drv an object of class `DoltDriver`, created by `dolt_remote()`.
#' @param dbname The database name
#' @param username The username. Defaults to "root"
#' @param password The login password.  Defaults to empty.
#' @param host The IP of the host. Defaults to the local machine, `127.0.0.1`
#' @param port The TCP port for connections. Defaults to 3306.
#' @param autocommit Whether to autocommit changes in the _SQL_ sense. That is,
#'   to flush pending changes to disk and update the working set.
#' @param ... other arguments passed to [RMariaDB::MariaDB]
#' @details Most methods fall back to those for [`RMariaDB`][RMariaDB::MariaDB].
#' @export
#' @import DBI
#' @import methods
#' @import RMariaDB
#' @importClassesFrom RMariaDB MariaDBDriver MariaDBConnection
#' @family connections
dolt_remote <- function() {
  new("DoltDriver")
}

#' @export
#' @noRd
setClass("DoltDriver", contains = "MariaDBDriver")

#' @export
#' @noRd
setMethod("dbUnloadDriver", "DoltDriver", function(drv, ...) { TRUE })

#' @export
#' @noRd
setMethod("show", "DoltDriver", function(object) { cat("<DoltDriver>\n") })

#' @export
#' @noRd
setClass("DoltResult", contains = "MariaDBResult")

#' @export
#' @noRd
setClass("DoltConnection", contains = "MariaDBConnection",
         slots = c(port = "integer", username = "character"))

#' @export
#' @rdname dolt_remote
setMethod("dbConnect", "DoltDriver",
          function(drv, dbname = Sys.getenv("DOLT_DIR", "doltdb"),
                   username = Sys.getenv("DOLT_USERNAME", "root"),
                   password = Sys.getenv("DOLT_PASSWORD", ""),
                   host = Sys.getenv("DOLT_HOST", "127.0.0.1"),
                   port = Sys.getenv("DOLT_PORT", 3306L),
                   autocommit = TRUE, ...) {
            conn <- dbConnect(RMariaDB::MariaDB(),
                              dbname = dbname,
                              username = username,
                              password = password,
                              port = port,
                              host = host,
                              ...)
            attr(conn, "db") <- dbname
            attr(conn, "port") <- port
            attr(conn, "username") <- username
            attr(conn, "class") <- structure("DoltConnection", package = "doltr")
            dbExecute(conn, paste0("SET @@autocommit = ", as.integer(autocommit)))
            conn
          })

#' @export
#' @noRd
setMethod("dbGetInfo", "DoltConnection", function(dbObj, ...) {
  minfo <- getMethod(dbGetInfo, "MariaDBConnection")(dbObj)
  minfo$port <- dbObj@port
  refs <- as.list(dbGetQuery(dbObj, paste0("select ",
                                           "@@", minfo$dbname, "_head_ref AS head_ref, ",
                                           "@@", minfo$dbname, "_head as head, ",
                                           "@@", minfo$dbname, "_staged as staged, ",
                                           "@@", minfo$dbname, "_working as working")))
  status <- dbGetQuery(dbObj, "select * from dolt_status")
  c(minfo, refs, list(status = status))
})

#' @export
#' @importFrom cli cli_h1 cli_ul cli_li cli_end cli_alert_warning
#' @noRd
setMethod("show", "DoltConnection", function(object) {
  if (dbIsValid(object)) {
    info <- dbGetInfo(object)
    cli_h1("<DoltConnection> {info$dbname}")
    l <- cli_ul()
    cli_li("Connected at: {info$username}@{info$host}:{info$port}")
    cli_li("HEAD: {info$head_ref} {info$head}")
    cli_end(l)
    print(dolt_statusline(info$status))
  } else {
    cli_alert_warning("DISCONNECTED")
  }
})

