#' @title Connect to a dolt database
#'
#' @description `dolt_remote()` is a DBI Driver to connect to a remote dolt
#'server via
#' a port. It, `DoltDriver`,
#' and `DoltConnection` class are wrappers around the around classes and methods
#' from the [`RMariaDB`][RMariaDB::MariaDB] package.
#'
#' Most parameters can be specified with
#' [package configuration environment variables][dolt_vars].
#'
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
#' @rdname dolt-connection
#' @importClassesFrom RMariaDB MariaDBDriver MariaDBConnection
#' @family connections
dolt_remote <- function() {
  new("DoltDriver")
}

#' @export
#' @rdname dolt-connection
setClass("DoltDriver", contains = "MariaDBDriver")

#' @export
#' @rdname dolt-connection
setMethod("dbUnloadDriver", "DoltDriver", function(drv, ...) { TRUE })

#' @export
#' @param object a connection object
#' @rdname dolt-connection
setMethod("show", "DoltDriver", function(object) { cat("<DoltDriver>\n") })

#' @export
#' @rdname dolt-connection
setClass("DoltResult", contains = "MariaDBResult")

#' @export
#' @rdname dolt-connection
setClass("DoltConnection", contains = "MariaDBConnection",
         slots = c(port = "integer", username = "character"))

#' @export
#' @rdname dolt-connection
setMethod("dbConnect", "DoltDriver",
          function(drv = dolt_remote(),
                   dbname = Sys.getenv("DOLT_DIR", "doltdb"),
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
