#' Miscellaneous Dolt Methods
#'
#' These methods largely wrap `RMariaDB` methods with small tweaks to work with
#' Dolt databases.
#'
#' @importMethodsFrom RMariaDB dbSendQuery
#' @param conn an [DoltConnection-class] object.
#' @param res A  [DoltResult-class] object.
#' @param params A list of query parameters to be substituted into
#'   a parameterized query.
#' @param statement a character vector of length one specifying the SQL
#'   statement that should be executed.  Only a single SQL statement should be
#'   provided.
#' @param ... Unused. Needed for compatibility with generic.#' @export
#' @rdname dolt-methods
setMethod(
  "dbSendQuery", c("DoltConnection", "character"),
  function(conn, statement, params = NULL, ...) {
    rs <- getMethod(dbSendQuery, c("MariaDBConnection", "character"))(
      conn, statement, params, ...)
    attr(rs, "class") <- structure("DoltResult", package = "doltr")
    rs
  }
)

#' Makes results DoltResults rather than MariaDBResults, needed for DoltResult
#' @importMethodsFrom RMariaDB dbSendStatement
#' @export
#' @rdname dolt-methods
setMethod(
  "dbSendStatement", signature("DoltConnection", "character"),
  function(conn, statement, params = NULL, ...) {
    rs <- getMethod(dbSendStatement, c("MariaDBConnection", "character"))(
      conn, statement, params, ...)
    attr(rs, "class") <- structure("DoltResult", package = "doltr")
    rs
  }
)

#' Sets a hook in interactive mode to update watching processes (e.g. the R
#' RStudio connection pane) when DB transactions finish
#' @importMethodsFrom RMariaDB dbClearResult
#' @export
#' @rdname dolt-methods
setMethod("dbClearResult", signature("DoltResult"),
          function(res, ...) {
            if (interactive() && Sys.getboolenv("DOLT_WATCH", TRUE)) {
              on.exit(dolt_watch(res@conn))
            }
            getMethod(dbClearResult, "MariaDBResult", )(res, ...)
          }
)

#' Clear out cached connections and watching panes when closing a connection
#' @importMethodsFrom RMariaDB dbDisconnect
#' @export
#' @rdname dolt-methods
setMethod("dbDisconnect", "DoltConnection", function(conn, ...) {
  conn_name <- dolt_conn_name(conn)
  getMethod(dbDisconnect, "MariaDBConnection")(conn)
  suppressWarnings(rm(list = conn_name, envir = dolt_cache))
  suppressWarnings(rm(list = conn_name, envir = dolt_states))
  if (interactive()) close_dolt_pane(conn)
})
