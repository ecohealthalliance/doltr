#' Driver for local dolt database served from a directory
#'
#' @keywords internal
#' @export
#' @import DBI
#' @import methods
#' @import RMariaDB
#' @importClassesFrom RMariaDB MariaDBDriver
#' @noRd
setClass("DoltLocalDriver", contains = "DoltDriver")

#' @export
#' @noRd
setMethod("dbUnloadDriver", "DoltLocalDriver", function(drv, ...) {
  TRUE
})

#' @export
#' @noRd
setMethod("show", "DoltLocalDriver", function(object) {
  cat("<DoltLocalDriver>\n")
})

#' @export
#' @noRd
#' @rdname dolt-local-class
dolt_local <- function() {
  new("DoltLocalDriver")
}

#' @noRd
setOldClass("dolt_server")

#' @export
#' @noRd
setClass("DoltLocalConnection", contains = "DoltConnection",
         slots = c(
           dir = "character",
           server = "dolt_server"))

#' @export
#' @noRd
setClass("DoltLocalResult", contains = "DoltResult")

#' @title Connect to a local dolt database directory
#' @description `dolt_local()` creates a `DoltLocalDriver`, which can generate
#' a `DoltLocalConnection`.  Unlike [dolt()] and `DoltDriver`, this version
#' takes a directory name for an on-disk dolt database, starts and manages a
#'  [dolt SQL server][dolt_server()] in the background serving that directory,
#'  connects to it and returns the connection. Parameters govern both the server
#'  and connection
#'
#' `doltdb(...)` is a convenience function for `dbConnect(dolt_local),...)`
#'
#' Connections to the same server, and servers of the same directory, are cached
#' and re-used, so re-connecting to the same directory is fast and
#' `doltdb()` can be invoked repeatedly and in simultaneous sessions, with little
#' overhead. Multi-user or other, more complicated networking steups should
#' use `dolt()` and `dolt_server()` directly.
#'
#' @param find_server if TRUE, look for an existing dolt server process serving
#'   the directory to connect to before starting a new server
#' @param find_port if TRUE, switch to a different port if `port` is used by
#' another process
#' @param ... additional arguments to pass to [dolt_server()] to configure the
#'   server
#' @rdname dolt-local-class
setMethod("dbConnect", "DoltLocalDriver",
  function(drv, dir = Sys.getenv("DOLT_DIR", "doltdb"),
           username = Sys.getenv("DOLT_USERNAME", "root"),
           password = Sys.getenv("DOLT_PASSWORD", ""),
           port = Sys.getenv("DOLT_PORT", 3306L),
           host = Sys.getenv("DOLT_HOST", "127.0.0.1"),
           find_server = TRUE,
           find_port = TRUE,
           ...) {

    #browser()
    dir = normalizePath(dir, mustWork = FALSE)
    port = as.integer(port)
    server_running <- FALSE
    # Get the cached connection
    con = mget(dir, dolt_env, ifnotfound = NA)[[1]]

    # If the cached connection exists check that it works and otherwise disconnect
    if (inherits(con, "DoltLocalConnection")) {
      if (dbIsValid(con)) {
        return(con)
      } else {
        suppressWarnings(getMethod(dbDisconnect, "MariaDBConnection")(con))
      }
      if (!ps::ps_is_running(con@server)) {
        kill_softly(con@server)
        server_running <- FALSE
      }
    }

        # If the connection doesn't have a server, try to find one
    if (!server_running && find_server) {
      running_servers <- dolt_server_find(dir = dir)
      if (nrow(running_servers)) {
        srv <- running_servers$ps_handle[[1]]
        port <- running_servers$lport[[1]]
        class(srv) <- c("dolt_server", class(srv))
        server_running <- TRUE
      }
    }

    # Otherwise start one
    if (!server_running) {
      if (!dir.exists(file.path(dir, ".dolt"))) dolt_init(dir)
      if (find_port) port <- port_fallback(port)
      srv <- dolt_server(dir = dir, username = username, password = password,
                         port = port, host = host, ...)
      Sys.sleep(0.5)
    }

    con <- dbConnect(dolt(), dbname = basename(dir), username = username,
                     password = password, host = host, port = port)
    attr(con, "dir") <- dir
    attr(con, "server") <- srv
    class(con) <- structure("DoltLocalConnection", package = "doltr")
    assign(dir, con, envir = dolt_env)
    return(con)
  })

#' @importFrom DBI dbConnect
#' @rdname dolt-local-class
#' @usage doltdb()
doltdb <- function(...) {
  dbConnect(dolt_local(), ...)
}

#' @export
#' @rdname dolt-local-class
setMethod("dbGetInfo", "DoltLocalConnection", function(dbObj, ...) {
  info <- getMethod(dbGetInfo, "DoltConnection")(dbObj)
  info$dir <- dbObj@dir
  info$server_pid <- ps::ps_pid(dbObj@server)
  info
})

#' @export
#' @rdname dolt-local-class
#' @importFrom cli cli_h1 cli_ul cli_li cli_end cli_alert_warning
setMethod("show", "DoltLocalConnection", function(object) {
  info <- dbGetInfo(object)
  cli_h1("<DoltLocalConnection> {info$dbname}")
  if (dbIsValid(object)) {
    l <- cli_ul()
    cli_li("Serving {info$dir}, PID {info$server_pid}")
    cli_li("Connected at: {info$username}@{info$host}:{info$port}")
    cli_li("HEAD: {info$head_ref} {info$head}")
    cli_li("Status:")
    cli_end(l)
    print(info$status) #TODO: pretty-print status info better, use line for working and staged,
  } else {
    cli_alert_warning("DISCONNECTED")
  }
})

#' @export
#' @rdname dolt-local-class
setMethod("dbDisconnect", "DoltLocalConnection", function(conn, ...) {
  getMethod(dbDisconnect, "MariaDBConnection")(conn)
  kill_softly(conn@server)
  rm(conn@dir, envir = dolt_env)
})


dolt_env <- new.env()
reg.finalizer(
  dolt_env,
  function(env) eapply(env, dbDisconnect, all.names = TRUE),
  onexit = TRUE)
