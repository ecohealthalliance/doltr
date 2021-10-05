#' @export
#' @title Connect to a local dolt database directory
#' @description `dolt_local()` creates a `DoltLocalDriver`, which can generate
#' a `DoltLocalConnection`.  Unlike [dolt_remote()] and `DoltDriver`, this version
#' takes a directory name for an on-disk dolt database, starts and manages a
#'  [dolt SQL server][dolt_server()] in the background serving that directory,
#'  connects to it and returns the connection. Parameters govern both the server
#'  and connection
#'
#' Multi-user or other, more complicated networking set-ups should
#' use [dolt_server()] and [dolt_remote()] directly.
#' @usage DBI::dbConnect(doltr::dolt_local(), ...)
#' @param drv an object of class `DoltLocalDriver`, created by `dolt_local()`.
#' @param dir The dolt directory to serve and connect to
#' @inheritParams dolt_remote
#' @param server_args a list of additional arguments to pass to [dolt_server()]
#' @param ... additional arguments to pass to [`RMariaDB`][RMariaDB::MariaDB]
#' @family connections
#' @include connection-remote.R server.R
dolt_local <- function() {
  new("DoltLocalDriver")
}

#' @export
#' @noRd
setClass("DoltLocalDriver", contains = "DoltDriver")

#' @export
#' @noRd
setMethod("dbUnloadDriver", "DoltLocalDriver", function(drv, ...) { TRUE })

#' @export
#' @noRd
setMethod("show", "DoltLocalDriver", function(object) {
  cat("<DoltLocalDriver>\n")
})

#' @export
#' @noRd
setClass("DoltLocalConnection", contains = "DoltConnection",
         slots = c(dir = "character", server = "dolt_server"))

#' @export
#' @noRd
setClass("DoltLocalResult", contains = "DoltResult")

#' @export
#' @rdname dolt_local
setMethod("dbConnect", "DoltLocalDriver",
  function(drv,
           dir = Sys.getenv("DOLT_DIR", "doltdb"),
           username = Sys.getenv("DOLT_USERNAME", "root"),
           password = Sys.getenv("DOLT_PASSWORD", ""),
           port = Sys.getenv("DOLT_PORT", 3306L),
           host = Sys.getenv("DOLT_HOST", "127.0.0.1"),
           find_port = TRUE,
           server_args = list(),
           ...) {

    #browser()
    dir = normalizePath(dir, mustWork = FALSE)
    port <- if (find_port) port_fallback(port) else as.integer(port)

    # Get the cached connection
    if (!dir.exists(file.path(dir, ".dolt"))) dolt_init(dir)

    srv <- do.call(dolt_server,
                   c(list(dir = dir, username = username, password = password,
                       port = port, host = host), server_args))

    conn <- dbConnect(dolt_remote(), dbname = basename(dir), username = username,
                     password = password, host = host, port = port, ...)

    attr(conn, "dir") <- dir
    attr(conn, "server") <- srv
    class(conn) <- structure("DoltLocalConnection", package = "doltr")
    return(conn)
  })

#' @export
#' @noRd
setMethod("dbGetInfo", "DoltLocalConnection", function(dbObj, ...) {
  info <- getMethod(dbGetInfo, "DoltConnection")(dbObj)
  info$dir <- dbObj@dir
  info$server_pid <- ps::ps_pid(dbObj@server)
  info
})

#' @export
#' @noRd
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
#' @noRd
setMethod("dbDisconnect", "DoltLocalConnection", function(conn, ...) {
  getMethod(dbDisconnect, "MariaDBConnection")(conn)
  if (inherits(conn@server, "ps_handle"))
    try(kill(conn@server), silent = TRUE)
  invisible(TRUE)
})

#' @export
#' @noRd
#' @importFrom ps ps_is_running
setMethod("dbIsValid", "DoltLocalConnection", function(dbObj, ...) {
  valid <- getMethod(dbIsValid, "MariaDBConnection")(dbObj)
  if (!valid && inherits(dbObj@server, "ps_handle"))
    try(kill(dbObj@server), silent = TRUE)
  valid
})

