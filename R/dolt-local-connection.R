#' @export
#' @title Connect to a local dolt database directory
#' @description `dolt_local()` creates a `DoltLocalDriver`, which can generate
#' a `DoltLocalConnection`.  Unlike [dolt_remote()] and `DoltDriver`, _local_
#' connections are for dolt databases stored in directories on-disk, and take
#' a directory name as an argument. The local connection type starts and manages
#' a [dolt SQL server][dolt_server()] in the background serving that directory,
#' connects to it and returns the connection. Parameters govern both the server
#' and connection
#'
#' Local dolt connection objects contain additional slots including the
#' database path on-disk and an external pointer to the server process, and
#' these are returned via `dbGetInfo` and displayed in the connection print
#' method. The `dbDisconnect` method kills the background server if no other
#' processes are connected to it.
#'
#' Multi-user or other, more complicated networking set-ups should
#' use [dolt_server()] and [dolt_remote()] directly.
#' @param drv an object of class `DoltLocalDriver`, created by `dolt_local()`.
#' @param dir The dolt directory to serve and connect to
#' @inheritParams dolt_remote
#' @param find_port whether to find an open port if the default is used by another
#'   process
#' @param find_server whether to look for another server process serving the same
#'   directory before creating a new one
#' @param server_args a list of additional arguments to pass to [dolt_server()]
#' @param ... additional arguments to pass to [`RMariaDB`][RMariaDB::MariaDB]
#' @family connections
#' @include dolt-connection.R server.R
dolt_local <- function() {
  new("DoltLocalDriver")
}

#' @export
#' @rdname dolt_local
setClass("DoltLocalDriver", contains = "DoltDriver")

#' @export
#' @rdname dolt_local
setMethod("dbUnloadDriver", "DoltLocalDriver", function(drv, ...) { TRUE })

#' @export
#' @rdname dolt_local
setMethod("show", "DoltLocalDriver", function(object) {
  cat("<DoltLocalDriver>\n")
})

#' @export
#' @rdname dolt_local
setClass("DoltLocalConnection", contains = "DoltConnection",
         slots = c(dir = "character", server = "dolt_server"))

#' @export
#' @rdname dolt_local
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
                   find_server = TRUE,
                   autocommit = TRUE,
                   server_args = list(),
                   ...) {

            #browser()
            dir = normalizePath(dir, mustWork = FALSE)

            if (!dir.exists(file.path(dir, ".dolt"))) dolt_init(dir)

            srv <- do.call(dolt_server,
                           c(list(dir = dir, username = username, password = password,
                                  port = port, host = host, find_port = find_port,
                                  find_server = find_server), server_args))

            port <- dolt_server_port(srv)

            conn <- dbConnect(dolt_remote(), dbname = basename(dir), username = username,
                              password = password, host = host, port = port,
                              autocommit = autocommit, ...)

            attr(conn, "dir") <- dir
            attr(conn, "server") <- srv
            class(conn) <- structure("DoltLocalConnection", package = "doltr")
            return(conn)
          })

#' @export
#' @rdname dolt_local
#' @param dbObj the database connection
setMethod("dbGetInfo", "DoltLocalConnection", function(dbObj, ...) {
  info <- getMethod(dbGetInfo, "DoltConnection")(dbObj, ...)
  info$dir <- dbObj@dir
  info$server_pid <- ps::ps_pid(dbObj@server)
  info
})

#' @export
#' @rdname dolt_local
#' @importFrom cli cli_h1 cli_ul cli_li cli_end cli_alert_warning
setMethod("show", "DoltLocalConnection", function(object) {
  if (dbIsValid(object)) {
  info <- dbGetInfo(object)
  cli_h1("<DoltLocalConnection> {info$dbname}")
    l <- cli_ul()
    cli_li("Serving {info$dir}, PID {info$server_pid}")
    cli_li("Connected at: {info$username}@{info$host}:{info$port}")
    cli_li(format(info$state))
    cli_li(format(info$last_commit))
    cli_li(format(info$status))
    cli_end(l)
  } else {
    cli_alert_warning("Invalid Connection")
  }
})

#' @export
#' @rdname dolt_local
#' @importFrom ps ps_environ ps_handle ps_connections ps_is_running
#' @importFrom DBI dbGetQuery
#' @param conn the database connection
setMethod("dbDisconnect", "DoltLocalConnection", function(conn, ...) {

  if (dbIsValid(conn) && ps_is_running(conn@server)) {
    # On disconnection, kill the server only if it was started by doltr and no
    # no other processes connect to it.

    is_doltr_server <- isTRUE(ps_environ(conn@server)["R_DOLT"] == "1")
    procs <- ps()
    procs <- procs[(procs$status == "running" | procs$status == "sleeping") & procs$pid != ps_pid(ps_handle()),]
    procs <- procs[vapply(procs$ps_handle, function(x) {
      conns <- try(ps_connections(x), silent = TRUE)
      out <- !inherits(conns, "try-error") && nrow(conns) && conn@port %in% conns$lport
      out
    }, logical(1)),]
    other_sessions <- as.logical(nrow(procs))
    other_conns_in_session <- sum(ps_connections(ps_handle())$rport == conn@port, na.rm = TRUE) > 1
    other_sql_procs <- nrow(dbGetQuery(conn, "show processlist;")) > 1
    kill_server <- is_doltr_server &&
      !other_sessions && !other_conns_in_session && !other_sql_procs
  } else {
    kill_server <- FALSE
  }
  getMethod(dbDisconnect, "DoltConnection")(conn)

  if (kill_server) {
    try(dkill(conn@server), silent = T)
  }
})

#' @export
#' @importFrom ps ps_is_running
#' @rdname dolt_local
setMethod("dbIsValid", "DoltLocalConnection", function(dbObj, ...) {
  valid <- getMethod(dbIsValid, "MariaDBConnection")(dbObj) &&
    class(try(dbGetQuery(dbObj, "SELECT 1"), silent = TRUE)) != "try-error" &&
    ps_is_running(dbObj@server)
  if (!valid && inherits(dbObj@server, "ps_handle"))
    try(dkill(dbObj@server), silent = TRUE)
  valid
})

