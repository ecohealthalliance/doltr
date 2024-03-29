#' Return a (cached) connection to the default Dolt database
#'
#' `dolt()` returns a connection to a default database. It is a convenience
#' wrapper around `dbConnect(dolt_local/remote(), ...` that also caches connections
#' for faster loading.
#'
#' @param dir The directory from which to server a [dolt_local()] connection.
#'   If `"remote"` a [dolt_remote()] connection will be made and no server will
#'   be started.
#' @inheritParams dolt_local
#' @param dbname for remote connections, the database name
#' @param cache_connection Should we preserve a cache of the connection? allows
#' faster load times and prevents connection from being garbage-collected.
#' @param ... further arguments passed to [dolt_server()] or [MariaDB()]
#' @family connections
#' @export
dolt <- function(dir = Sys.getenv("DOLT_DIR", "doltdb"),
                 dbname = NULL,
                 username = Sys.getenv("DOLT_USERNAME", "root"),
                 password = Sys.getenv("DOLT_PASSWORD", ""),
                 port = Sys.getenv("DOLT_PORT", 3306L),
                 host = Sys.getenv("DOLT_HOST", "127.0.0.1"),
                 cache_connection = TRUE,
                 ...) {

  if (dir == "remote") {
    if (is.null(dbname)) stop("A `dbname` is required for remote connections")
    envname <- paste0(dbname, "|", username, "@", host, ":", port)
  } else {
    envname <- normalizePath(dir, mustWork = FALSE)
  }

  conn = mget(envname, dolt_cache, ifnotfound = NA)[[1]]
  # If the cached connection exists check that it works and otherwise disconnect
  if (inherits(conn, "DoltConnection")) {
    if (dbIsValid(conn)) {
      return(conn)
    } else {
      suppressWarnings(dbDisconnect(conn))
    }
  }

  if (dir == "remote") {
    conn <- dbConnect(dolt_remote(), dbname = dbname, username = username,
                      password = password, host = host, port = port, ...)
  } else {
    conn <- dbConnect(dolt_local(), dir = dir, username = username,
                      password = password, host = host, port = port, ...)
  }

  if (cache_connection) {
    assign(envname, conn, envir = dolt_cache)
  }

  conn
}

dolt_cache <- new.env()
dolt_states <- new.env()
dolt_status_cache <- new.env()
reg.finalizer(
  dolt_cache,
  function(env) eapply(
    env,
    function(x) if (inherits(x, "DBIConnection")) dbDisconnect(x),
    all.names = TRUE),
  onexit = TRUE
)

dolt_watch <- function(conn = dolt()) {
  conn_name <- dolt_conn_name(conn)
  old_state <- mget(conn_name, dolt_states, ifnotfound = NA)[[1]]
  new_state <- dolt_state(conn)
  if(!identical(old_state, new_state)) {
    update_dolt_pane(conn)
    assign(conn_name, new_state, envir = dolt_states)
  }
  NULL
}

dolt_conn_name <- function(conn = dolt()) {
  if (inherits(conn, "DoltLocalConnection")) {
    return(normalizePath(conn@dir, mustWork = FALSE))
  } else {
    return(paste0(conn@db, "|", conn@username, "@", conn@host, ":", conn@port))
  }
}
