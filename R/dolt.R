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
    envname <- paste0(dbname, "|", username, "@", host, ":", port)
  } else {
    envname <- normalizePath(dir)
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

  if (cache_connection) assign(envname, conn, envir = dolt_cache)

  conn
}



  dolt_cache <- new.env()
  reg.finalizer(
    dolt_cache,
    function(env) eapply(env, dbDisconnect, all.names = TRUE),
    onexit = TRUE
    )

# # If the connection doesn't have a server, try to find one
# if (!server_running && find_server) {
#   running_servers <- dolt_server_find(dir = dir)
#   if (nrow(running_servers)) {
#     srv <- running_servers$ps_handle[[1]]
#     port <- running_servers$lport[[1]]
#     class(srv) <- c("dolt_server", class(srv))
#     server_running <- TRUE
#   }
# }
