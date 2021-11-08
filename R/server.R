#' Start up a dolt SQL server and return the server process handle
#'
#' @inheritParams dolt_remote
#' @param find_port if TRUE, switch to a different port if `port` is used by
#' another process
#' @param find_server if TRUE, find a server process serving the same directory
#'   rather than starting a new one. Note that other server options will be
#'   ignored. This allows the server to be used across R sessions. Note that to
#'   make best use of this you may want to turn off the "Quit child processes on
#'   exit" option in RStudio project options.
#' @param dir The dolt directory to serve
#' @param multi_db Serve multiple databases? If `TRUE`, `dir` should be a
#'   directory with multiple subdirectories that are dolt databases
#' @param autocommit Automatically commit database changes to the working set?
#'   If `FALSE`, anything not manually committed will be lost.
#' @param read_only should the database only allow read_only connections?
#' @param log_level Defines the level of logging provided. Options are "trace",
#'  debug", "info", "warning", "error", and "fatal" (default "info").
#' @param log_out Where logging output should be directed.  If `"|"` it is passed
#'   to `std_out()`, if `NULL` (default), it is suppressed.  Can also take
#'   a filename. See [processx::run()].
#' @param timeout Defines the timeout, in seconds, used for connections
#'   A value of `0` represents an infinite timeout (default `28800000`)
#' @param query_parallelism Set the number of go routines spawned to handle each
#'   query (default `2`)
#' @param max_connections Set the number of connections handled by the server
#'   (default `100`)
#' @param config_file The path to a YAML config file to set these and additional
#'   server configuration values.  See options in the
#'   [dolt documentation](https://docs.dolthub.com/interfaces/cli#dolt-sql-server).
#' @importFrom processx process
#' @importFrom ps ps_status ps_environ ps_parent ps_handle
#' @export
#' @return A `dolt_server` object that is also a [ps::ps_handle()]
dolt_server <- function(dir = Sys.getenv("DOLT_DIR", "doltdb"),
                        username = Sys.getenv("DOLT_USERNAME", "root"),
                        password = Sys.getenv("DOLT_PASSWORD", ""),
                        port = Sys.getenv("DOLT_PORT", 3306L),
                        host = Sys.getenv("DOLT_HOST", "127.0.0.1"),
                        find_port = TRUE,
                        find_server = TRUE,
                        multi_db = FALSE,
                        autocommit = TRUE,
                        read_only = FALSE,
                        log_level = "info",
                        log_out = NULL,
                        timeout = 0,
                        query_parallelism = 2,
                        max_connections = 100,
                        config_file = Sys.getenv("DOLT_CONFIG_FILE", "")) {

  dir = normalizePath(dir)

  stopifnot(
    "No such directory" = dir.exists(dir),
    "Not a dolt directory" = dir.exists(file.path(dir, ".dolt"))
  )

  if (find_server) {
    dp <- dolt_server_find(dir = dir)
    if (nrow(dp)) {
      dp <- dp[order(!dp$is_doltr, !dp$lport == port, dp$created),]
      p <- dp$ps_handle[[1]]
      class(p) <- c("dolt_server", class(p))
      return(p)
    }
  }

  if (find_port) port <- port_fallback(port)

  args <- c("sql-server",
            paste0("--host=", host),
            paste0("--port=", port),
            paste0("--user=", username),
            paste0("--timeout=", timeout),
            paste0("--max-connections=", max_connections),
            paste0("--loglevel=", log_level),
            paste0("--query-parallelism=", query_parallelism))

  if (password != "") args <- c(args, paste0("--password=", password))
  if (read_only) args <- c(args, "--readonly")
  if (!autocommit) args <- c(args, "--no-auto-commit")
  if (config_file != "") args <- c(args, paste0("--config=", config_file))
  if (multi_db) args <- c(args, paste0("--multi-db-dir=", dir))

  if (!is.null(log_out) && !log_out %in% c("", "|"))
    log_out <- normalizePath(log_out, mustWork = FALSE)

  proc <- process$new(dolt_path(), args = args, wd = dir,
                      stdout = log_out, stderr = "2>&1",
                      env = c("current", R_DOLT=1),
                      supervise = FALSE, cleanup = FALSE, cleanup_tree = FALSE)
  p <- proc$as_ps_handle()
  rm(proc)

  while(!isTRUE(port %in% ps_connections(p)$lport)) Sys.sleep(0.25)
  stopifnot(ps_status(p) == "running")
  class(p) <- c("dolt_server", class(p))
  p
}

dolt_server_port <- function(p) {
  conns <- ps_connections(p)
  port <- conns$lport[conns$state == "CONN_LISTEN" & !is.na(conns$state)]
}

#' @export
#' @importFrom ps ps_pid ps_cwd ps_connections ps_is_running
format.dolt_server <- function(x, ...) {
  pid <- ps_pid(x)
  if (ps_is_running(x)) {
    dir <-   ps_cwd(x)
    port <- dolt_server_port(x)
    out <- paste0("<dolt sql-server> PID=", pid, ", port=", port, ", dir=", dir)
  } else {
    out <- paste0("<dolt sql-server STOPPED> PID=", pid)
  }
  out
}

#' @export
print.dolt_server <- function(x, ...) {
  cat(format(x))
  invisible(x)
}

#' Initiate a dolt database directory
#'
#' @param dir path to the directory. Will be created if it does not exist
#' @importFrom processx run
#' @export
dolt_init <- function(dir = Sys.getenv("DOLT_DIR", "doltdb")) {
  if (!dir.exists(dir)) dir.create(dir)
  old <- setwd(dir = dir)
  on.exit(setwd(old))
  run("dolt", "init")
}

#' @noRd
setOldClass("dolt_server")

#' @importFrom ps ps ps_connections ps_cwd ps_environ ps_cmdline
dolt_server_find <- function(dir = NULL, port = NULL, doltr_only = FALSE) {
  dp <- ps()
  dp <- dp[dp$name == "dolt" & dp$status == "running",]
  if (nrow(dp))
    dp <- dp[vapply(dp$ps_handle, function(x) ps_cmdline(x)[2] == "sql-server", logical(1)),]
  if (nrow(dp)) {
    dp$wd = vapply(dp$ps_handle, ps_cwd, character(1))
    dp$lport = vapply(dp$ps_handle, function(x) {
      conns <- ps_connections(x)
      conns <- conns[conns$state == "CONN_LISTEN" & !is.na(conns$state), ]
      conns$lport},
      integer(1))
    dp$is_doltr <- vapply(dp$ps_handle, function(x) isTRUE(ps_environ(x)["R_DOLT"] == "1"), logical(1))
    if (doltr_only) {
      dp <- dp[dp$is_doltr, ]
    }
    if (!is.null(port)) {
      dp <- dp[dp$lport %in% port, ]
    }
    if (!is.null(dir)) {
      dp <- dp[dp$wd %in% dir, ]
    }
  }
  dp
}

dolt_server_kill <- function(dir = NULL, port = NULL, doltr_only = FALSE, verbose  = TRUE) {
  dp <- dolt_server_find(dir, port, doltr_only)
  lapply(dp$ps_handle, dkill)
  if (verbose) message(nrow(dp), " processes killed")
  invisible(dp)
}

#' @importFrom ps signals ps_terminate ps_kill
dkill <- function(p = ps_handle) {
  if (is.null(ps::signals()$SIGTERM)) {
    ps_terminate(p)
  } else {
    ps_kill(p)
  }
  invisible(NULL)
}



