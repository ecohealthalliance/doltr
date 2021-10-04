#' Start up a dolt SQL server and return the server process handle
#'
#' @inheritParams dolt_remote
#' @param find_port if TRUE, switch to a different port if `port` is used by
#' another process
#' @param dir The dolt directory to serve
#' @param multi_db Serve multiple databases? If `TRUE`, `dir` should be a
#'   directory with multiple subdirectories that are dolt databases
#' @param autocommit Automatically commit database changes to the working set?
#'   If `FALSE`, anything not manually committed will be lost.
#' @param read_only should the database only allow read_only connections?
#' @param log_level Defines the level of logging provided. Options are "trace",
#'  debug", "info", "warning", "error", and "fatal" (default "info").
#' @param log_out Where logging output should be directed.  If `TRUE` it is passed
#'   to `std_out()`, if `FALSE` (default), it is suppressed.  Can also take
#'   a filename, connection, or callback function.  See [sys::exec()].
#' @param timeout Defines the timeout, in seconds, used for connections
#'   A value of `0` represents an infinite timeout (default `28800000`)
#' @param query_parallelism Set the number of go routines spawned to handle each
#'   query (default `2`)
#' @param max_connections Set the number of connections handled by the server
#'   (default `100`)
#' @param config_file The path to a YAML config file to set these and additional
#'   server configuration values.  See options in the
#'   [dolt documentation](https://docs.dolthub.com/interfaces/cli#dolt-sql-server).
#' @importFrom sys exec_background
#' @importFrom ps ps_handle
#' @export
#' @return A `dolt_server` object that is also a [ps::ps_handle()]
dolt_server <- function(dir = Sys.getenv("DOLT_DIR", "doltdb"),
                        username = Sys.getenv("DOLT_USERNAME", "root"),
                        password = Sys.getenv("DOLT_PASSWORD", ""),
                        port = Sys.getenv("DOLT_PORT", 3306L),
                        host = Sys.getenv("DOLT_HOST", "127.0.0.1"),
                        find_port = TRUE,
                        multi_db = FALSE,
                        autocommit = TRUE,
                        read_only = FALSE,
                        log_level = "info",
                        log_out = FALSE,
                        timeout = 0,
                        query_parallelism = 2,
                        max_connections = 100,
                        config_file = Sys.getenv("DOLT_CONFIG_FILE", "")) {

  dir = normalizePath(dir)
  if (find_port) port <- port_fallback(port)

  stopifnot(
    "No such directory" = dir.exists(dir),
    "Not a dolt directory" = dir.exists(file.path(dir, ".dolt"))
  )

  args <- c("sql-server",
            paste0("--host=", host),
            paste0("--port=", port),
            paste0("--user=", username),
            paste0("--timeout=", timeout),
            paste0("--max-connections=", max_connections),
            paste0("--loglevel=", log_level),
            paste0("--query-parallelism=", query_parallelism))

  if (password != "") args <- c(paste0("--password=", password))
  if (read_only) args <- c(args, "--readonly")
  if (!autocommit) args <- c(args, "--no-auto-commit")
  if (config_file != "") args <- c(args, paste0("--config=", config_file))
  if (multi_db) args <- c(args, paste0("--multi-db-dir=", dir))

  old_dir <- setwd(dir = dir)
  on.exit(setwd(old_dir))

  pid <- exec_background(dolt_path(), args = args,
                              std_out = log_out, std_err = log_out)

  p <- ps_handle(as.integer(pid))

  Sys.sleep(0.5)
  stopifnot(ps::ps_status(p) == "running")
  class(p) <- c("dolt_server", class(p))
  p
}

# TODO figure out how to format this
#' @export
format.dolt_server <- function(p) {
  pid <- ps::ps_pid(p)
  dir <-   ps::ps_cwd(p)
  port <- ps::ps_connections(p)$lport
  out <- paste0("dolt server", "\n",
                "dir: ", dir, "\n",
                "port: ", port, "\n",
                "pid: ", pid)
  out
}

#' @export
print.dolt_server <- function(p) {
  cat(format(p))
  invisible(p)
}

#' Initiate a dolt database directory
#'
#' @param dir path to the directory. Will be created if it does not exist
#' @export
dolt_init <- function(dir = Sys.getenv("DOLT_DIR", "doltdb")) {
  if (!dir.exists(dir)) dir.create(dir)
  withr::with_dir(dir, sys::exec_wait("dolt", "init"))
}

#' @noRd
setOldClass("dolt_server")

#' Find local dolt server processes
dolt_server_processes <- function() {
  processes <- ps::ps() |>
    dplyr::filter(name == "dolt", status == "running") |>
    dplyr::filter(purrr::map_lgl(ps_handle, ~isTRUE(ps::ps_cmdline(.)[2] == "sql-server")))
  processes
}

dolt_server_find <- function(dir = NULL, port = NULL) {
  processes <- dolt_server_processes()
  if (nrow(processes)) {
    processes <- processes |>
      dplyr::mutate(cwd = purrr::map_chr(ps_handle, ps::ps_cwd)) |>
      dplyr::mutate(conns = purrr::map(ps_handle, ps::ps_connections)) |>
      tidyr::unnest(conns)
    if (!is.null(processes$state)) processes <- processes[processes$state == "CONN_LISTEN",]
    if (!is.null(dir)) {
      dir = normalizePath(dir, mustWork = FALSE)
      processes <- dplyr::filter(processes, cwd %in% dir)
    }
    if (!is.null(port)) {
      port = as.integer(port)
      processes <- dplyr::filter(processes, lport %in% port)
    }
  }
  processes
}

dolt_server_kill_all <- function(dir = NULL, port = NULL) {
  processes <- dolt_server_find(dir, port)
  purrr::walk(processes$ps_handle, ps::ps_kill)
}

kill <- function(p = ps_handle) {
  if (is.null(ps::signals()$SIGTERM)) {
    ps::ps_kill(p)
  } else {
    ps::ps_terminate(p)
  }
  NULL
}

# kill_softly <- function(p = ps_handle) {
#   sigs <- ps::signals()
#   if (!is.null(sigs$SIGTERM)) {
#     out <- ps::ps_terminate(p)
#     z <- 0
#     while (z < 12 && ps::ps_is_running(p)) {
#       Sys.sleep(0.25)
#       z <- z + 1
#     }
#   }
#   if (ps::ps_is_running(p)) out <- ps::ps_kill(p)
#   out
# }


