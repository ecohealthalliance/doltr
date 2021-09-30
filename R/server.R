#' Fire up a dolt server and return the process ID of the shell process
#' If already running, return the PID of the process
dolt_server <- function(dir = Sys.getenv("DOLT_DIR", "doltdb"),
                        username = Sys.getenv("DOLT_USERNAME", "root"),
                        password = Sys.getenv("DOLT_PASSWORD", ""),
                        port = Sys.getenv("DOLT_PORT", 3306L),
                        host = Sys.getenv("DOLT_HOST", "127.0.0.1"),
                        multi_db = FALSE,
                        autocommit = TRUE,
                        read_only = FALSE,
                        log_level = "info",
                        log_out = FALSE,
                        timeout = 0,
                        query_parallelism = 2,
                        max_connections = 100,
                        config_file = NULL) {

  dir = normalizePath(dir)

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
  if (!is.null(config_file)) args <- c(args, paste0("--config=", config_file))
  if (multi_db) args <- c(args, paste0("--multi-db-dir=", dir))

  old_dir <- setwd(dir = dir)
  on.exit(setwd(old_dir))

  pid <- sys::exec_background(dolt_path(), args = args,
                       std_out = log_out, std_err = log_out)

  p <- ps::ps_handle(as.integer(pid))

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

#' Find local dolt server processes
dolt_server_processes <- function() {
  processes <- ps::ps() |>
    dplyr::filter(name == "dolt", status == "running") |>
    dplyr::filter(purrr::map_lgl(ps_handle, ~isTRUE(ps::ps_cmdline(.)[2] == "sql-server")))
  processes
}

dolt_server_find <- function(dir = NULL, port = NULL) {
  processes <- dolt_server_processes() |>
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
  processes
}

dolt_server_kill_all <- function(dir = NULL, port = NULL) {
  processes <- dolt_server_find(dir, port)
  purrr::walk(processes$ps_handle, ps::ps_kill)
}

kill_softly <- function(p = ps_handle) {
  sigs <- ps::signals()
  if (!is.null(sigs$SIGTERM)) {
    out <- ps::ps_terminate(p)
    z <- 0
    while (z < 12 && ps::ps_is_running(p)) {
      Sys.sleep(0.25)
      z <- z + 1
    }
  }
  if (ps::ps_is_running(p)) out <- ps::ps_kill(p)
  out
}

#' @export
dolt_init <- function(dir = Sys.getenv("DOLT_DIR", "doltdb")) {
  if (!dir.exists(dir)) dir.create(dir)
  withr::with_dir(dir, sys::exec_wait("dolt", "init"))
}

