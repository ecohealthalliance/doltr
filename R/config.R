#' Configuration options
#'
#' The doltr package's behavior can be modified by setting these environment
#' variables:
#'
#' - `DOLT_DIR` set the default directory to look for
#'    a dolt database and run a server when using [dolt_local()] and [dolt()].
#'    Defaults to "doltdb".
#' - `DOLT_PORT` sets the port to connect to or to run the server on. Defaults
#'    to 3306.
#' - `DOLT_HOST` sets the host IP to connect to or to run the server
#'    on. Defaults to 127.0.0.1.
#' - `DOLT_CONFIG_FILE` is the path to a
#'    file with additional configuration options for the dolt sql server. See
#'    <https://docs.dolthub.com/interfaces/cli#dolt-sql-server> for options.
#' - `DOLT_PATH` specifies the path to the dolt binary if running locally.
#'    Defaults to the one found in the system path.
#' - `DOLT_COLLECT` specifies whether dolt
#'    convenience functions returning data should return fully collected
#'    tibbles or lazy tibbles for further processing.
#' - `DOLT_SHOW_SQL` will print the SQL statements executed when running dolt
#'    convenience functions. Useful for understanding how dolt SQL commands work.
#'    Set it to 1 or `true` to enable this behavior.
#' - `DOLT_WATCH` determines whether the RStudio Connection pane automatically
#'    updates in response to changes in the database.  Set it to 0 or `false`
#'    to disable this behavior.
#'
#' @rdname dolt-config
#' @name dolt-config
#' @aliases dolt_config config
NULL
