#' Configuration variable options
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
#'    tibbles or lazy tibbles for further processing. Set it to 0 or `false`
#'    to disable, potentially for when large databases with long histories yield
#'    very large responses to commands like `dolt_log()` or `dolt_diffs()`.
#' - `DOLT_VERBOSE` will print the SQL or command-line statements executed when
#'    running functions that wrap database or system calls. Useful for
#'    understanding how dolt commands work. Set to 1 or `true` to enable this
#'    behavior.
#' - `DOLT_WATCH` determines whether the RStudio Connection pane automatically
#'    updates in response to changes in the database.  Set it to 0 or `false`
#'    to disable this behavior.
#' - `DOLT_ROOT_DIR` the directory where Dolt global configuration and credential
#'    data is stored (`~/.dolt` by default). Note this can also be set in your
#'    shell to configure command-line dolt.
#'
#' @rdname dolt-vars
#' @name dolt-vars
#' @seealso dolt-config
#' @aliases dolt_vars env_vars environment_variables
NULL
