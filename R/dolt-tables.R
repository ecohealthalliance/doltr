#' Dolt System Tables
#'
#' These functions query the dolt database for system tables that describe the
#' database version history and structure.
#'
#' @param conn the database connection
#' @param collect whether to collect the result into R or return a [dbplyr::tbl_lazy()]
#'   to be further processed before collecting.  Defaults to `TRUE`, can be set with the [environment variable][dolt_vars]
#'   `DOLT_COLLECT`.
#' @param show_sql Whether to print the SQL statements used internally to fetch
#'   the data. Useful for learning how Dolt works internally. Defaults to `FALSE`, can
#'   be set with the environment variable `DOLT_VERBOSE`.
#' @export
#' @rdname dolt-tables
dolt_branches <- function(conn = dolt(), collect = NULL, show_sql = NULL) {
  collect <- .collect(collect); show_sql <- .show_sql(show_sql)
  dolt_query("select * from dolt_branches", conn, collect, show_sql)
}

#' @export
#' @rdname dolt-tables
dolt_remotes <- function(conn = dolt(), collect = NULL, show_sql = NULL) {
  collect <- .collect(collect); show_sql <- .show_sql(show_sql)
  query <- "select name, url, cast(fetch_specs as char) as fetch_specs, cast(params as char) as params from dolt_remotes"
  dolt_query(query, conn, collect, show_sql)
}

#' @export
#' @rdname dolt-tables
dolt_docs <- function(conn = dolt(), collect = NULL, show_sql = NULL) {
  collect <- .collect(collect); show_sql <- .show_sql(show_sql)
  query <- "select * from dolt_docs"
  dolt_query(query, conn, collect, show_sql)
}

#' @export
#' @rdname dolt-tables
dolt_log <- function(conn = dolt(), collect = NULL, show_sql = NULL) {
  collect <- .collect(collect); show_sql <- .show_sql(show_sql)
  query <- paste0("select * from dolt_log")
  dolt_query(query, conn, collect, show_sql)
}

# Awaiting resolution to https://github.com/dolthub/dolt/issues/2073
# dolt_system_tables <- function(conn) {
#   DBI::dbGetQuery(conn, "select * from information_schema.tables")
# }

