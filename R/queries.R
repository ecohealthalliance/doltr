# Convenience functions for working with a dolt database.
# These should expand/evolve as we get used to the workflow
# TODO: return collect()-able queries rather than full values, option to collect=TRUE
# Allow filtering variables to be passed, maybe by dots

dolt_branches <- function(conn) {
  DBI::dbGetQuery(conn, "SELECT * from dolt_branches")
}

# Awaiting resolution to https://github.com/dolthub/dolt/issues/2073
# dolt_system_tables <- function(conn) {
#   DBI::dbGetQuery(conn, "SELECT * from information_schema.tables")
# }


dolt_diffs <- function(conn, table, to) {
  query <- paste0("SELECT * from dolt_commit_diff_", table, " WHERE to_commit = ", to)
  DBI::dbGetQuery(conn, query)
}

dolt_docs <- function(conn) {
  DBI::dbGetQuery(conn, "SELECT * from dolt_docs")
}

dolt_table_history <- function(conn, table) {
  query <- paste0("SELECT * from dolt_history_", table)
  DBI::dbGetQuery(conn, query)
}

dolt_log <- function(conn) {
  query <- paste0("SELECT * from dolt_log")
  DBI::dbGetQuery(conn, query)
}

dolt_status <- function(conn) {
  query <- paste0("SELECT * from dolt_status")
  DBI::dbGetQuery(conn, query)
}

# Add tables, all by default
dolt_add <- function(conn, tables = NULL) {
  if (is.null(tables))
    tables <- "'--all'"
  else
    tables <- paste0("'", tables, "'", collapse = ", ")
  query <- paste0("SELECT DOLT_ADD(", tables, ")");
  DBI::dbGetQuery(conn, query)
}

dolt_commit <- function(conn, all = TRUE, message = NULL, author = NULL, date = NULL) {
  if(is.null(message) && !interactive()) stop("A commit message must be specified.")
  if (is.null(message)) message <- readline("Commit message: ")
  args <- c(ifelse(all, "'--all'", NULL),
            "'--message'", paste0("'", message, "'"))
  if (!is.null(author)) args <- c(args, "'--author'", paste0("'", author, "'"))
  if (!is.null(date)) args <- c(args, "'--date'", paste0("'", date, "'"))
  query <- paste0("SELECT DOLT_COMMIT(", paste(args, collapse = ", "), ")")
  hash <- DBI::dbGetQuery(conn, query)[[1]]
  class(result) <- "dolt_hash"

  result
}

# Return the name of the HEAD
dolt_head_ref <- function(conn) {
  dbname <- dbGetInfo(conn)$dbname
  query <- paste0("SELECT @@", dbname, "_head_ref")
  DBI::dbGetQuery(conn, query)[[1]]
}

dolt_head_hash <- function(conn) {
  dbname <- dbGetInfo(conn)$dbname
  query <- paste0("SELECT @@", dbname, "_head")
  hash <- DBI::dbGetQuery(conn, query)[[1]]
  class(hash) <- "dolt_hash"

  hash
}

dolt_working_hash <- function(conn) {
  dbname <- dbGetInfo(conn)$dbname
  query <- paste0("SELECT @@", dbname, "_head")
  hash <- DBI::dbGetQuery(conn, query)[[1]]
  class(hash) <- "dolt_hash"

  hash
}

dolt_set_head <- function(conn, hash) {

}

# TODO: Set a default connection that all dolt_ functions will use for the session, for convenience
#set_dolt_conn <- function(conn) {
#}

dolt_push <- function(dir) {
  NULL
  # First check that the server is shut down
}

dolt_pull <- function(dir) {
  NULL
  # First check that the server is shut down
  # Then run the command in the working directory
  #withr::with_dir(dir,
}

dolt_fetch <- function(dir) {
  NULL
}

