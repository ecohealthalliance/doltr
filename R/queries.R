# Convenience functions for working with a dolt database.
# These should expand/evolve as we get used to the workflow
# Allow filtering variables to be passed, maybe by dots

.collect <- function(collect) {
  if (is.null(collect))
    return(Sys.getboolenv("DOLT_COLLECT", TRUE))
  else
    return(collect)
}

.show_sql <- function(show_sql) {
  if (is.null(show_sql))
    return(Sys.getboolenv("DOLT_SHOW_SQL", FALSE))
  else
    return(show_sql)
}


#' Get info about the dolt database
#' @export
#' @name dolt-info
#' @rdname dolt-info
dolt_branches <- function(conn = dolt(), collect = NULL, show_sql = NULL) {
  collect <- .collect(collect); show_sql <- .show_sql(show_sql)
  query <- query
  dolt_query("select * from dolt_branches", conn, collect, show_sql)
}

#' @export
#' @rdname dolt-info
dolt_remotes <- function(conn = dolt(), collect = NULL, show_sql = NULL) {
  collect <- .collect(collect); show_sql <- .show_sql(show_sql)
  query <- "select name, url, cast(fetch_specs as char) as fetch_specs, cast(params as char) as parms from dolt_remotes"
  dolt_query(query, conn, collect, show_sql)
}

#' @export
#' @rdname dolt-info
dolt_docs <- function(conn = dolt(), collect = NULL, show_sql = NULL) {
  collect <- .collect(collect); show_sql <- .show_sql(show_sql)
  query <- "select * from dolt_docs"
  dolt_query(query, conn, collect, show_sql)
}

#' @export
#' @rdname dolt-info
dolt_log <- function(conn = dolt(), collect = NULL, show_sql = NULL) {
  collect <- .collect(collect); show_sql <- .show_sql(show_sql)
  query <- paste0("select * from dolt_log")
  dolt_query(query, conn, collect, show_sql)
}

# Awaiting resolution to https://github.com/dolthub/dolt/issues/2073
# dolt_system_tables <- function(conn) {
#   DBI::dbGetQuery(conn, "select * from information_schema.tables")
# }


#' Examine information about dolt tables and diffs
#' @param table [character] the name of a table in the database
#' @param to commit to compare to
#' @inheritParams dolt-info
#' @export
#' @rdname dolt-diffs
#' @family dolt-sql-commands
dolt_diffs <- function(table, to, conn = dolt(), collect = NULL, show_sql = NULL) {
  collect <- .collect(collect); show_sql <- .show_sql(show_sql)
  query <- paste0("select * from dolt_commit_diff_", table, " WHERE to_commit = ", to)
  dolt_query(query, conn, collect, show_sql)
}

#' @param table [character] the name of a table in the database
#' @inheritParams dolt-info
#' @export
#' @rdname dolt-diffs
dolt_table_history <- function(table, conn = dolt(), collect = NULL, show_sql = NULL) {
  collect <- .collect(collect); show_sql <- .show_sql(show_sql)
  query <- paste0("select * from dolt_history_", table)
  dolt_query(query, conn, collect, show_sql)
}


#' Add, commit, and reset tables in a dolt database
#' @inheritParams dolt-info
#' @param tables tables to add. If NULL, all tables with working changes are added
#' @export
#' @rdname dolt-ver
#' @family dolt-sql-commands
dolt_add <- function(tables = NULL, conn = dolt(), collect = NULL, show_sql = NULL) {
  collect <- .collect(collect); show_sql <- .show_sql(show_sql)
  if (is.null(tables))
    tables <- "'--all'"
  else
    tables <- paste0("'", tables, "'", collapse = ", ")
  query <- paste0("select dolt_add(", tables, ")");
  dolt_query(query, conn, collect, show_sql)
  invisible(dolt_status())
}

#' @inheritParams dolt-info
#' @param all stage all tables before comitting?
#' @param message A commit message. If NULL in an interactive session, the user
#' will be prompted. Otherwise will error if empty.
#' @export
#' @rdname dolt-ver
dolt_commit <- function(all = TRUE, message = NULL, author = NULL, date = NULL,
                        allow_empty = FALSE, conn = dolt(), collect = NULL,
                        show_sql = NULL) {
  collect <- .collect(collect); show_sql <- .show_sql(show_sql)


  if (is.null(message) && Sys.getenv("RSTUDIO") == "1") {
    message <- rstudioapi::showPrompt("Commit Message", "Commit Message:")
  } else if (is.null(message)) {
    message <- readline("Commit message: ")
  }

  if(is.null(message) && !interactive()) {
    stop("A commit message is required")
  }
  args <- c(ifelse(all, "'--all'", NULL),
            "'--message'", paste0("'", message, "'"))
  if (!is.null(author)) args <- c(args, "'--author'", paste0("'", author, "'"))
  if (!is.null(date)) args <- c(args, "'--date'", paste0("'", date, "'"))
  if (allow_empty) args <- c(args, "'--allow-empty'")
  query <- paste0("select dolt_commit(", paste0(args, collapse = ", "), ")");
  dolt_query(query, conn, collect, show_sql)
  invisible(dolt_state())
}

#' @param hard Reset working and staged tables? If FALSE (default), a "soft"
#'   reset, of only staged tables, will be performed
#' @param tables Which tables to be reset? Defaults to all tables if NULL.
#' @inheritParams dolt_status
#' @export
#' @rdname dolt-ver
dolt_reset <- function(hard = FALSE, tables = NULL, conn = dolt(),
                       collect = NULL, show_sql = NULL) {
  collect <- .collect(collect); show_sql <- .show_sql(show_sql)
  args <- c()
  if (!is.null(tables)) args <- paste0(sql_quote(tables, "'"), collapse = ", ")
  if (hard) args <- c(sql_quote("--hard", "'"), args)
  query <- paste0("select dolt_reset(", paste(args, collapse = ", ", ")"))
  dolt_query(query, conn, collect, show_sql)
  invisible(dolt_state())
}

#' Navigate dolt history
#'
#' `dolt_checkout()` checks out a dolt branch setting that branch as HEAD and
#' bringing you to it's tip. `dolt_use()` sets the database to use a specific
#' commit as it's state and puts you in read-only mode.
#' @inheritParams dolt-info
#' @param branch the branch to check out
#' @param b whether to create a new branch
#' @export
#' @rdname dolt-nav
#' @importFrom dbplyr sql_quote
dolt_checkout <- function(ref, b = FALSE, start_point = NULL,
                          conn = dolt(), collect = NULL, show_sql = NULL) {
  collect <- .collect(collect); show_sql <- .show_sql(show_sql)
  ref = sql_quote(ref, "'")
  if (b) ref <- paste0(sql_quote("-b", "'"), ", ", ref)
  if (!is.null(start_point)) ref <- paste0(ref, ", ", start_point)
  query <- paste0("select dolt_checkout(", ref, ")")
  dolt_query(query, conn, collect, show_sql)
  invisible(dolt_state())
}

#' @export
#' @param hash the commit hash you want to set the database to. If NULL, checks
#' out the head of the main branch and brings you out of read-only mode.
#' @rdname dolt-nav
dolt_use <- function(hash = NULL, conn = dolt()) {
  db <- if (is.null(hash)) conn@db else paste0(conn@db, "/", hash)
  dbExecute(conn, paste0("use `", db, "`"))
  invisible(dolt_state())
}

#' Work with dolt repository remotes
#'
#' @param remote the name of the remote. "origin" is used by default
#' @param ref the branch reference
#' @param set_upstream whether to set the remote branch reference to track
#' @param force whether to overwrite any conflicting history
#'   the current branch
#' @inheritParams dolt_status
#' @export
#' @rdname dolt-remote
#' @family dolt-sql-commands
#' @importFrom dbplyr sql_quote
dolt_push <- function(remote = NULL, ref = NULL, set_upstream = FALSE,
                      force = FALSE, conn = dolt(), collect = NULL,
                      show_sql = NULL) {
  collect <- .collect(collect); show_sql <- .show_sql(show_sql)
  args <- ""
  if (!is.null (remote)) args <- c(args, sql_quote(remote, "'"))
  if (!is.null (ref)) args <- c(args, sql_quote(ref, "'"))
  if (set_upstream) args <- c("'--set-upstream'", args)
  if (force) args <- c(args, "'--force'")
  query <- paste0("select dolt_push(", paste0(args, collapse = ", "), ")")
  dolt_query(query, conn, collect, show_sql)
  invisible(dolt_state())
}

#' @param squash whether to merge changes to the working set without
#'   updating the commit history
#' @inheritParams dolt_status
#' @export
#' @rdname dolt-remote
dolt_pull <- function(remote = NULL, squash = FALSE, conn = dolt(),
                      collect = NULL, show_sql = NULL) {
  collect <- .collect(collect); show_sql <- .show_sql(show_sql)
  args <- ""
  if (!is.null(remote)) args <- c(args, sql_quote(remote, "'"))
  if (squash) args <- c(args, "'--squash'")
  query <- paste0("select dolt_pull(", paste0(args, collapse = ", "), ")")
  dolt_query(query, conn, collect, show_sql)
  invisible(dolt_state())
}

#' @inheritParams dolt_status
#' @export
#' @rdname dolt-remote
dolt_fetch <- function(remote = NULL, ref = FALSE, force = FALSE,
                       conn = dolt(), collect = NULL, show_sql = NULL) {
  collect <- .collect(collect); show_sql <- .show_sql(show_sql)
  args <- ""
  if (!is.null(remote)) args <- c(args, sql_quote(remote, "'"))
  if (!is.null(ref)) args <- c(args, sql_quote(remote, "'"))
  if (force) args <- c(args, "'--force'")
  query <- paste0("select dolt_fetch(", paste0(args, collapse = ", "), ")")
  dolt_query(query, conn, collect, show_sql)
  invisible(dolt_state())
}


