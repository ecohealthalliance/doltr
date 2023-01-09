#' Add, commit, and reset tables in a dolt database
#' @inheritParams dolt_branches
#' @param tables tables to add. If NULL, all tables with working changes are added
#' @export
#' @rdname dolt-stage-commit
dolt_add <- function(tables = NULL, conn = dolt(), collect = NULL, show_sql = NULL) {
  collect <- .collect(collect); show_sql <- .show_sql(show_sql)
  if (is.null(tables))
    tables <- "'--all'"
  else
    tables <- paste0("'", tables, "'", collapse = ", ")
  query <- paste0("call dolt_add(", tables, ")");
  dolt_query(query, conn, collect, show_sql)
  invisible(dolt_status())
}

#' @inheritParams dolt_branches
#' @param all stage all tables before committing?
#' @param message A commit message. If NULL in an interactive session, the user
#' will be prompted. Otherwise will error if empty.
#' @param author,date Author and date. If null, uses the ones set in [dolt-config].
#'   Author should be in the format `"A U Thor author@example.com"`
#' @param allow_empty Allow recording a commit that has the exact same data as
#'   its sole parent. This is usually a mistake, so it is FALSE by default.
#' @export
#' @rdname dolt-stage-commit
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
  query <- paste0("call dolt_commit(", paste0(args, collapse = ", "), ")");
  dolt_query(query, conn, collect, show_sql)
  invisible(dolt_state())
}

#' @param hard Reset working and staged tables? If FALSE (default), a "soft"
#'   reset will be performed, only unstaging staged tables.  If TRUE, all
#'   working and staged changes will be discarded.
#' @param tables Which tables to be reset? Defaults to all tables if NULL.
#' @inheritParams dolt_branches
#' @export
#' @rdname dolt-stage-commit
dolt_reset <- function(hard = FALSE, tables = NULL, conn = dolt(),
                       collect = NULL, show_sql = NULL) {
  collect <- .collect(collect); show_sql <- .show_sql(show_sql)
  args <- c()
  if (!is.null(tables)) args <- paste0(sql_quote(tables, "'"), collapse = ", ")
  if (hard) args <- c(sql_quote("--hard", "'"), args)
  query <- paste0("call dolt_reset(", paste(args, collapse = ", ", ")"))
  dolt_query(query, conn, collect, show_sql)
  invisible(dolt_state())
}
