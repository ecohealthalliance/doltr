#' Navigate dolt history
#'
#' `dolt_checkout()` checks out a dolt branch, setting that branch as HEAD and
#' bringing you to its tip. `dolt_use()` sets the database to use a specific
#' commit as it's state and puts you in read-only mode.
#' @inheritParams dolt_branches
#' @param branch the branch to check out
#' @param b whether to create a new branch
#' @param start_point a commit hash from which the branch should start. If NULL,
#' starts from current HEAD.
#' @export
#' @rdname dolt-nav
#' @importFrom dbplyr sql_quote
dolt_checkout <- function(branch, b = FALSE, start_point = NULL,
                          conn = dolt(), collect = NULL, show_sql = NULL) {
  collect <- .collect(collect); show_sql <- .show_sql(show_sql)
  branch = sql_quote(branch, "'")
  if (b) branch <- paste0(sql_quote("-b", "'"), ", ", branch)
  if (!is.null(start_point)) branch <- paste0(branch, ", ", start_point)
  query <- paste0("call dolt_checkout(", branch, ")")
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
