#' Examine information about dolt tables and diffs
#'
#' @param table [character] the name of a table in the database
#' @param to commit to compare to
#' @param from commit to compare from
#' @inheritParams dolt_branches
#' @export
#' @rdname dolt-diffs
dolt_diffs <- function(table, to, from, conn = dolt(), collect = NULL, show_sql = NULL) {
  collect <- .collect(collect); show_sql <- .show_sql(show_sql)
  query <- paste0("select * from dolt_commit_diff_", table,
                  " where to_commit='", to, "' and from_commit='", from, "'")
  dolt_query(query, conn, collect, show_sql)
}

#' @param table [character] the name of a table in the database
#' @inheritParams dolt_branches
#' @export
#' @rdname dolt-diffs
dolt_table_history <- function(table, conn = dolt(), collect = NULL, show_sql = NULL) {
  collect <- .collect(collect); show_sql <- .show_sql(show_sql)
  query <- paste0("select * from dolt_history_", table)
  dolt_query(query, conn, collect, show_sql)
}
