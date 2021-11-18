#' Work with dolt repository remotes
#'
#' @param remote the name of the remote. "origin" is used by default
#' @param remote_branch the name of the branch when setting upstream branch. The current local branch is used by default
#' @param ref the branch reference
#' @param set_upstream whether to set the remote branch reference to track
#' @param force whether to overwrite any conflicting history
#'   the current branch
#' @inheritParams dolt_branches
#' @export
#' @rdname dolt-remote
#' @family dolt-sql-commands
#' @importFrom dbplyr sql_quote
dolt_push <- function(remote = NULL, ref = NULL, set_upstream = FALSE,
                      force = FALSE, conn = dolt(), collect = NULL,
                      show_sql = NULL) {
  collect <- .collect(collect); show_sql <- .show_sql(show_sql)
  args <- character(0)
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


#' @rdname dolt-remote
#' @importFrom processx run
#' @param remote_url the remote URL to clone
#' @param new_dir the directory to clone into
#' @param branch the branch to clone. If NULL, clones all branches
#' @export
dolt_clone <- function(remote_url, remote = "origin",
                       new_dir = basename(remote_url), branch = NULL) {

  args <- c(paste0("--remote=", remote), remote_url, new_dir)
  if (!is.null(branch)) args <- c(args, paste0("--branch=", branch))

  run(dolt_path(), c("clone", args), stdout = "", stderr = "")
  invisible(normalizePath(new_dir))
}
