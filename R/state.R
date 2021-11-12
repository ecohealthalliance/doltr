#' This is used so that internal queries don't trigger watch processes
#' @noRd
#' @importMethodsFrom RMariaDB dbSendQuery dbClearResult
dolt_query_quiet <- function(conn = dolt(), query) {
  rs <- getMethod(dbSendQuery, c("MariaDBConnection", "character"))(
    conn, query)
  out <- dbFetch(rs)
  getMethod(dbClearResult, "MariaDBResult")(rs)
  out
}


#' Get information about a dolt database
#'
#' These functions yield information about the current state of a dolt database.
#' `dolt_state()` provides information on current branch or headless commit.
#' `dolt_status()` summarizes changes to the database in working or staged tables (from the `dolt_status` table).
#' `dolt_last_commit()` pulls the most recent value from the `dolt_log` table.  All
#' have pretty-print methods for the objects returned but can be interrogated for more detail.
#'
#' Values from each of these functions are returned as part of the `dbGetInfo()`
#' method and are part of the information shown in the `DoltConnection` print
#' method and in the RStudio Connection pane for a Dolt Database.
#'
#' @return A data frame of class "dolt_status" and [tibble::tbl_df].  It
#' pretty-prints as an abbreviated summary of status.
#' @importMethodsFrom RMariaDB dbSendQuery dbClearResult
#' @param conn the database connection
#' @export
#' @rdname dolt-state
dolt_state <- function(conn = dolt()) {
  dbname <- conn@db
  using <- dolt_query_quiet(conn, "select database()")[[1]]
  vars <- dolt_query_quiet(conn, paste0("select ",
                 "`@@", using, "_head_ref` AS head_ref, ",
                 "`@@", using, "_head` as head, ",
                 "`@@", using, "_staged` as staged, ",
                 "`@@", using, "_working` as working"))
  structure(c(dbname = dbname, using = using, as.list(vars)), class = "dolt_state")

}

#' @export
#' @noRd
format.dolt_state <- function(x, ...) {
  using_hash <- regextract(x$using, "(?<=/)(\\w+)$")
  if (x$dbname == x$using  && is.na(using_hash)) {
    branch <- regextract(x$head_ref, "(\\w+)$")
    out <- paste0("On branch ", branch)
  } else if (!is.na(using_hash) && using_hash == x$head && x$head_ref == "") {
    out <- paste0("Using fixed read-only database at ", using_hash, ".")
  } else if (!is.na(using_hash) && x$head_ref != "") {
    branch <- regextract(x$head_ref, "(\\w+)$")
    out <- paste0("Using fixed read-only database at ", using_hash, ", but branch head '", branch, "' checked out")
  } else {
    out <- paste0("Indeterminate state! Examine dolt_state() object. \n", paste0(names(x), ": ", unlist(x), collapse = "\n"))
  }
  out
}

#' @export
#' @noRd
print.dolt_state <- function(x, ...) {
  cat(format(x))
}


#' @export
#' @rdname dolt-state
dolt_status <- function(conn = dolt()) {
  query <- paste0("select * from dolt_status")
  status <- dolt_query(query, conn, collect = TRUE, show_sql = FALSE)
  class(status) <- c("dolt_status", class(status))
  status
}

#' @export
#' @importFrom dplyr mutate group_by summarize recode arrange pull %>%
#' @importFrom rlang .data
#' @noRd
format.dolt_status <- function(x, ...) {
  if (!nrow(x))
    out <- "Working database clean"
  else {
    out <- x %>%
      mutate(status = recode(.data$status, `new table`="new", `new doc`="new"),
             staged =  c("Working", "Staged")[.data$staged + 1]) %>%
      group_by(.data$staged, .data$status) %>%
      summarize(changed = paste0(paste(.data$table_name, collapse = ", "), " (", .data$status[1], ")")) %>%
      group_by(.data$staged) %>%
      summarize(changed = paste0(.data$staged[1], ": ", paste(.data$changed, collapse = ", "))) %>%
      arrange(.data$staged) %>%
      pull(.data$changed, name = .data$staged) %>%
      paste(collapse = "\n")
  }
  out
}

#' @export
#' @importFrom cli col_green col_yellow col_red cat_line
#' @noRd
print.dolt_status <- function(x, ...) {
  out <- format(x)
  if (out == "Working database clean")  {
    cat_line(col_green(out))
  } else {
    for(line in strsplit(out, "\n")[[1]]) {
      if (grepl("Working:", line))
        cat_line(col_red(line))
      else
        cat_line(col_yellow(line))
    }
  }
}

#' @export
#' @rdname dolt-state
dolt_last_commit <- function(conn = dolt()) {
  structure(dolt_query_quiet(conn, "SELECT * FROM dolt_log LIMIT 1"),
            class = "dolt_commit")
}

#' @export
#' @noRd
format.dolt_commit <- function(x, ...) {
  if (nchar(x$message) > 50)
    x$message <- paste0(substr(x$message, 1, 50), "...")
  ago <- paste(format(round(Sys.time() - x$date)), "ago")
  paste0("Last commit by ", x$committer, " ", ago, ": ", x$message)
}

#' @export
#' @noRd
print.dolt_commit <- function(x, ...) {
  cat(format(x))
}
