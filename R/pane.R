# TODO: Break this into smaller functions
# TODO: Allow this and dolt() to be called with a single-string connection
# of form `dbname|user@host`. Automatically get creds?
#' Open a Dolt connection pane in RStudio
#'
#' This function launches the RStudio "Connection" pane to interactively
#' explore the database.  The pane will show the database versioning state,
#' tables stored in the database, and dolt system tables showing history.
#'
#' When running dolt interactively, the connection pane will automatically
#' update in response to most queries that modify the database state.  You
#' can stop this behavior by setting the `DOLT_WATCH` environment variable
#' to `0` or `false`.  See [dolt_vars] for more configuration variables
#'
#' @export
#' @return The connection object (invisibly)
#' @param conn a dolt connection. If a path is provided instead, a connection
#' will be created to the path using [dolt()].
dolt_pane <- function(conn = dolt()) {
  if (!inherits(conn, "DBIConnection") && is.character(conn) && length(conn) == 1)
    conn <- doltr::dolt(conn)
  observer <- getOption("connectionObserver")
  if (!is.null(observer) && interactive() && dbIsValid(conn)) {
    conn_name <- dolt_conn_name(conn)
    observer$connectionOpened(
      connectionObject = conn,
      type = "Dolt",
      host = conn_name,
      displayName = conn_name,
      icon = ifile("dolt-db.png"),
      connectCode = dolt_connect_code(conn),
      disconnect = function() {
        DBI::dbDisconnect(conn)
        observer$connectionClosed("Dolt", conn_name)
      },
      listObjectTypes = dolt_db_obj_types,
      listObjects = function(schema = NULL, versioning = NULL) {
        if (is.null(schema)) {
          stateline <- format(dolt_state(conn))
          last_commit <- format(dolt_last_commit(conn))
          statusline <- strsplit(format(dolt_status(conn)), "\n")[[1]]
          tbls <- dbListTables(conn)
          out <- data.frame(
            name = c(stateline, last_commit, statusline, tbls, "dolt_system_tables", "information_schema"),
            type = c("info", "info", rep("info", length(statusline)), rep("table", length(tbls)), "schema", "schema"),
            stringsAsFactors = FALSE
          )
        } else if (schema == "information_schema") {
          out <- dbGetQuery(conn, "select table_name as name, 'table' as type from information_schema.tables where table_schema = 'information_schema'")
        } else if (schema == "dolt_system_tables" && is.null(versioning)) {
          names <- paste0("dolt_", c("branches", "docs", "log", "status",
                                     "procedures", "remotes", "query_catalog"))
          present <- vapply(names, function(n) {
            as.logical(tryCatch(
              DBI::dbGetQuery(conn, paste0("select count(*) from ", n, " limit 1"))[[1]],
              error = function(e) 0))
          }, logical(1))

          tabs <- paste(dbListTables(conn), "versioning")
          out <- data.frame(
            name = c(names[present], tabs),
            type = c(rep("table", length(names[present])), rep("versioning", length(tabs))),
            stringsAsFactors = FALSE
          )
        } else if (schema == "dolt_system_tables" && !is.null(versioning)) {
          tab <- strsplit(versioning, " ")[[1]][1]
          names <- paste0(c("dolt_commit_diff_", "dolt_diff_", "dolt_history_", "dolt_conflicts_"), tab)
          present <- present <- vapply(names, function(n) {
            as.logical(tryCatch(
              DBI::dbGetQuery(conn, paste0("select count(*) from ", n, " limit 1"))[[1]],
              error = function(e) 0))
          }, logical(1))
          out <- data.frame(
            name = names[present],
            type = "table",
            stringsAsFactors = FALSE
          )
        }
        return(out)
      },
      listColumns = function(schema = "", table, versioning = NULL) {
        s <- if (schema == "dolt_system_tables") "" else if (schema == "") "" else paste0(schema, ".")
        #res <- DBI::dbGetQuery(conn, paste0("select * from ", s, table, " limit 1"))
        structure(DBI::dbGetQuery(conn, paste0("describe ", s, table))[,1:2],
                  .Names = c("name", "type"))

      },
      previewObject =  function(limit = 1000, table, schema = "", versioning = NULL) {
        dolt_preview_table(limit = limit, table = table, schema = schema, versioning = versioning, conn = conn)
      },
      actions = list(
        SQL = list(
          icon = ifile("rstudio/edit-sql.png"),
          callback = sql_action
        ),
        Stage = list(
          icon = ifile("rstudio/staged.png"),
          callback = function() {
            dolt_add(conn = conn)
            update_dolt_pane(conn)
          }
        ),
        Unstage = list(
          icon = ifile("rstudio/unstaged.png"),
          callback = function() {
            dolt_reset(conn = conn)
            update_dolt_pane(conn)
          }
        ),
        Commit = list(
          icon = ifile("rstudio/commit.png"),
          callback = function() {
            dolt_commit(conn = conn)
            update_dolt_pane(conn)
          }
        ),
        Pull = list(
          icon = ifile("rstudio/pull.png"),
          callback = function() doltr::dolt_pull(conn = conn)
        ),
        Push = list(
          icon = ifile("rstudio/push.png"),
          callback = function() doltr::dolt_push(conn = conn)
        ),
        Log = list(
          icon = ifile("rstudio/history.png"),
          callback = function() view2(x = dolt_log(conn = conn), title = "Dolt Log")
        )
      )
    )
  }
  invisible(conn)

}

ifile <- function(f) {
  system.file("img", f, package = "doltr")
}

#' @export
#' @rdname dolt_pane
update_dolt_pane <- function(conn = dolt()) {
  observer <- getOption("connectionObserver")
  if (!is.null(observer)) {
    observer$connectionUpdated(
      type = "Dolt",
      host = dolt_conn_name(conn),
      hint = ""
    )
  }
}

#' @export
#' @rdname dolt_pane
close_dolt_pane <- function(conn = dolt()) {
  observer <- getOption("connectionObserver")
  if (!is.null(observer)) {
    observer$connectionClosed("Dolt", dolt_conn_name(conn))
  }
}

# Needed for using tidyselect's `where()` below
utils::globalVariables("where")


#' @importFrom dplyr mutate across if_else
#' @importFrom blob is_blob blob
dolt_preview_table  <- function(limit = 1000, table, schema = "", versioning = NULL, conn = dolt()) {
    s <- if (schema == "information_schema") "information_schema." else ""
    tab <- dolt_query_quiet(conn, paste("SELECT * FROM", s, table, "LIMIT", limit))
    tab <- mutate(tab, across(where(is.character), function(x) if_else(nchar(x) > 255, paste0(substr(x, 1, 252), "..."), x)))
    tab <- mutate(tab, across(where(function(x) all(vapply(x, is.raw, logical(1)))), function(x) format(as_blob(x))))
    tab
}

format_raw <- function(x, digits = 3) {
  x <- vapply(x, length, numeric(1))
}

dolt_connect_code <- function(conn) {
  if (inherits(conn, "DoltLocalConnection")) {
    paste0("doltr::dolt_pane(doltr::dolt(dir = '", conn@dir, "'))")
  } else {
    paste0("doltr::dolt_pane(doltr::dolt('remote',\n",
           "  dbname = '", conn@db, "',\n",
           "  username = '", conn@username, "',\n",
           "  password = Sys.getenv('DOLT_PASSWORD', ''),\n",
           "  port = ", conn@port, "L,\n",
           "  host = '", conn@host, "'))"
    )
  }
}

dolt_db_obj_types <- function() {
  list(
    schema = list(contains =
                    list(
                      table = list(contains = "data"),
                      versioning = list(
                        icon = ifile("diff.png"),
                        contains = list(
                          table = list(contains = "data")
                        )
                      )
                    ),
                  table = list(contains = "data")),
    info = list())
}

sql_action <- function() {
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
      exists("documentNew", asNamespace("rstudioapi"))) {
    contents <- paste(
      "-- !preview conn=dolt()",
      "",
      "SELECT * FROM dolt_log LIMIT 100",
      "",
      sep = "\n"
    )

    rstudioapi::documentNew(
      text = contents, type = "sql",
      position = rstudioapi::document_position(2, 40),
      execute = FALSE
    )
  }
}
