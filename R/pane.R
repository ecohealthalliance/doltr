# TODO: Break this into smaller functions

#' Open a Dolt connection pane in RStudio
#'
#' This function launches the RStudio "Connection" pane to interactively
#' explore the database.
#'
#' @export
#' @return The connection object (invisibly)
#' @examples
#' if (!is.null(getOption("connectionObserver"))) dolt_pane()
#' @importFrom rscontract rscontract_spec rscontract_open rscontract_ide
dolt_pane <- function(conn = doltr::dolt()) {
  conn_arg <- paste(deparse(substitute(conn)), collapse = "\n")
  info <- dbGetInfo(conn)
  spec <- rscontract_ide(
    connectionObject = conn,
    type = paste0(info$username, "@", info$host, ":", info$port),
    host = paste0(info$username, "@", info$host, ":", info$port),
    displayName = info$dir %||% info$dbname,
    connectCode = paste0("doltr::dolt_pane(", conn_arg, ")"),
    disconnect = function() DBI::dbDisconnect(conn),
    listObjectTypes = function() {
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
    },
    listObjects = function(schema = NULL, versioning = NULL) {
      if (is.null(schema)) {
        HEAD_REF <- paste0("Branch: ", dbGetQuery(conn, "select @@doltdb_head_ref")[[1]])
        HEAD_HASH <- paste0("Head Hash: ", dbGetQuery(conn, paste0("select @@", info$dbname, "_head"))[[1]])
        WORKING_HASH <- paste0("Working Hash:" , dbGetQuery(conn, paste0("select @@", info$dbname, "_working"))[[1]])
        tbls <- dbListTables(conn)
        out <- data.frame(
          name = c(HEAD_REF, HEAD_HASH, WORKING_HASH, tbls, "dolt_system_tables", "information_schema"),
          type = c("info", "info", "info", rep("table", length(tbls)), "schema", "schema"),
          stringsAsFactors = FALSE
        )
      } else if (schema == "information_schema") {
        out <- dbGetQuery(conn, "select table_name as name, 'table' as type from information_schema.tables where table_schema = 'information_schema'")
      } else if (schema == "dolt_system_tables" && is.null(versioning)) {
        names <- paste0("dolt_", c("branches", "docs", "log", "status",
                                   "procedures", "remotes"))
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
    previewObject = function(limit = 1000, table, schema = "", versioning = NULL) { # nolint
      s <- if (schema == "information_schema") "information_schema." else ""
      DBI::dbGetQuery(conn, paste("SELECT * FROM", s, table, "LIMIT", limit))
    },
    actions = list(
      Stage = list(
        icon = ifile("staged.png"),
        callback = function() doltr::dolt_add(conn = conn)
      ),
      Unstage = list(
        icon = ifile("unstaged.png"),
        callback = function() doltr::dolt_reset(conn = conn)
      ),
      Commit = list(
        icon = ifile("commit.png"),
        callback = function() doltr::dolt_commit(conn = conn)
      ),
      Pull = list(
        icon = ifile("pull.png"),
        callback = function() doltr::dolt_pull(conn = conn)
      ),
      Push = list(
        icon = ifile("push.png"),
        callback = function() doltr::dolt_push(conn = conn)
      ),
      History = list(
        icon = ifile("history.png"),
        callback = function() view2(x = dolt_log(conn = conn), title = "Dolt Log")
      )
    )
  )
  rscontract_open(spec)
  invisible(conn)
}

ifile <- function(f) {
  system.file("img", f, package = "doltr")
}

# update_dolt_pane <- function() {
#   observer <- getOption("connectionObserver")
#   if (!is.null(observer)) {
#     observer$connectionUpdated("CITESDB", "citesdb", "")
#   }
# }
