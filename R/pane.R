# TODO: Break this into smaller functions

#' Open a Dolt connection pane in RStudio
#'
#' This function launches the RStudio "Connection" pane to interactively
#' explore the database.
#'
#' @export
#'
#' @examples
#' if (!is.null(getOption("connectionObserver"))) dolt_pane()
#' @importFrom rscontract rscontract_spec rscontract_open rscontract_ide
#' @importFrom utils View
dolt_pane <- function(conn = doltr::dolt()) {
  conn_arg <- paste(deparse(substitute(conn)), collapse = "\n")
  info <- dbGetInfo(conn)
  spec <- rscontract_ide(
    connectionObject = conn,
    type = paste0(info$username, "@", info$host, ":", info$port),
    host = paste0(info$username, "@", info$host, ":", info$port),
    displayName = paste0(info$dbname, ": ", info$head_ref, ", ", info$head),
    connectCode = paste0("conn <- doltr::dolt_pane(", conn_arg, ")"),
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
        table = list(contains = "data")))
    },
    listObjects = function(schema = NULL, versioning = NULL) {
      if (is.null(schema)) {
        tbls <- dbListTables(conn)
        out <- data.frame(
          name = c(tbls, "dolt_sys", "information_schema"),
          type = c(rep("table", length(tbls)), "schema", "schema"),
          stringsAsFactors = FALSE
        )
      } else if (schema == "information_schema") {
        out <- dbGetQuery(conn, "select table_name as name, 'table' as type from information_schema.tables where table_schema = 'information_schema'")
      } else if (schema == "dolt_sys" && is.null(versioning)) {
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
      } else if (schema == "dolt_sys" && !is.null(versioning)) {
        cat("hello!")
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
      s <- if (schema == "information_schema") "information_schema." else ""
      res <- DBI::dbGetQuery(conn, paste0("select * from ", s, table, " limit 1"))
      data.frame(
        name = names(res), type = vapply(res, function(x) class(x)[1], character(1)),
        stringsAsFactors = FALSE
      )
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
        callback = function() View(x = dolt_log(conn = conn), title = "Dolt Log")
      )
    )
  )
  rscontract_open(spec)
  return(conn)
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
