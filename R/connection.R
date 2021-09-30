#' Driver for dolt database.
#'
#' @keywords internal
#' @export
#' @import DBI
#' @import methods
#' @import RMariaDB
#' @importClassesFrom RMariaDB MariaDBDriver
#' @noRd
setClass("DoltDriver", contains = "MariaDBDriver")

#' @export
#' @noRd
setMethod("dbUnloadDriver", "DoltDriver", function(drv, ...) {
  TRUE
})

#' @export
#' @noRd
setMethod("show", "DoltDriver", function(object) {
  cat("<DoltDriver>\n")
})

#' @export
#' @noRd
setClass("DoltResult", contains = "MariaDBResult")


#' @title Connect to a dolt database
#'
#' @description `dolt()` is a DBI Driver for a Dolt Database. It, `DoltDriver`,
#' and `DoltConnection` class are wrappers around the around classes and methods
#' from the [`RMariaDB`][RMariaDB::MariaDB] package.
#' @usage DBI::dbConnect(doltr::dolt(), ...)
#' @details Methods not shown here fall back to those for [RMariaDB][RMariaDB::MariaDB].
#' @export
#' @rdname dolt-class
dolt <- function() {
  new("DoltDriver")
}

#' @importClassesFrom RMariaDB MariaDBConnection
#' @export
#' @keywords internal
#' @noRd
setClass("DoltConnection", contains = "MariaDBConnection")

#' @export
#' @rdname dolt-class
setMethod("dbConnect", "DoltDriver",
          function(drv, dbname, username, password, host, port, ...) {
            conn <- dbConnect(RMariaDB::MariaDB(),
                     dbname = Sys.getenv("DOLT_DIR", "doltdb"),
                     username = Sys.getenv("DOLT_USERNAME", "root"),
                     password = Sys.getenv("DOLT_PASSWORD", ""),
                     port = Sys.getenv("DOLT_PORT", 3306L),
                     host = Sys.getenv("DOLT_HOST", "127.0.0.1"),
                     ...)
   attr(conn, "class") <- structure("DoltConnection", package = "doltr")
   conn
})

#' @export
#' @rdname dolt-class
setMethod("dbGetInfo", "DoltConnection", function(dbObj, ...) {
  minfo <- getMethod(dbGetInfo, "MariaDBConnection")(dbObj)
  minfo$port <- NULL
  refs <- as.list(dbGetQuery(dbObj, paste0("select ",
  "@@port as port, ",
  "@@", minfo$dbname, "_head_ref AS head_ref, ",
  "@@", minfo$dbname, "_head as head, ",
  "@@", minfo$dbname, "_staged as staged, ",
  "@@", minfo$dbname, "_working as working")))
  refs$port <- as.integer(refs$port)
  status <- dbGetQuery(dbObj, "select * from dolt_status")
  c(minfo, refs, list(status = status))
})

#' @export
#' @rdname dolt-class
#' @importFrom cli cli_h1 cli_ul cli_li cli_end cli_alert_warning
setMethod("show", "DoltConnection", function(object) {
  info <- dbGetInfo(object)
  cli_h2("<DoltConnection> {info$dbname}")
  if (dbIsValid(object)) {
    l <- cli_ul()
    cli_li("Connected at: {info$username}@{info$host}:{info$port}")
    cli_li("HEAD: {info$head_ref} {info$head}")
    cli_li("Status:")
    cli_end(l)
    print(info$status) #TODO: pretty-print status info better, use line for working and staged,
  } else {
    cli_alert_warning("DISCONNECTED")
  }
})

