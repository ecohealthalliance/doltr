#' @title Connect to a dolt database
#'
#' @description `dolt_remote()` is a DBI Driver to connect to a dolt server via
#' a port. It, `DoltDriver`,
#' and `DoltConnection` class are wrappers around the around classes and methods
#' from the [`RMariaDB`][RMariaDB::MariaDB] package.
#'
#' Most parameters can be specified with environment variables See [config].
#'
#' @usage DBI::dbConnect(doltr::dolt_remote(), ...)
#' @param drv an object of class `DoltDriver`, created by `dolt_remote()`.
#' @param dbname The database name
#' @param username The username. Defaults to "root"
#' @param password The login password.  Defaults to empty.
#' @param host The IP of the host. Defaults to the local machine, `127.0.0.1`
#' @param port The TCP port for connections. Defaults to 3306.
#' @param autocommit Whether to autocommit changes in the _SQL_ sense. That is,
#'   to flush pending changes to disk and update the working set.
#' @param ... other arguments passed to [RMariaDB::MariaDB]
#' @details Most methods fall back to those for [`RMariaDB`][RMariaDB::MariaDB].
#' @export
#' @import DBI
#' @import methods
#' @import RMariaDB
#' @importClassesFrom RMariaDB MariaDBDriver MariaDBConnection
#' @family connections
dolt_remote <- function() {
  new("DoltDriver")
}

#' @export
#' @noRd
setClass("DoltDriver", contains = "MariaDBDriver")

#' @export
#' @noRd
setMethod("dbUnloadDriver", "DoltDriver", function(drv, ...) { TRUE })

#' @export
#' @noRd
setMethod("show", "DoltDriver", function(object) { cat("<DoltDriver>\n") })

#' @export
#' @noRd
setClass("DoltResult", contains = "MariaDBResult")

#' @export
#' @noRd
setClass("DoltConnection", contains = "MariaDBConnection",
         slots = c(port = "integer", username = "character"))

#' @export
#' @rdname dolt_remote
setMethod("dbConnect", "DoltDriver",
          function(drv, dbname = Sys.getenv("DOLT_DIR", "doltdb"),
                   username = Sys.getenv("DOLT_USERNAME", "root"),
                   password = Sys.getenv("DOLT_PASSWORD", ""),
                   host = Sys.getenv("DOLT_HOST", "127.0.0.1"),
                   port = Sys.getenv("DOLT_PORT", 3306L),
                   autocommit = TRUE, ...) {
            conn <- dbConnect(RMariaDB::MariaDB(),
                              dbname = dbname,
                              username = username,
                              password = password,
                              port = port,
                              host = host,
                              ...)
            attr(conn, "db") <- dbname
            attr(conn, "port") <- port
            attr(conn, "username") <- username
            attr(conn, "class") <- structure("DoltConnection", package = "doltr")
            dbExecute(conn, paste0("SET @@autocommit = ", as.integer(autocommit)))
            conn
          })

#' @export
#' @noRd
setMethod("dbGetInfo", "DoltConnection", function(dbObj, ...) {
  minfo <- getMethod(dbGetInfo, "MariaDBConnection")(dbObj)
  minfo$port <- dbObj@port
  last_commit <- dolt_last_commit()
  state <- dolt_state(dbObj)
  status <- dolt_status(dbObj)
  c(minfo, list(last_commit = last_commit, state = state, status = status))
})

#' @export
#' @importFrom cli cli_h1 cli_ul cli_li cli_end cli_alert_warning
#' @noRd
setMethod("show", "DoltConnection", function(object) {
  if (dbIsValid(object)) {
    info <- dbGetInfo(object)
    cli_h1("<DoltConnection> {info$dbname}")
    l <- cli_ul()
    cli_li("Connected at: {info$username}@{info$host}:{info$port}")
    cli_end(l)
    print(info$state)
    print(info$last_commit)
    print(info$status)
  } else {
    cli_alert_warning("DISCONNECTED")
  }
})


#' Dolt Data Types
#'
#' `dbDataType` fits more precise data types and returns a data type size attribute
#' in case fields need to be recast in revision. `dolt_type_sizes`
#' @param dbObj the database connection
#' @param obj the data type (vector or data frame)
#' @param min_varchar The minimum size `VARCHAR` types should be cast as
#' @param max_varchar the maximum size `VARCHAR` types should be cast as. Larger
#' text data will return types `TEXT`,`MEDIUMTEXT`, or `LONGTEXT`
#' @return A character vector of classes, with attributes of the maximum size for
#' text and blob classes
#' @export
#' @rdname types
setMethod("dbDataType", "DoltConnection", function(dbObj, obj,
                                                   min_varchar = Sys.getenv("DOLT_MINVARCAHR", 255L),
                                                   max_varchar = Sys.getenv("DOLT_MAXVARCHAR", 16383L),
                                                   ...) {
  min_varchar = as.integer(min_varchar)
  max_varchar = as.integer(max_varchar)
  if (is.data.frame(obj)) {
    typ_l <- lapply(obj, dolt_data_type,
                    min_varchar = min_varchar, max_varchar = max_varchar)
    typ <- unlist(typ_l)
    max_size <- numeric(length(obj))
    for(i in seq_along(max_size)) max_size[i] <- attr(typ_l[[i]], "max_size")
    attr(typ, "max_size") <- max_size
  } else {
    typ <- dolt_data_type(obj,  min_varchar, max_varchar)
  }

  typ
})

dolt_data_type <- function(obj, min_varchar, max_varchar) {
  if (is.factor(obj)) return(dolt_text_type(levels(obj), min_varchar, max_varchar))

  if (inherits(obj, "POSIXct"))   return(structure("DATETIME", max_size = NA_real_))
  if (inherits(obj, "Date"))      return(structure("DATE", max_size = NA_real_))
  if (inherits(obj, "difftime"))  return(structure("TIME", max_size = NA_real_))
  if (inherits(obj, "integer64")) return(structure("BIGINT", max_size = NA_real_))
  if (inherits(obj, "blob")) dolt_blob_type(obj)


  switch(typeof(obj),
         logical = structure("BOOLEAN", max_size = NA_real_), # works better than BIT(1), https://stackoverflow.com/q/289727/946850
         integer = structure("INT", max_size = NA_real_),
         double =  structure("DOUBLE", max_size = NA_real_),
         character = dolt_text_type(obj, min_varchar, max_varchar),
         list = dolt_blob_type(obj),
         stop("Unsupported type", call. = FALSE)
  )
}

dolt_text_type <- function(obj, min_varchar, max_varchar) {
  nc <- max(nchar(enc2utf8(obj), type = "bytes"), 1, na.rm = TRUE)
  if (nc <= min_varchar) {
    return(structure(paste0("VARCHAR(", min_varchar, ")"), max_size = min_varchar))
  } else if (nc > min_varchar & nc <= 16383 & nc < max_varchar) {
    sz <- 2^(floor(log2(nc)) + 1) - 1
    return(structure(paste0("VARCHAR(", sz, ")"), max_size = sz))
  } else if (nc > max_varchar && nc <= 16383) {
    return(structure("TEXT", max_size = 16383))
  } else if ((nc > 16383 || nc > max_varchar) && nc <= 4194303) {
    return(structure("MEDIUMTEXT", max_size = 4194303))
  } else if (nc > 4194303 && nc <= 4294967295) {
    return(structure("LONGTEXT", max_size = 4294967295))
  } else {
    stop("Text data is greater than 4GB! No storage type fits.")
  }
}

#' @importFrom blob as_blob
dolt_blob_type <- function(obj) {
  if (!all(vapply(obj, is.raw, logical(1)))) "Stop only lists of raw vectors (blobs) allowed"
  nb <- max(vapply(obj, \(x) length(x), 1), 1, na.rm = TRUE)
  if (nb <=  65535) {
    return(structure("BLOB", max_size = 65535))
  } else if (nb > 65535L && nb <= 16777215) {
    return(structure("MEDIUMBLOB", max_size = 16777215))
  } else if (nb > 16777215 && nb <= 4294967295) {
    return(structure("LONGBLOB", max_size = 4294967295))
  } else if (nb > 4294967295) {
    stop("Blob data is greater than 4GGB! No storage type fits")
  }
}

#' @export
#' @importFrom dplyr case_when
#' @rdname types
dolt_type_sizes <- function(types) {
  case_when(
    grepl("^VARCHAR", types) ~ numeric(regextract(types, "\\d+")),
    gripl("TEXT", types) ~ 16383,
    gripl("MEDIUMTEXT", types) ~ 4194303,
    gripl("LONGTEXT", types) ~ 4294967295,
    gripl("BLOB", types) ~ 65535,
    gripl("MEDIUMBLOB", types) ~ 16777215,
    gripl("LONGBLOB", types) ~ 4294967295,
    TRUE ~ NA_real_)
}


#' Makes results DoltResults rather than MariaDBResults, needed for methods
#' @importMethodsFrom RMariaDB dbSendQuery
#' @export
#' @noRd
setMethod(
  "dbSendQuery", c("DoltConnection", "character"),
  function(conn, statement, params = NULL, ...) {
    rs <- getMethod(dbSendQuery, c("MariaDBConnection", "character"))(
      conn, statement, params, ...)
    attr(rs, "class") <- structure("DoltResult", package = "doltr")
    rs
  }
)

#' Makes results DoltResults rather than MariaDBResults, needed for DoltResult
#' @importMethodsFrom RMariaDB dbSendStatement
#' @export
#' @noRd
setMethod(
  "dbSendStatement", signature("DoltConnection", "character"),
  function(conn, statement, params = NULL, ...) {
    rs <- getMethod(dbSendStatement, c("MariaDBConnection", "character"))(
      conn, statement, params, ...)
    attr(rs, "class") <- structure("DoltResult", package = "doltr")
    rs
  }
)

#' Sets a hook in interactive mode to update watching processes (e.g. the R
#' RStudio connection pane) when DB transactions finish
#' @importMethodsFrom RMariaDB dbClearResult
#' @export
#' @noRd
setMethod("dbClearResult", signature("DoltResult"),
          function(res, ...) {
            if (interactive() && Sys.getboolenv("DOLT_WATCH", TRUE)) {
              on.exit(dolt_watch(res@conn))
            }
            getMethod(dbClearResult, "MariaDBResult", )(res, ...)
          }
)



#' Clear out cached connections and watching panes when closing a connection
#' @importMethodsFrom RMariaDB dbDisconnect
#' @export
#' @noRd
setMethod("dbDisconnect", "DoltConnection", function(conn, ...) {
  conn_name <- dolt_conn_name(conn)
  getMethod(dbDisconnect, "MariaDBConnection")(conn)
  suppressWarnings(rm(list = conn_name, envir = dolt_cache))
  suppressWarnings(rm(list = conn_name, envir = dolt_states))
  if (interactive()) close_dolt_pane(conn)
})
