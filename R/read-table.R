#' @include dolt-connection.R
NULL

#' Reading from a Dolt database.
#'
#' These methods are extensions of standard DBI functions such as [DBI::dbReadTable].
#' They differ in that they can take an `as_of` argument, reading historical data
#' from the database that was written as of a certain date or commit hash, or
#' from a different branch.
#'
#' @seealso [Querying Historical Data with AS OF Queries](https://www.dolthub.com/blog/2020-03-20-querying-historical-data-with-as-of/)
#' on the DoltHub blog, and [RMariaDB methods][RMariaDB::mariadb-tables] upon
#' which these are built.
#' @return A data.frame in the case of `dbReadTable()`; a character vector of
#' names for `dbListTables()` and `dbListObjects()`, and a logical result for
#' `dbExistsTable()`.
#' @param conn a [dolt connection][dolt_remote()] object, produced by
#'   [DBI::dbConnect()] or [dolt()]
#' @param name a character string specifying a table name.
#' @param check.names If `TRUE`, the default, column names will be
#'   converted to valid R identifiers.
#' @param as_of A dolt commit hash, branch name, or object coercible to POSIXct
#' @inheritParams DBI::sqlRownamesToColumn
#' @param ... Unused, needed for compatibility with generic.
#' @name dolt-read
NULL

#' @export
#' @rdname dolt-read
setMethod("dbReadTable", c("DoltConnection", "character"),
          function(conn, name, as_of = NULL, ..., row.names = FALSE, check.names = TRUE) {
            row.names <- compatRowNames(row.names)

            if ((!is.logical(row.names) && !is.character(row.names)) || length(row.names) != 1L)  {
              stopc("`row.names` must be a logical scalar or a string")
            }

            if (!is.logical(check.names) || length(check.names) != 1L)  {
              stopc("`check.names` must be a logical scalar")
            }

            name <- dbQuoteIdentifier(conn, name)
            query <- paste("SELECT * FROM ", name)
            if (!is.null(as_of)) query <- query_as_of(query, as_of)

            out <- dbGetQuery(conn, query,
                              row.names = row.names)

            if (check.names) {
              names(out) <- make.names(names(out), unique = TRUE)
            }

            out
          }
)

query_as_of <- function(query, as_of) {
  as_of <- tryCatch(
    paste0("TIMESTAMP('", as.character(as.POSIXct(as_of)), "')"),
    error = function(e) paste("'", as_of, "'")
  )
  query <- paste0(query, " AS OF ", as_of)
  query
}

#' @export
#' @rdname dolt-read
setMethod("dbListTables", "DoltConnection", function(conn, as_of = NULL, ...) {
  # DATABASE(): https://stackoverflow.com/a/8096574/946850
  query <- paste0("SELECT table_name FROM INFORMATION_SCHEMA.tables\n",
                  "WHERE table_schema = DATABASE()")
  if (!is.null(as_of)) query <- query_as_of(query, as_of)

  dbGetQuery(conn, query)[[1]]

})


#' @export
#' @inheritParams DBI::dbListObjects
#' @rdname dolt-read
setMethod("dbListObjects", c("DoltConnection", "ANY"), function(conn, prefix = NULL, as_of = NULL, ...) {
  query <- NULL
  if (is.null(prefix)) {
    # DATABASE(): https://stackoverflow.com/a/8096574/946850
    query <- paste0(
      "SELECT NULL AS `schema`, table_name AS `table` FROM INFORMATION_SCHEMA.tables\n",
      "WHERE table_schema = DATABASE()\n",
      "UNION ALL\n",
      "SELECT DISTINCT table_schema AS `schema`, NULL AS `table` FROM INFORMATION_SCHEMA.tables"
    )
  } else {
    unquoted <- dbUnquoteIdentifier(conn, prefix)
    is_prefix <- vlapply(unquoted, function(x) { "schema" %in% names(x@name) && !("table" %in% names(x@name)) })
    schemas <- vcapply(unquoted[is_prefix], function(x) x@name[["schema"]])
    if (length(schemas) > 0) {
      schema_strings <- dbQuoteString(conn, schemas)
      query <- paste0(
        "SELECT table_schema AS `schema`, table_name AS `table` FROM INFORMATION_SCHEMA.tables\n",
        "WHERE ",
        "(table_schema IN (", paste(schema_strings, collapse = ", "), "))"
      )
    }
  }

  if (is.null(query)) {
    res <- data.frame(schema = character(), table = character(), stringsAsFactors = FALSE)
  } else {
    if (!is.null(as_of)) query <- query_as_of(query, as_of)
    res <- dbGetQuery(conn, query)
  }

  is_prefix <- !is.na(res$schema) & is.na(res$table)
  tables <- Map(res$schema, res$table, f = as_table)

  ret <- data.frame(
    table = I(unname(tables)),
    is_prefix = is_prefix,
    stringsAsFactors = FALSE
  )
  ret
})

#' @export
#' @rdname dolt-read
setMethod("dbExistsTable", c("DoltConnection", "character"),
          function(conn, name, as_of = NULL, ...) {
            stopifnot(length(name) == 1L)
            if (!dbIsValid(conn)) {
              stopc("Invalid connection")
            }
            tryCatch({
              query <- paste0("SELECT NULL FROM ",
                              dbQuoteIdentifier(conn, name))
              if (!is.null(as_of)) query <- query_as_of(query, as_of)
              query <- paste0(query, " WHERE FALSE")
              dbGetQuery(conn, query)
              TRUE
            }, error = function(...) {
              FALSE
            })
          }
)

compatRowNames <- function (row.names) {
  if (is.null(row.names)) {
    row.names <- FALSE
  }
  row.names
}
