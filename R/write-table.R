#' @include dolt-connection.R
NULL

#' Write a table to the database
#'
#' This method uses [dbx::dbxInsert()] as that implementation
#' is much more performant than the standard method from [RMariaDB::dbWriteTable()][RMariaDB::mariadb-tables],
#' due to the way Dolt handles repeat `INSERT` statements.
#'
#' This the dependency on `dbx` may be removed if the base issue is resolved: <https://github.com/dolthub/dolt/issues/2091>.
#'
#' @param batch_size The number of records to insert in a single SQL statement
#'   (defaults to all)
#' importFrom dbx dbxInsert. REMOVED BY NCL. CUSTOM SF FRIENDLY dbxInsert
#' @inheritParams DBI::sqlRownamesToColumn
#' @param conn a database connection
#' @param overwrite a logical specifying whether to overwrite an existing table
#'   or not. Its default is `FALSE`.
#' @param append a logical specifying whether to append to an existing table
#'   in the database  If appending, then the table (or temporary table)
#'   must exist, otherwise an error is reported. Its default is `FALSE`.
#' @param name the table name
#' @param value A data frame.
#' @param field.types Optional, overrides default choices of field types,
#'   derived from the classes of the columns in the data frame. See [dbDataType()][dbDataType,DoltConnection-method]
#' @param temporary If `TRUE`, creates a temporary table that expires
#'   when the connection is closed. For `dbRemoveTable()`, only temporary
#'   tables are considered if this argument is set to `TRUE`
#' @param batch_size The number of records to insert in a single statement (defaults to all)
#' @param ... for additional parameters passed on. Not currently used.
#' @export
#' @rdname dolt-write
#' @seealso dolt-read
setMethod("dbWriteTable", c("DoltConnection", "character", "data.frame"),
          function(conn, name, value, field.types = NULL, row.names = FALSE,
                   overwrite = FALSE, append = FALSE, temporary = FALSE,
                   batch_size = NULL, ...) {
            if (!is.data.frame(value))  {
              stopc("`value` must be data frame")
            }

            row.names <- compatRowNames(row.names)

            if ((!is.logical(row.names) && !is.character(row.names)) || length(row.names) != 1L)  {
              stopc("`row.names` must be a logical scalar or a string")
            }
            if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite))  {
              stopc("`overwrite` must be a logical scalar")
            }
            if (!is.logical(append) || length(append) != 1L || is.na(append))  {
              stopc("`append` must be a logical scalar")
            }
            if (!is.logical(temporary) || length(temporary) != 1L)  {
              stopc("`temporary` must be a logical scalar")
            }
            if (overwrite && append) {
              stopc("overwrite and append cannot both be TRUE")
            }
            if (!is.null(field.types) && !(is.character(field.types) && !is.null(names(field.types)) && !anyDuplicated(names(field.types)))) {
              stopc("`field.types` must be a named character vector with unique names, or NULL")
            }
            if (append && !is.null(field.types)) {
              stopc("Cannot specify `field.types` with `append = TRUE`")
            }

            if (!temporary) {
              found <- dbExistsTable(conn, name)
              if (found && !overwrite && !append) {
                stop("Table ", name, " exists in database, and both overwrite and",
                     " append are FALSE", call. = FALSE)
              }
            } else {
              found <- FALSE
            }


            if (overwrite) {
              dbRemoveTable(conn, name, temporary = temporary,
                            fail_if_missing = FALSE)
            }

            # row.names <- compatRowNames(row.names) # Already done on line 42
            value <- sqlRownamesToColumn(value, row.names)
            value <- factor_to_string(value)

            if (!found || overwrite) {
              if (is.null(field.types)) {
                combined_field_types <- dbDataType(conn, value)
              } else {
                combined_field_types <- rep("", length(value))
                names(combined_field_types) <- names(value)
                field_types_idx <- match(names(field.types), names(combined_field_types))
                stopifnot(!any(is.na(field_types_idx)))
                combined_field_types[field_types_idx] <- field.types
                values_idx <- setdiff(seq_along(value), field_types_idx)
                combined_field_types[values_idx] <- lapply(value[values_idx], dbDataType, dbObj = conn)
              }

              dbCreateTable(
                conn = conn,
                name = name,
                fields = combined_field_types,
                temporary = temporary
              )

              if (nrow(value) > 0) {
                dbxInsert(
                  conn = conn,
                  table = name,
                  records = value,
                  batch_size = batch_size
                )
              }
            }
            invisible((TRUE))
          })




# dolt_import_csv <- function(conn = dolt()) {
#   if (!inherits(conn, "DoltLocalConnection")) stop("Only Local Dolt Connections Allowed")
#   TODO: Move elsewhere and use LOAD DATA command (https://docs.dolthub.com/interfaces/sql/sql-support/supported-statements,
#   https://www.google.com/search?client=firefox-b-1-d&q=mysql+load+data&shem=ssmd
#   https://www.mysqltutorial.org/import-csv-file-mysql-table/
# }

