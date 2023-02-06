# With these changes to dbx I can successfully create a table using
# dbCreate table (so long as I specify field.types ahead of time),
# add records with dbxInsert, pull the table back down as an sf
# object and decode the WKB blob with my version of sf_read.
# Not sure yet if dbWriteTable works.

dbxInsert <- function(conn, table, records, batch_size=NULL, returning=NULL, show_sql = T) {
  dbx:::inBatches(records, batch_size, function(batch) {
    sql <- insertClause(conn, table, batch)
    if(show_sql) message(sql)
    dbx:::selectOrExecute(conn, sql, batch, returning=returning)
  })
}

insertClause <- function(conn, table, records) {
  cols <- colnames(records)

  # quote
  quoted_table <- dbx:::quoteIdent(conn, table)
  quoted_cols <- dbx:::quoteIdent(conn, cols)

  cols_sql <- dbx:::colsClause(quoted_cols)
  records_sql <- valuesClause(conn, records)
  paste0("INSERT INTO ", quoted_table, " (", cols_sql, ") VALUES ", records_sql)
}

valuesClause <- function(conn, records) {
  quoted_records <- quoteRecords(conn, records)
  rows <- apply(quoted_records, 1, function(x) { paste0(x, collapse=", ") })
  paste0("(", rows, ")", collapse=", ")
}

########################################################################
################# PR to dbx would be the following #####################
########################################################################

# Check if column is sf
isSf <- function(col) {
  inherits(col, "sf") | inherits(col, "sfc")
}

# Add in gis function call and appropriate literal quoting for sf columns
quoteRecords <- function(conn, records) {
  quoted_records <- data.frame(matrix(ncol=0, nrow=nrow(records)))
  for (i in 1:ncol(records)) {

    col <- DBI::dbQuoteLiteral(conn, castData(conn, records[, i, drop=T]))

    if(isSf(records[, i, drop=T])) {
      crs <- unlist(str_split(st_crs(records)$input, ":"))[2] |> as.numeric()
      if(!is.na(crs))
        col <- paste0("ST_GeomFromWKB(X", DBI::dbQuoteLiteral(conn, col), ",", crs, ")")
      else {
        col <- paste0("ST_GeomFromWKB(X", DBI::dbQuoteLiteral(conn, col), ")") # Not sure if this is needed. What does mySQL do if EPSG is null or missing?
      }
    }
    quoted_records[, i] <- col
  }
  quoted_records
}

# Add in cast sf columns as wkb
castData <- function(conn, col) {
  if (dbx:::isMySQL(conn) || dbx:::isSQLite(conn) || dbx:::isSQLServer(conn)) {
    # since no standard for SQLite, store dates and datetimes in the same format as Rails
    # store times without dates as strings to keep things simple
    if(isSf(col)) {
      col <- sf:::db_binary(col)
    } else if (dbx:::isDatetime(col)) {
      col <- format(col, tz=storageTimeZone(conn), "%Y-%m-%d %H:%M:%OS6")
    } else if (dbx:::isDate(col)) {
      col <- format(col)
    } else if (dbx:::isTime(col)) {
      col <- format(col)
    }

  } else if (dbx:::isPostgres(conn)) {

    if (dbx:::isDatetime(col)) {
      col <- format(col, tz=storageTimeZone(conn), "%Y-%m-%d %H:%M:%OS6 %Z")
    } else if (dbx:::isTime(col)) {
      col <- format(col)
    } else if (is.logical(col) && dbx:::isRPostgreSQL(conn)) {
      col <- as.character(col)
    } else if (dbx:::isDate(col) && dbx:::isRPostgreSQL(conn)) {
      col <- format(col)
    } else if (isBinary(col)) {
      if (dbx:::isRPostgreSQL(conn)) {
        col <- as.character(lapply(col, function(x) { RPostgreSQL::postgresqlEscapeBytea(conn, x) }))
      } else {
        # removes AsIs
        col <- blob::as.blob(lapply(col, function(x) { x }))
      }
    } else if (isDifftime(col) && isRPostgres(conn)) {
      col <- as.character(col)
    }
  }

  col
}
