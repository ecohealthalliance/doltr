#' @importFrom dplyr tbl collect sql
dolt_query <- function(query, conn = dolt(),
                       collect = Sys.getboolenv("DOLT_COLLECT", TRUE),
                       show_sql = Sys.getboolenv("DOLT_VERBOSE", FALSE),
                       execute = F) {
  query <- sql(query)
  if (show_sql) message(query)
  if(!execute) {
    result <- tbl(conn, query)
    if (collect) result <- collect(result)
  } else {
    result <- RMariaDB::dbExecute(conn, query)
  }
  result
}


.collect <- function(collect) {
  if (is.null(collect))
    return(Sys.getboolenv("DOLT_COLLECT", TRUE))
  else
    return(collect)
}

.show_sql <- function(show_sql) {
  if (is.null(show_sql))
    return(Sys.getboolenv("DOLT_VERBOSE", FALSE))
  else
    return(show_sql)
}
