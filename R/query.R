#' @importFrom dplyr tbl collect sql
dolt_query <- function(query, conn = dolt(),
                       collect = Sys.getboolenv("DOLT_COLLECT", TRUE),
                       show_sql = Sys.getboolenv("DOLT_SHOW_SQL", FALSE)) {
  query <- sql(query)
  if (show_sql) message(query)
  result <- tbl(conn, query)
  if (collect) result <- collect(result)
  result
}

