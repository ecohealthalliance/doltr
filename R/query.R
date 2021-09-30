#' @importFrom dbplyr as.sql
dolt_query <- function(query, conn,
                       return = Sys.getenv("DOLT_RETURN", "data.frame"),
                       show_sql = Sys.getboolenv(DOLT_SHOW_SQL, "")) {
  query <- as.sql(query, conn)
  if (show_sql) print(query)
  out <- switch(
    return,
    data.frame = dbGetQuery(query, conn),
    tibble = collect(tbl(conn, query)),
    tbl_lazy = tbl(conn, query),
    ... = stop("`return` must be one of `data.frame`, `tibble`, or `tbl_lazy`")
  )
}

