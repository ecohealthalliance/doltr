#' @export
#' @importFrom dplyr mutate group_by summarize pull recode %>%
dolt_statusline <- function(status_table = dolt_status()) {
  if (!nrow(status_table))
    out <- c(Clean="Working database clean")
  else {
  out <- status_table %>%
    mutate(status = recode(status, `new table`="new", `new doc`="new"),
           staged =  c("Working", "Staged")[staged + 1]) %>%
    group_by(staged, status) %>%
    summarize(changed = paste0(paste(table_name, collapse = ", "), " (", status[1], ")")) %>%
    group_by(staged) %>%
    summarize(changed = paste0(staged[1], ": ", paste(changed, collapse = ", "))) %>%
    pull(changed, name = staged)
  }
  class(out) <- c("dolt_statusline", class(out))
  out
}

#' @export
#' @importFrom cli col_green col_yellow col_red cat_line
print.dolt_statusline <- function(x) {
  if (!is.na(x["Clean"])) cat_line(col_green(x["Clean"]))
  if (!is.na(x["Staged"])) cat_line(col_yellow(x["Staged"]))
  if (!is.na(x["Working"])) cat_line(col_red(x["Working"]))
}
