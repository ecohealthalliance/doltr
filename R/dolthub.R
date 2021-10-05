#' Fetch data from the DoltHub API
#'
#' The API is alpha and does not yet support authentication.  See
#' <https://www.dolthub.com/blog/2020-08-21-dolthub-repository-apis/> and
#' <https://docs.dolthub.com/dolthub/api>
#' @param query an SQL query
#' @param repo A doltub repo [remotes shorthand form][remotes::parse_repo_spec],
#'   e.g., `username/repo@refspec`. `@refspec` is optional
#' @param base_url The DoltHub API endpoint
#' @importFrom httr GET content
#' @importFrom dplyr bind_rows
#' @example dh_get("Show full tables where table_type='VIEW'", repo = "dolthub/nba-players")
dh_get <- function(query = "SHOW TABLES;", repo = NULL,
    base_url = "https://www.dolthub.com/api/v1alpha1") {

  nc <- nchar(query)
  if (substr(query, nc, nc) != ";") query <- paste0(query, ";")

  rp <- parse_repo_spec(repo)
  url <- paste(base_url, rp$username, rp$repo, rp$re, sep = "/")

  res <- GET(url, query = list(q = query))
  cont <- content(res)
  schema <- bind_rows(cont$schema)
  dat <- bind_rows(cont$rows)

  # for(i %in% seq_len(nrow(schema))) {
  #   dat[[i]][dat[[i]] == 'null', ] <- NA_character_
  #   dat[[i]] <- as(dat[[i]], schema)
  # }

  attr(dat, "schema") <- schema
  class(dat) <- c("dh_reponse", class(dat))

  #TODO: Use the schema to set data types in return Convert null to NA
  #TODO: Mess around more with AS OF and refspec to test

  dat
}

dolt_non_char_types <- c(
  "BIGINT" = "integer",
  #"LONGTEXT" = "character",
  #"DOUBLE" = "double",
  #"TEXT" = "character",
  "VARCHAR" = "character"

)
