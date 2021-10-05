#' Return a boolean value from a system variable.
#' @param default the value returned if there is no variable
#' @param stop_on_failure whether to error if there is no variable
#' @noRd
Sys.getboolenv <- function (x, default = FALSE, stop_on_failure = FALSE)
{
  r <- Sys.getenv(x)
  if (identical(r, "")) {
    if (isTRUE(stop_on_failure)) {
      stop(paste("Environment variable", x, "is not set."))
    }
    else {
      r <- default
    }
  }
  else {
    r <- tryCatch(as.numeric(r), warning = function(w) return(r))
    r <- as.logical(r)
    if (is.na(r)) {
      if (isTRUE(stop_on_failure)) {
        stop(paste("Environment variable", x, "is set to",
                   "a value not interpretable as boolean."))
      }
      else {
        r <- default
      }
    }
    else {
    }
  }
  return(r)
}

#' Return the provided port unless it is busy, then select a random one
#' @importFrom httpuv startServer
#' @noRd
port_fallback <- function(port, ...) {
  port <- as.integer(port)
  s <- NULL
  tryCatch(s <- httpuv::startServer("127.0.0.1", port, list(), quiet = TRUE),
           error = function(e) {})
  if (is.null(s)) {
    return(httpuv::randomPort(...))
  } else {
    s$stop()
    return(port)
  }
}

## copied from rematch2@180fb61
re_match <- function(text, pattern, perl = TRUE, ...) {

  stopifnot(is.character(pattern), length(pattern) == 1, !is.na(pattern))
  text <- as.character(text)

  match <- regexpr(pattern, text, perl = perl, ...)

  start  <- as.vector(match)
  length <- attr(match, "match.length")
  end    <- start + length - 1L

  matchstr <- substring(text, start, end)
  matchstr[ start == -1 ] <- NA_character_

  res <- data.frame(
    stringsAsFactors = FALSE,
    .text = text,
    .match = matchstr
  )

  if (!is.null(attr(match, "capture.start"))) {

    gstart  <- attr(match, "capture.start")
    glength <- attr(match, "capture.length")
    gend    <- gstart + glength - 1L

    groupstr <- substring(text, gstart, gend)
    groupstr[ gstart == -1 ] <- NA_character_
    dim(groupstr) <- dim(gstart)

    res <- cbind(groupstr, res, stringsAsFactors = FALSE)
  }

  names(res) <- c(attr(match, "capture.names"), ".text", ".match")
  class(res) <- c("tbl_df", "tbl", class(res))
  res
}


`%||%` <- function (a, b) if (!is.null(a)) a else b

## Taken from the remotes package


parse_repo_spec <- function (repo) {

  package_name_rx <- "(?:(?<package>[[:alpha:]][[:alnum:].]*[[:alnum:]])=)?"
  username_rx <- "(?:(?<username>[^/]+)/)"
  repo_rx <- "(?<repo>[^/@#]+)"
  subdir_rx <- "(?:/(?<subdir>[^@#]*[^@#/])/?)?"
  ref_rx <- "(?:@(?<ref>[^*].*))"
  pull_rx <- "(?:#(?<pull>[0-9]+))"
  release_rx <- "(?:@(?<release>[*]release))"
  ref_or_pull_or_release_rx <- sprintf("(?:%s|%s|%s)?", ref_rx,
                                       pull_rx, release_rx)
  spec_rx <- sprintf("^%s%s%s%s%s$", package_name_rx, username_rx,
                     repo_rx, subdir_rx, ref_or_pull_or_release_rx)
  params <- as.list(re_match(text = repo, pattern = spec_rx))
  if (is.na(params$.match)) {
    stop(sprintf("Invalid git repo specification: '%s'",
                 repo))
  }
  params[grepl("^[^\\.]", names(params))]
}
