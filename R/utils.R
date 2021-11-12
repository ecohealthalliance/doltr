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

#' Convenience if.null binomial function
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) return(y) else return(x)



#' Use the RStudio viewer programatically
#' @noRd
get_rsiew <- function() {
  if (Sys.getenv("RSTUDIO") == "1") {
    return(as.environment("tools:rstudio")[[".rs.viewHook"]])
  } else {
    return(NULL)
  }
}
view2 <- function(x, ...) {
  vfn <- get_rsiew()
  if (is.null(vfn)) {
    utils::View(x)
  } else {
    vfn(x,x,...)
  }
}

#' Base version of stri_extract
#' @noRd
regextract <- function(x, pat, ignore.case = FALSE, perl = TRUE, fixed = FALSE) {
  match <- regexpr(pat, x, ignore.case = ignore.case, perl = perl, fixed = fixed)
  out <- rep(NA_character_, length(x))
  out[match != -1] <- regmatches(x, match)
  out
}

#' A convenience function
#' @noRd
gripl <- function(pat, x) grepl(pat, x, ignore.case = TRUE, fixed = FALSE)


## Stuff from RMariaDB
stopc <- function (...) {
  stop(..., call. = FALSE, domain = NA)
}

compatRowNames <- function(row.names) {
  if (is.null(row.names)) {
    row.names <- FALSE
  }
  row.names
}


factor_to_string <- function (value, warn = FALSE) {
  is_factor <- vlapply(value, is.factor)
  if (warn && any(is_factor)) {
    warning("Factors converted to character", call. = FALSE)
  }
  value[is_factor] <- lapply(value[is_factor], as.character)
  value
}

vlapply <- function(X, FUN, ..., USE.NAMES = TRUE) {
  vapply(X = X, FUN = FUN, FUN.VALUE = logical(1L), ..., USE.NAMES = USE.NAMES)
}

vcapply <- function(X, FUN, ..., USE.NAMES = TRUE) {
  vapply(X = X, FUN = FUN, FUN.VALUE = character(1L), ..., USE.NAMES = USE.NAMES)
}

as_table <- function(schema, table) {
  args <- c(schema = schema, table = table)
  # Also omits NA args
  args <- args[!is.na(args) & args != ""]
  do.call(Id, as.list(args))
}
