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

`%||%` <- function(x, y) if (is.null(x)) return(y) else return(x)

view2 <- function(x, ...) {
  if (Sys.getenv("RSTUDIO") == "1") View(x, ...) else utils::View(x)
}

