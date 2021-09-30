Sys.getboolenv <- function (x, stop_on_failure = FALSE)
{
  r <- Sys.getenv(x)
  if (identical(r, "")) {
    if (isTRUE(stop_on_failure)) {
      throw(paste("Environment variable", x, "is not set."))
    }
    else {
      r <- FALSE
    }
  }
  else {
    r <- tryCatch(as.numeric(r), warning = function(w) return(r))
    r <- as.logical(r)
    if (is.na(r)) {
      if (isTRUE(stop_on_failure)) {
        throw(paste("Environment variable", x, "is set to",
                    "a value not interpretable as boolean."))
      }
      else {
        r <- FALSE
      }
    }
    else {
    }
  }
  return(r)
}
