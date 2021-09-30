#' @importFrom httpuv startServer
port_fallback <- function(port, ...) {
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
