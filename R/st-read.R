#' Title
#'
#' @param dsn DoltLocalConnection.
#' @param layer character.
#'
#' @return
#' @export
#' @import sf
#'
#' @examples
setMethod("st_read", signature("DoltLocalConnection", "character"),
          function(dsn = NULL,
                   layer = NULL,
                   query = NULL,
                   EWKB = TRUE,
                   quiet = TRUE,
                   as_tibble = FALSE,
                   geometry_column = NULL,
                   ...) {
  if (is.null(dsn))
    stop("no connection provided") # nocov

  if (as_tibble && !requireNamespace("tibble", quietly = TRUE)) {
    stop("package tibble not available: install first?") # nocov
  }

  # check that ellipsis contains only what is needed
  expe <- setdiff(names(list(...)), names(formals(st_sf)))
  if(length(expe) > 0) {
    # error,  these arguments would be passed to st_sf
    suggest <- NULL
    if("table" %in% expe){
      suggest <- c(suggest, "\nMaybe you should use `layer` rather than `table` ?")
    }
    pref <- if(length(expe) > 1) "\t *" else  ""
    stop(
      "Unused arguments: ",
      if(length(expe) > 1) "\n" else "",
      paste(pref, expe, "=", list(...)[expe], collapse = "\n", sep = " "),
      suggest,
      "\nCheck arguments for `st_sf()` for details.",
      call. = FALSE
    )
  }

  # filter expected warnings (for RPostgreSQL driver)
  filter_warning <- function(expr, regexp) {
    wlist <- NULL
    warning_handler <- function(w) {
      wlist <<- c(wlist, list(w))
      invokeRestart("muffleWarning")
    }
    msg <- function(x) x$message
    out <- withCallingHandlers(expr, warning = warning_handler)
    if(!all(grepl(regexp, wlist))) {
      lapply(vapply(wlist, msg, character(1)), warning, call. = FALSE)  # nocov
    }
    return(out)
  }

  # Check layer and query conflict
  if (!is.null(layer)) {
    if (!is.null(query)) {
      warning("You provided both `layer` and `query` arguments,",
              " will only use `layer`.", call. = FALSE)
    }
    # capture warnings from RPostgreSQL package
    if (inherits(dsn, "PostgreSQLConnection")) {
      tbl <- filter_warning(dbReadTable(dsn, layer), "unrecognized PostgreSQL field type geometry")
    } else {
      tbl <- dbReadTable(dsn, layer)
    }
  } else if(is.null(query)) {
    stop("Provide either a `layer` or a `query`", call. = FALSE)
  } else {
    # capture warnings from RPostgreSQL package
    if (inherits(dsn, "PostgreSQLConnection")) {
      filter_warning(tbl <- dbGetQuery(dsn, query), "unrecognized PostgreSQL field type geometry")
    } else {
      tbl <- dbGetQuery(dsn, query)
    }
  }

  if (is.null(tbl)) {
    stop("Query `", query, "` returned no results.", call. = FALSE)  #nocov
  }

  if (is.null(geometry_column)) {
    # scan table for simple features column
    geometry_column = sf:::is_geometry_column.default(dsn, tbl)
    tbl[geometry_column] <- lapply(tbl[geometry_column], sf:::try_postgis_as_sfc, EWKB = EWKB, conn = dsn)
  } else {
    if (!all(geometry_column %in% names(tbl))) {
      # prepare error message
      nm <- names(tbl)
      prefix <- ""
      new_line <- ""
      if(length(nm) > 1) {
        prefix <- "  *"
        new_line <- "\n"
      }
      stop("Could not find `geometry_column` (\"", paste(geometry_column, collapse = "\", \""), "\") ",
           "in column names. Available names are:",
           new_line,
           paste(prefix, nm, collapse = "\n", sep = " "),
           call. = FALSE)
    }
    tbl[geometry_column] <- lapply(tbl[geometry_column], postgis_as_sfc, EWKB = EWKB, conn = dsn)
  }

  # if there are no simple features geometries, return a data frame
  if (!any(vapply(tbl, inherits, logical(1), "sfc"))) {
    # try reading blob columns:
    blob_columns = vapply(tbl, inherits, logical(1), "blob")
    success = FALSE
    for (i in which(blob_columns)) {
      message(i)
      crs <- lapply(tbl[[i]], function(x) x[1:4])
      sfc <- lapply(tbl[[i]], function(x) x[-c(1:4)])

      try(sfc <- st_as_sfc(sfc), silent = TRUE)

      if (!inherits(sfc, "try-error")) {
        if (length(unique(crs)) < 1)
          stop(paste("More than one crs found in column:", i))
        crs = readBin(crs[[1]],
                      what = "int",
                      n = 1,
                      size = 4L)
        st_crs(sfc) = crs

        tbl[[i]] = sfc
        success = TRUE
      }
    }
    if (!success) {
      warning("Could not find a simple features geometry column. Will return a `data.frame`.")
      return(tbl)
    }
  }

  x <- st_sf(tbl, ...)

  if (!quiet) print(x, n = 0) # nocov

  if (as_tibble) {
    x <- tibble::new_tibble(x, nrow = nrow(x), class = "sf")
  }
  return(x)
})

# from: https://stackoverflow.com/questions/70884796/convert-hexadecimal-string-to-bytes-in-r
hex_to_raw <- function(x) {
  digits <- strtoi(strsplit(x, "")[[1]], base=16L)
  as.raw(bitwShiftL(digits[c(TRUE, FALSE)],4) + digits[c(FALSE, TRUE)])
}
