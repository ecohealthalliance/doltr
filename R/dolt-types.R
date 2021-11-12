
#' Dolt Data Types
#'
#' `dbDataType` matches R data types to dolt data types. For text and blob data
#' types, it automatically chooses amongst options (e.g., `VARCHAR(N), `TEXT`,
#' `LONGTEXT`, etc.`) based on maximum field length.  An attribute of maximum
#' size of these fields is returned to support operations where fields need to be recast in revision.
#'
#' `dolt_type_sizes()` takes a vector of SQL types and returns the maximum field
#' size, if applicable.
#' @param dbObj the database connection
#' @param obj the data type (vector or data frame)
#' @param min_varchar The minimum size `VARCHAR` types should be cast as
#' @param max_varchar the maximum size `VARCHAR` types should be cast as. Larger
#' text data will return types `TEXT`,`MEDIUMTEXT`, or `LONGTEXT`
#' @param ... further arguments to methods
#' @return A character vector of classes, with attributes of the maximum size for
#' text and blob classes
#' @export
#' @rdname dolt-types
setMethod("dbDataType", "DoltConnection", function(dbObj, obj,
                                                   min_varchar = Sys.getenv("DOLT_MINVARCAHR", 255L),
                                                   max_varchar = Sys.getenv("DOLT_MAXVARCHAR", 16383L),
                                                   ...) {
  min_varchar = as.integer(min_varchar)
  max_varchar = as.integer(max_varchar)
  if (is.data.frame(obj)) {
    typ_l <- lapply(obj, dolt_data_type,
                    min_varchar = min_varchar, max_varchar = max_varchar)
    typ <- unlist(typ_l)
    max_size <- numeric(length(obj))
    for(i in seq_along(max_size)) max_size[i] <- attr(typ_l[[i]], "max_size")
    if (is.null(typ)) typ <- character(0)
    attr(typ, "max_size") <- max_size
  } else {
    typ <- dolt_data_type(obj,  min_varchar, max_varchar)
  }

  typ
})

dolt_data_type <- function(obj, min_varchar, max_varchar) {
  if (is.factor(obj)) return(dolt_text_type(levels(obj), min_varchar, max_varchar))

  if (inherits(obj, "POSIXct"))   return(structure("DATETIME", max_size = NA_real_))
  if (inherits(obj, "Date"))      return(structure("DATE", max_size = NA_real_))
  if (inherits(obj, "difftime"))  return(structure("TIME", max_size = NA_real_))
  if (inherits(obj, "integer64")) return(structure("BIGINT", max_size = NA_real_))
  if (inherits(obj, "blob")) dolt_blob_type(obj)


  switch(typeof(obj),
         logical = structure("TINYINT", max_size = NA_real_), # works better than BIT(1), https://stackoverflow.com/q/289727/946850
         integer = structure("INT", max_size = NA_real_),
         double =  structure("DOUBLE", max_size = NA_real_),
         character = dolt_text_type(obj, min_varchar, max_varchar),
         list = dolt_blob_type(obj),
         stop("Unsupported type", call. = FALSE)
  )
}

dolt_text_type <- function(obj, min_varchar, max_varchar) {
  nc <- max(nchar(enc2utf8(obj), type = "bytes"), 1, na.rm = TRUE)
  if (nc <= min_varchar) {
    return(structure(paste0("VARCHAR(", min_varchar, ")"), max_size = min_varchar))
  } else if (nc > min_varchar & nc <= 16383 & nc < max_varchar) {
    sz <- 2^(floor(log2(nc)) + 1) - 1
    return(structure(paste0("VARCHAR(", sz, ")"), max_size = sz))
  } else if (nc > max_varchar && nc <= 16383) {
    return(structure("TEXT", max_size = 16383))
  } else if ((nc > 16383 || nc > max_varchar) && nc <= 4194303) {
    return(structure("MEDIUMTEXT", max_size = 4194303))
  } else if (nc > 4194303 && nc <= 4294967295) {
    return(structure("LONGTEXT", max_size = 4294967295))
  } else {
    stop("Text data is greater than 4GB! No storage type fits.")
  }
}

#' @importFrom blob as_blob
dolt_blob_type <- function(obj) {
  if (!all(vapply(obj, is.raw, logical(1)))) "Stop only lists of raw vectors (blobs) allowed"
  nb <- max(vapply(obj, \(x) length(x), 1), 1, na.rm = TRUE)
  if (nb <=  65535) {
    return(structure("BLOB", max_size = 65535))
  } else if (nb > 65535L && nb <= 16777215) {
    return(structure("MEDIUMBLOB", max_size = 16777215))
  } else if (nb > 16777215 && nb <= 4294967295) {
    return(structure("LONGBLOB", max_size = 4294967295))
  } else if (nb > 4294967295) {
    stop("Blob data is greater than 4GGB! No storage type fits")
  }
}

#' @export
#' @importFrom dplyr case_when
#' @param types a character vector of dolt types, e.g., `"VARCHAR(12)"`,
#'   `"LONGBLOB"`, `"TINYINT"`, etc.
#' @rdname dolt-types
dolt_type_sizes <- function(types) {
  case_when(
    grepl("^VARCHAR", types, ignore.case = TRUE) ~ as.numeric(regextract(types, "\\d+")),
    gripl("TEXT", types) ~ 16383,
    gripl("MEDIUMTEXT", types) ~ 4194303,
    gripl("LONGTEXT", types) ~ 4294967295,
    gripl("BLOB", types) ~ 65535,
    gripl("MEDIUMBLOB", types) ~ 16777215,
    gripl("LONGBLOB", types) ~ 4294967295,
    TRUE ~ NA_real_)
}
