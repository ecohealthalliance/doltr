# Load local copy of dolt
# devtools::install("/Users/nathanlayman/Documents/Academia/EHA/Projects/doltr")

library(doltr)
library(DBI)
library(sf)
conn <- dolt()

dbExecute(conn, "create table us_state_capitals_NCL (state varchar(50) primary key, city varchar(50), coord point);")
dbExecute(conn, "insert into us_state_capitals_NCL values ('alabama', 'montgomery', ST_SRID(point(32.361667, -86.279167), 4326));")
dbExecute(conn, "insert into us_state_capitals_NCL values ('alaska', 'juneau', st_geomfromtext('POINT(58.3 -134.416)'));")
dbExecute(conn, "insert into us_state_capitals_NCL values ('washington', 'tonasket', ST_SRID(point(32.361667, -86.279167), 4326));")


# MYSQL query to set point with specified
#ST_PointFromText(wkt[, srid [, options]])

# These work, if you look into st_read.DBIObject, you see it just tries all the
#`blob` type columns to see if st_as_sfc.blob(), works, which converts from EEKB
sf_via_dbi <- DBI::dbReadTable(conn, "us_state_capitals_NCL")
geom <- st_read(conn, "us_state_capitals_NCL")
identical(geom, sf_via_dbi |> st_as_sf())

# No CRS is set on st_read. Probably because it wasn't set during the insert queries above
st_crs(geom)
geom <- geom |> st_set_crs(4326)

# What does the select statement look like?
sf:::to_postgis(conn, geom, binary = T)

#TODO: Figure out if CRS is stored and retrieved correctly

# These fail
st_write(sf_via_sf, conn, "capitals_2")
st_write(sf_via_sf, conn, "capitals_2", driver = "mysql")

# Try monkeypatch
to_postgis <- function(conn, x, binary) {
  geom_col <- vapply(x, inherits, TRUE, what = "sfc")
  x[geom_col] <- lapply(x[geom_col], sync_crs, conn = conn)
  if (binary) {
    x[geom_col] <- lapply(x[geom_col], db_binary)
  } else {
    x[geom_col] <- lapply(x[geom_col], st_as_text, EWKT = TRUE)
  }
  x <- as.data.frame(x)
  clean_columns(x, factorsAsCharacter = TRUE)
}

# This part works.
sync_crs <- function(conn, geom) {
  # Pull the crs
  crs <- st_crs(geom)

  # Convert to 4 digit EPSG code
  srid <- sf:::epsg(crs)
  if (is.na(crs) || is.na(srid)) {
    if (is.na(st_as_text(crs)))
      crs <- st_crs(NA)
    else {
      crs <- get_possibly_new_srid(conn, crs)
    }
  }
  st_set_crs(geom, crs)
}

clean_columns = function(obj, factorsAsCharacter) {
  permitted = c("character", "integer", "numeric", "Date", "POSIXct", "logical", "list")
  for (i in seq_along(obj)) {
    if (is.factor(obj[[i]])) {
      obj[[i]] = if (factorsAsCharacter)
        as.character(obj[[i]])
      else
        as.numeric(obj[[i]])
    }
    if (! inherits(obj[[i]], permitted)) {
      if (inherits(obj[[i]], "POSIXlt"))
        obj[[i]] = as.POSIXct(obj[[i]])
      else if (is.numeric(obj[[i]]))
        obj[[i]] = as.numeric(obj[[i]]) # strips class
    }
    if (is.character(obj[[i]]))
      obj[[i]] = enc2utf8(obj[[i]])
  }
  ccls.ok = vapply(obj, function(x) inherits(x, permitted), TRUE)
  if (any(!ccls.ok)) {
    # nocov start
    nms <- names(obj)[!ccls.ok]
    cls <- sapply(obj, function(x) paste(class(x), collapse=";"))[!ccls.ok]
    warning("Dropping column(s) ", paste(nms, collapse=","),
            " of class(es) ", paste(cls, collapse=","))
    obj = obj[ccls.ok]
    # nocov end
  }
  colclasses = vapply(obj, function(x) permitted[ which(inherits(x, permitted, which = TRUE) > 0)[1] ] , "")
  # check that list columns contain raw vectors:
  for (lc in which(colclasses == "list")) {
    if (!all(sapply(obj[[lc]], inherits, "raw")))
      stop("list columns are only allowed with raw vector contents")
  }
  structure(obj, colclasses = colclasses)
}

# Version of st_as_binary that allows locally invalid srids
db_binary <- function(x) {
  st_as_binary(x, EWKB = TRUE, hex = TRUE, pureR = FALSE, srid = epsg(st_crs(x)))
}


(sf_via_dbi |> st_as_sf())$coord

rc <- rawConnection(sf_via_dbi$coord[[3]], "r")
readBin(rc, what = "int", n = 1, size = 4L, endian = "little") # 4 bytes for CRS
readBin(rc, what = "int", n = 1, size = 1L, endian = "little") # 1 byte-order indicator. 1 = little endian
readBin(rc, what = "int", n = 1, size = 4L, endian = "little") # 4 bytes for integer type information (1 = Point)
readBin(rc, what = "double", n = 2, size = 8L, endian = "little") # Points
