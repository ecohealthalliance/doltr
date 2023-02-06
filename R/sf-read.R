# Proposed mySQL sf_read method
#' Title
#'
#' @param conn
#' @param table_name
#'
#' @return
#' @export
#'
#' @examples
sf_read <- function(conn, table_name = "us_state_capitals_NCL") {

  # Read raw table
  sf_via_dbi <- DBI::dbReadTable(conn, table_name)

  # Identify geometry column. Sadly DBI::dbReadTable only returns 'char' for wkb columns
  # This still needs to be resolved. For now in our tests it's the 3rd column
  names(sf_via_dbi)[3] <- "coord"
  geom_col <- names(sf_via_dbi)[3]

  # This only works for text. lapply is uglier but works
  # Split raw into crs and wkb columns
  # sf_via_dbi <- sf_via_dbi |> rowwise() |>
  #   separate(all_of(geom_col), sep = 4, c("crs", "wkb"))

  # This works for raw
  sf_via_dbi <- sf_via_dbi |>
    mutate(crs = lapply(coord, function(x) x[1:4]),
           wkb = lapply(coord, function(x) x[-c(1:4)])) |>
    select(-coord)

  # This works for raw
  sf_via_dbi <- sf_via_dbi |>
    rowwise() |>
    mutate(crs = readBin(crs, what = "int", n = 1, size = 4L)) |>
    st_as_sf()

  # Do all rows have the same crs? If not error out
  testthat::expect_equal(length(unique(sf_via_dbi$crs)), 1L)

  # Set the crs of the simple feature collection and
  # rename wkb column to geometry type
  # Add in error protection for unkown crs
  sf_via_dbi <- possibly(st_set_crs, sf_via_dbi)(sf_via_dbi$crs[1]) |>
  select(-crs) |>
  rename(!!geom_col := wkb)

  sf_via_dbi
}


# NCL:I don't think we'll go this route yet. dbx is a better target to patch
#' Proposed mySQL sf_write method.
#'
#' @param conn
#' @param sfc
#' @param table_name
#'
#' @return
#' @export
#'
#' @examples
sf_write <- function(conn, sfc, table_name) {

  # Strip out crs
  crs <- unlist(str_split(st_crs(sfc)$input, ":"))[2] |> as.numeric()
  st_crs(sfc) = NA

  # Convert crs to hex taking care of NA handling
  crs <- ifelse(is.na(crs), "0000", R.utils::intToHex(crs))

  # Identify geometry column
  # sf_col <- names(sfc)[which(map_lgl(map(sfc, class), ~any("sfc" %in% .x)))]
  sf_col <- "Point"

  # Get wkb hex string and append crs
  # This might need some work if there are multiple sf columns?
  # sf_col is just the FIRST one.
  sfc <- sf:::to_postgis(conn, sfc, binary = T) |>
    mutate(across(any_of(sf_col), ~paste0(crs, .x)))

  sfc

}

# from: https://stackoverflow.com/questions/70884796/convert-hexadecimal-string-to-bytes-in-r
hex_to_raw <- function(x) {
  digits <- strtoi(strsplit(x, "")[[1]], base=16L)
  as.raw(bitwShiftL(digits[c(TRUE, FALSE)],4) + digits[c(FALSE, TRUE)])
}
