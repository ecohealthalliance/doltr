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

  # Split raw into crs and wkb columns
  sf_via_dbi <- sf_via_dbi |>
    mutate(crs = lapply(coord, function(x) x[1:4]),
           wkb = lapply(coord, function(x) x[-c(1:4)])) |>
    select(-coord)

  # Convert crs to int then use sf machinery to translate wkb
  sf_via_dbi <- sf_via_dbi |>
    rowwise() |>
    mutate(crs = readBin(crs, what = "int", n = 1, size = 4L)) |>
    st_as_sf()

  # Do all rows have the same crs? If not error out
  testthat::expect_equal(length(unique(sf_via_dbi$crs)), 1L)

  # Set the crs of the simple feature collection and
  # rename wkb column to geometry type
  sf_via_dbi |>
    st_set_crs(sf_via_dbi$crs[3]) |>
    select(-crs) |>
    rename(!!st_geometry_type(sf_via_dbi, by_geometry = F) |> as.character() := wkb )
}
