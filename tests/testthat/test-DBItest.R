if (Sys.getenv("GITHUB_ACTIONS") == "true" ||
    (identical(Sys.getenv("NOT_CRAN"), "true"))) {

  options(testthat.progress.max_fails = Inf)

  DBItest::make_context(
    dolt_local(),
    tweaks = DBItest::tweaks(constructor_name = "dolt_local",
                             constructor_relax_args = TRUE,
                             placeholder_pattern = "?",
                             logical_return = function(x) as.integer(x),
                             list_temporary_tables = FALSE),
    default_skip = c(
      "package_name",
      "data_logical",                               # not an error: cannot cast to logical
      "data_raw",                                   # not an error: can't cast to blob type

      # bad tests
      "list_objects_features",
      "append_roundtrip_timestamp",
      "roundtrip_timestamp"
    )
  )
  DBItest::test_getting_started()
  DBItest::test_connection()
  DBItest::test_driver()
  #DBItest::test_result()
  DBItest::test_sql()
  DBItest::test_meta()
  DBItest::test_transaction()
  DBItest::test_compliance()
  DBItest::test_stress()
}
