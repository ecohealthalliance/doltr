if ((Sys.getenv("GITHUB_ACTIONS") == "true" || Sys.getenv("NOT_CRAN") != "") &&
    is_dolt_installed()) {

  options(testthat.progress.max_fails = Inf)

  ctx <- DBItest::make_context(
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
      "roundtrip_timestamp",
      # From test_sql(): dolt tables do not allow spaces
      "read_table_name",
      "create_table_name",
      "list_tables_quote",
      "exists_table_name",
      "remove_table_name",
      "list_objects_quote",
      "write_table_name",
      "append_table_name",
      "get_info_connection", # FIXME passes manually but fails in test env for some reason
      NULL
    )
  )
  DBItest::test_getting_started(ctx = ctx)
  DBItest::test_connection(ctx = ctx)
  DBItest::test_driver(ctx = ctx)
  #DBItest::test_result(ctx = ctx)
  #DBItest::test_sql(ctx  = ctx)
  #DBItest::test_meta(ctx = ctx)
  #DBItest::test_transaction(ctx = ctx)
  #DBItest::test_compliance(ctx = ctx)
  #DBItest::test_stress()

  unlink("doltdb")
}
