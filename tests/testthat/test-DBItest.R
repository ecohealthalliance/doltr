if (Sys.getenv("GITHUB_ACTIONS") == "true" || (doltDefault() && identical(Sys.getenv("NOT_CRAN"), "true"))) {
  DBItest::make_context(dolt_local(), NULL)
  DBItest::test_all()
  DBItest::test_stress()
}
