# Run after all tests
dolt_test_dir = file.path(tempdir(), "dolt_test_db")

withr::defer({
  withr::local_envvar(c("DOLT_DIR" = dolt_test_dir))
  if (is_dolt_installed()) dbDisconnect(dolt())
  unlink(Sys.getenv("DOLT_DIR"), recursive = TRUE, force = TRUE)
  unlink(list.files(file.path(tempdir(), ".."), pattern = "^[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}$", full.names = TRUE))
}, teardown_env())
