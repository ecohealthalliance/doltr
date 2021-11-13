# Run before any test
dolt()
dbDisconnect(dolt())
# Run after all tests
withr::defer({
 dbDisconnect(dolt())
 unlink("doltdb", recursive = TRUE)
}, teardown_env())
