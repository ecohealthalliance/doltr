library(testthat)
library(doltr)

options(testthat.summary.max_reports = 10,
        testthat.progress.max_fails = 10)

test_check("doltr")
