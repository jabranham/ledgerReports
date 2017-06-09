library(ledgerReports)
context("Reading data")


test_that("Returns data.frame", {
  ledger_demo_file <- read_ledger("'^assets' '^liab'",
                                 system.file("extdata",
                                             "demo.ledger",
                                             package="ledgerReports"))
  ledger_real_file <- read_ledger("'^assets' '^liab'",
                                 Sys.getenv("LEDGER_FILE"))
  expect_s3_class(ledger_demo_file, "data.frame")
  expect_s3_class(ledger_real_file, "data.frame")
})

test_that("date is a date", {
  ledger_demo_file <- read_ledger("'^assets' '^liab'",
                                 system.file("extdata",
                                             "demo.ledger",
                                             package="ledgerReports"))
  ledger_real_file <- read_ledger("'^assets' '^liab'",
                                 Sys.getenv("LEDGER_FILE"))
 expect_s3_class(ledger_demo_file[, 1], "POSIXct")
 expect_s3_class(ledger_real_file[, 1], "POSIXct")
})

test_that("amount is a number", {
  ledger_demo_file <- read_ledger("'^assets' '^liab'",
                                 system.file("extdata",
                                             "demo.ledger",
                                             package="ledgerReports"))
  ledger_real_file <- read_ledger("'^assets' '^liab'",
                                 Sys.getenv("LEDGER_FILE"))
  expect_is(ledger_demo_file[, "amount"], "numeric")
  expect_is(ledger_real_file[, "amount"], "numeric")
})
