context("individual index detection")

test_that("build.panel picks the newest of several index files", {
  n <- 20L
  d <- fake_psid_datadir(c(1985, 1986), n = n, ind_year = 2019)

  file.copy(
    file.path(d$dir, "IND2019ER.rda"),
    file.path(d$dir, "IND2021ER.rda")
  )

  fam.vars <- data.frame(year = c(1985, 1986), v = c("FAMVAR1", "FAMVAR1"))
  out <- build.panel(datadir = d$dir, fam.vars = fam.vars,
                     design = "all", heads.only = FALSE, loglevel = WARN)

  expect_equal(nrow(out), 2 * n)
})

test_that("build.panel accepts a non-latest index and logs everything downloaded", {
  n <- 20L
  d <- fake_psid_datadir(c(1985, 1986), n = n, ind_year = 2019)

  log <- tempfile(fileext = ".log")
  old <- futile.logger::flog.appender()
  futile.logger::flog.appender(futile.logger::appender.file(log))
  on.exit(futile.logger::flog.appender(old), add = TRUE)

  fam.vars <- data.frame(year = c(1985, 1986), v = c("FAMVAR1", "FAMVAR1"))
  out <- build.panel(datadir = d$dir, fam.vars = fam.vars,
                     design = "all", heads.only = FALSE, loglevel = INFO)

  futile.logger::flog.appender(old)
  logged <- readLines(log)

  expect_equal(nrow(out), 2 * n)
  expect_true(any(grepl("everything already downloaded", logged, fixed = TRUE)))
})
