context("waves build.panel")

test_that("build.panel rejects unsupported years", {
  expect_error(
    build.panel(datadir = tempdir(),
                fam.vars = data.frame(year = 2099, x = "A")),
    "does not support"
  )

  expect_error(
    build.panel(datadir = tempdir(),
                fam.vars = data.frame(year = 1998, x = "A")),
    "does not support"
  )
})

test_that("build.panel still builds the synthetic fixture panel", {
  n <- 20L
  d <- fake_psid_datadir(c(1985, 1986), n = n)

  out <- build.panel(
    datadir = d$dir,
    fam.vars = data.frame(year = c(1985, 1986), v = c("FAMVAR1", "FAMVAR1")),
    heads.only = FALSE,
    design = "all",
    loglevel = WARN
  )

  expect_equal(nrow(out), 2 * n)
})
