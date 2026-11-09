context("NA family variables")

test_that("NA fam.vars do not trigger a with=FALSE warning or drop rows", {
  n <- 20L
  d <- fake_psid_datadir(c(1985, 1986), n = n)

  fam.vars <- data.frame(
    year = c(1985, 1986),
    money = c("FAMVAR1", NA),
    age = c("FAMVAR1", "FAMVAR1")
  )

  warnings <- character()
  out <- withCallingHandlers(
    build.panel(
      datadir = d$dir,
      fam.vars = fam.vars,
      heads.only = FALSE,
      design = "all",
      loglevel = WARN
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_false(any(grepl("with=FALSE", warnings, fixed = TRUE)))

  expect_equal(nrow(out), 2 * n)

  # money is all NA in 1986 and has no NA in 1985
  expect_true(all(is.na(out[year == 1986, money])))
  expect_true(all(!is.na(out[year == 1985, money])))

  # the column is numeric and the 1986 rows are present
  expect_true(is.numeric(out$money))
  expect_equal(out[year == 1986, .N], n)
  expect_equal(out[year == 1985, .N], n)
})
