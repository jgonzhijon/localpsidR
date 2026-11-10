context("missing files")

test_that("build.panel errors clearly when no files are present", {
  dir <- tempfile("empty_")
  dir.create(dir)

  fam.vars <- data.frame(year = c(1985, 1986), v = c("FAMVAR1", "FAMVAR1"))

  err <- tryCatch(
    build.panel(datadir = dir, fam.vars = fam.vars, loglevel = WARN),
    error = function(e) conditionMessage(e)
  )

  expect_match(err, "getPSID")
  expect_match(err, "FAM1985ER")
  expect_match(err, psidR:::psid_ind_name())
})

test_that("build.panel errors when only the index file is present", {
  d <- fake_psid_datadir(c(1985, 1986))
  fam_files <- list.files(d$dir, pattern = "^FAM", full.names = TRUE)
  file.remove(fam_files)

  fam.vars <- data.frame(year = c(1985, 1986), v = c("FAMVAR1", "FAMVAR1"))

  err <- tryCatch(
    build.panel(datadir = d$dir, fam.vars = fam.vars, loglevel = WARN),
    error = function(e) conditionMessage(e)
  )

  expect_match(err, "getPSID")
  expect_match(err, "FAM1985ER")
  expect_false(grepl("no individual index", err, fixed = TRUE))
})
