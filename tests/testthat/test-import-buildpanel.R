context("build.panel compatibility")

skip_if_not_installed("SAScii")
skip_if_not_installed("zip")

test_that("getPSID output builds the same panel as the fixture datadir", {
  d <- fake_psid_datadir(c(1985, 1986))
  ind_year <- max(locapsidR:::makeids()$year)

  inputdir <- tempfile("input_")
  dir.create(inputdir)

  build <- tempfile("build_")
  dir.create(build)

  write_fixed_width(d$fam[["1985"]], build, "FAM1985ER")
  write_fixed_width(d$fam[["1986"]], build, "FAM1986ER")
  write_fixed_width(d$ind, build, paste0("IND", ind_year, "ER"))

  zip_files(file.path(build, c("FAM1985ER.txt", "FAM1985ER.sas")),
            file.path(inputdir, "FAM1985ER.zip"))
  zip_files(file.path(build, c("FAM1986ER.txt", "FAM1986ER.sas")),
            file.path(inputdir, "FAM1986ER.zip"))
  zip_files(file.path(build, c(paste0("IND", ind_year, "ER.txt"),
                               paste0("IND", ind_year, "ER.sas"))),
            file.path(inputdir, paste0("IND", ind_year, "ER.zip")))

  outputdir <- tempfile("output_")
  getPSID(inputdir, outputdir)

  fam.vars <- data.frame(year = c(1985, 1986), v = c("FAMVAR1", "FAMVAR1"))

  imported <- build.panel(datadir = outputdir, fam.vars = fam.vars,
                          design = "all", heads.only = FALSE, loglevel = WARN)
  original <- build.panel(datadir = d$dir, fam.vars = fam.vars,
                          design = "all", heads.only = FALSE, loglevel = WARN)

  data.table::setorder(imported, pid, year)
  data.table::setorder(original, pid, year)

  expect_equal(as.data.frame(imported), as.data.frame(original))
})

test_that("build.panel logs everything already downloaded at INFO", {
  d <- fake_psid_datadir(c(1985, 1986))

  log <- tempfile(fileext = ".log")
  old <- futile.logger::flog.appender()
  futile.logger::flog.appender(futile.logger::appender.file(log))
  on.exit(futile.logger::flog.appender(old), add = TRUE)

  fam.vars <- data.frame(year = c(1985, 1986), v = c("FAMVAR1", "FAMVAR1"))
  build.panel(datadir = d$dir, fam.vars = fam.vars, design = "all",
              heads.only = FALSE, loglevel = INFO)

  futile.logger::flog.appender(old)
  logged <- readLines(log)

  expect_true(any(grepl("everything already downloaded", logged,
                        fixed = TRUE)))
})
