context("job discovery")

test_that("psid_find_jobs finds jobs by prefix and number", {
  d <- tempfile("jobs_")
  dir.create(d)
  dir.create(file.path(d, "FAM2023ER"))
  file.create(file.path(d, "FAM2021ER.zip"))
  file.create(file.path(d, "IND2023ER.txt"))
  file.create(file.path(d, "IND2023ER.sas"))
  file.create(file.path(d, "README.txt"))

  fam <- locapsidR:::psid_find_jobs(d, "fam")
  expect_length(fam, 2)
  expect_identical(vapply(fam, function(j) j$num, character(1)),
                   c("2021", "2023"))

  ind <- locapsidR:::psid_find_jobs(d, "ind")
  expect_length(ind, 1)
  expect_identical(ind[[1]]$num, "2023")
  expect_length(ind[[1]]$files, 2)
})

test_that("duplicate kinds collapse to one job with a warning", {
  d <- tempfile("jobs_")
  dir.create(d)
  dir.create(file.path(d, "FAM2021ER"))
  file.create(file.path(d, "FAM2021ER.zip"))

  log <- tempfile(fileext = ".log")
  old <- futile.logger::flog.appender()
  futile.logger::flog.appender(futile.logger::appender.file(log))
  on.exit(futile.logger::flog.appender(old), add = TRUE)

  jobs <- locapsidR:::psid_find_jobs(d, "fam")

  futile.logger::flog.appender(old)
  logged <- readLines(log)

  expect_length(jobs, 1)
  expect_identical(jobs[[1]]$num, "2021")
  expect_identical(basename(jobs[[1]]$files), "FAM2021ER.zip")
  expect_true(any(grepl("multiple job kinds", logged, fixed = TRUE)))
})

test_that("psid_already_exists matches only the exact full base name", {
  d <- tempfile("out_")
  dir.create(d)
  file.create(file.path(d, "FAM2021ER.rda"))
  file.create(file.path(d, "fam2021er.RData"))

  expect_true(locapsidR:::psid_already_exists(d, "FAM2021ER", "rda"))
  expect_true(locapsidR:::psid_already_exists(d, "fam2021er", "rda"))

  for (f in c("FAM2021ER_old.rda", "oldFAM2021ER.rda",
              "FAM20210ER.rda", "FAM2021ER.rds")) {
    d2 <- tempfile("out_")
    dir.create(d2)
    file.create(file.path(d2, f))
    expect_false(locapsidR:::psid_already_exists(d2, "FAM2021ER", "rda"))
  }
})

test_that("psid_already_exists handles rds and missing directories", {
  d <- tempfile("out_")
  dir.create(d)
  file.create(file.path(d, "FAM2021ER.rds"))

  expect_true(locapsidR:::psid_already_exists(d, "FAM2021ER", "rds"))
  expect_false(locapsidR:::psid_already_exists(d, "FAM2021ER", "rda"))

  missing <- file.path(tempdir(), "does-not-exist-xyz")
  expect_false(locapsidR:::psid_already_exists(missing, "FAM2021ER", "rda"))
})

test_that("psid_target_name builds the uppercase target name", {
  expect_identical(locapsidR:::psid_target_name("fam", "2021", "ER"), "FAM2021ER")
  expect_identical(locapsidR:::psid_target_name("ind", "2023", "ER"), "IND2023ER")
})
