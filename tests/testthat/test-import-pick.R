context("pick_pair")

test_that("matching base names beat README and format files", {
  d <- tempfile("pick_")
  dir.create(d)
  writeLines("data", file.path(d, "A.txt"))
  writeLines(c("DATA x;", "INPUT a 1-2;"), file.path(d, "A.sas"))
  writeLines("read me", file.path(d, "README.txt"))
  writeLines(c("DATA x;", "INPUT a 1-2;"), file.path(d, "A_formats.sas"))

  files <- list.files(d, recursive = TRUE, full.names = TRUE)
  res <- localpsidR:::pick_pair(files)

  expect_identical(basename(res$txt), "A.txt")
  expect_identical(basename(res$sas), "A.sas")
})

test_that("non-matching names pick the larger .txt with a warning", {
  d <- tempfile("pick_")
  dir.create(d)
  writeLines("small", file.path(d, "B.txt"))
  writeLines(strrep("x", 200), file.path(d, "C.txt"))
  writeLines(c("DATA x;", "INPUT a 1-2;"), file.path(d, "D.sas"))

  files <- list.files(d, recursive = TRUE, full.names = TRUE)

  log <- tempfile(fileext = ".log")
  old <- futile.logger::flog.appender()
  futile.logger::flog.appender(futile.logger::appender.file(log))
  on.exit(futile.logger::flog.appender(old), add = TRUE)

  res <- localpsidR:::pick_pair(files)

  futile.logger::flog.appender(old)
  logged <- readLines(log)

  expect_true(any(grepl("largest .txt", logged, fixed = TRUE)))
  expect_identical(basename(res$txt), "C.txt")
  expect_identical(basename(res$sas), "D.sas")
})

test_that("a .sas without an INPUT block is ignored", {
  d <- tempfile("pick_")
  dir.create(d)
  writeLines("data a", file.path(d, "A.txt"))
  writeLines("data b", file.path(d, "B.txt"))
  writeLines("DATA x;", file.path(d, "A.sas"))
  writeLines(c("DATA x;", "INPUT a 1-2;"), file.path(d, "B.sas"))

  files <- list.files(d, recursive = TRUE, full.names = TRUE)
  res <- localpsidR:::pick_pair(files)

  expect_identical(basename(res$txt), "B.txt")
  expect_identical(basename(res$sas), "B.sas")
})

test_that("__MACOSX paths and dot-prefixed base names are ignored", {
  d <- tempfile("pick_")
  dir.create(d)
  dir.create(file.path(d, "__MACOSX"))
  writeLines("data", file.path(d, "A.txt"))
  writeLines(c("DATA x;", "INPUT a 1-2;"), file.path(d, "A.sas"))
  writeLines("junk", file.path(d, "__MACOSX", "._A.txt"))
  writeLines("junk", file.path(d, ".hidden.txt"))

  files <- c(
    file.path(d, "A.txt"),
    file.path(d, "A.sas"),
    file.path(d, "__MACOSX", "._A.txt"),
    file.path(d, ".hidden.txt")
  )

  res <- localpsidR:::pick_pair(files)

  expect_identical(basename(res$txt), "A.txt")
  expect_identical(basename(res$sas), "A.sas")
})

test_that("no valid pair errors and lists the file names", {
  d <- tempfile("pick_")
  dir.create(d)
  writeLines("data", file.path(d, "A.txt"))

  files <- list.files(d, recursive = TRUE, full.names = TRUE)

  expect_error(localpsidR:::pick_pair(files), "A.txt")
})
