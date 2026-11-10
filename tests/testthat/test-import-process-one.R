context("process_one")

skip_if_not_installed("SAScii")
skip_if_not_installed("zip")

test_that("zip input produces an .rda with a single object named x", {
  d <- tempfile("fixture_")
  dir.create(d)
  df <- data.frame(A = c(1L, 22L, 333L), B = c(10L, 20L, 30L))
  write_fixed_width(df, d, "A")

  z <- tempfile(fileext = ".zip")
  zip_files(list.files(d, full.names = TRUE), z)

  out <- tempfile(fileext = ".rda")
  res <- localpsidR:::process_one(z, out)

  expect_identical(res, out)

  env <- new.env()
  load(out, envir = env)
  expect_identical(ls(env), "x")
  expect_true(is.data.frame(env$x))
  expect_identical(names(env$x), c("A", "B"))
  expect_equal(env$x$A, c(1, 22, 333))
  expect_equal(env$x$B, c(10, 20, 30))
})

test_that("rds format works with loose files", {
  d <- tempfile("fixture_")
  dir.create(d)
  df <- data.frame(A = c(1L, 22L, 333L), B = c(10L, 20L, 30L))
  write_fixed_width(df, d, "A")

  out <- tempfile(fileext = ".rds")
  localpsidR:::process_one(list.files(d, full.names = TRUE), out, format = "rds")

  x <- readRDS(out)
  expect_true(is.data.frame(x))
  expect_identical(names(x), c("A", "B"))
  expect_equal(x$A, c(1, 22, 333))
  expect_equal(x$B, c(10, 20, 30))
})

test_that("directory input works and leaves the .sas unchanged", {
  d <- tempfile("fixture_")
  dir.create(d)
  df <- data.frame(A = c(1L, 22L, 333L), B = c(10L, 20L, 30L))
  paths <- write_fixed_width(df, d, "A")
  sas_md5 <- tools::md5sum(paths[["sas"]])

  out <- tempfile(fileext = ".rda")
  localpsidR:::process_one(d, out)

  expect_identical(tools::md5sum(paths[["sas"]]), sas_md5)

  env <- new.env()
  load(out, envir = env)
  expect_identical(ls(env), "x")
  expect_equal(env$x$A, c(1, 22, 333))
  expect_equal(env$x$B, c(10, 20, 30))
})

test_that("a zip without a .sas errors and leaves no output behind", {
  d <- tempfile("fixture_")
  dir.create(d)
  writeLines("data", file.path(d, "A.txt"))

  z <- tempfile(fileext = ".zip")
  zip_files(file.path(d, "A.txt"), z)

  out <- tempfile(fileext = ".rda")
  expect_error(localpsidR:::process_one(z, out))

  expect_false(file.exists(out))
  expect_false(file.exists(paste0(out, ".part")))
})

test_that("the extraction directory is removed after success and error", {
  psid_dirs <- function() {
    list.files(tempdir(), pattern = "^psid_", full.names = TRUE)
  }

  d <- tempfile("fixture_")
  dir.create(d)
  df <- data.frame(A = c(1L, 22L, 333L), B = c(10L, 20L, 30L))
  write_fixed_width(df, d, "A")
  z <- tempfile(fileext = ".zip")
  zip_files(list.files(d, full.names = TRUE), z)

  out <- tempfile(fileext = ".rda")

  before <- psid_dirs()
  localpsidR:::process_one(z, out)
  expect_setequal(psid_dirs(), before)

  bad <- tempfile("fixture_bad_")
  dir.create(bad)
  writeLines("data", file.path(bad, "B.txt"))
  z2 <- tempfile(fileext = ".zip")
  zip_files(file.path(bad, "B.txt"), z2)

  out2 <- tempfile(fileext = ".rda")
  before <- psid_dirs()
  expect_error(localpsidR:::process_one(z2, out2))
  expect_setequal(psid_dirs(), before)
})

test_that("a zip with a path traversal entry is rejected", {
  d <- tempfile("evil_")
  dir.create(file.path(d, "sub"), recursive = TRUE)
  writeLines("boom", file.path(d, "evil.txt"))

  z <- tempfile(fileext = ".zip")
  old <- setwd(file.path(d, "sub"))
  on.exit(setwd(old), add = TRUE)
  suppressWarnings(zip::zip(z, "../evil.txt", root = ".", mode = "mirror"))

  outside <- file.path(tempdir(), "evil.txt")
  unlink(outside)

  out <- tempfile(fileext = ".rda")
  err <- tryCatch(localpsidR:::process_one(z, out), error = function(e) e)

  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "../evil.txt", fixed = TRUE)
  expect_false(file.exists(outside))
})
