context("SAS import helpers")

test_that("clean_sas replaces high and NUL bytes and leaves input untouched", {
  p <- tempfile(fileext = ".sas")
  q <- tempfile(fileext = ".sas")

  raw <- c(
    charToRaw("DATA x;\n"),
    as.raw(0xA0),
    as.raw(0x00),
    charToRaw("\nINPUT a 1-2;\n")
  )
  writeBin(raw, p)
  before <- tools::md5sum(p)

  localpsidR:::clean_sas(p, out = q)

  out <- readBin(q, what = "raw", n = file.size(q))
  expect_false(any(as.integer(out) > 127))
  expect_false(any(out == as.raw(0x00)))
  expect_identical(tools::md5sum(p), before)
})

test_that("has_input_block detects the INPUT block", {
  p <- tempfile(fileext = ".sas")
  writeLines(c("DATA x;", "INFILE 'd.txt';", "INPUT a 1-2;"), p)
  expect_true(localpsidR:::has_input_block(p))
})

test_that("has_input_block returns FALSE without an INPUT block", {
  p <- tempfile(fileext = ".sas")
  writeLines(c("DATA x;", "INFILE 'd.txt';", "LABEL a='a';"), p)
  expect_false(localpsidR:::has_input_block(p))
})

test_that("has_input_block is case-insensitive with a high byte present", {
  p <- tempfile(fileext = ".sas")
  raw <- c(
    charToRaw("DATA x;\n"),
    as.raw(0xA0),
    charToRaw("\ninput a 1-2;\n")
  )
  writeBin(raw, p)
  expect_true(localpsidR:::has_input_block(p))
})
