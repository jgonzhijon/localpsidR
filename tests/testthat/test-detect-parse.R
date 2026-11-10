context("SAS label parsing")

test_that("the three SAS label syntaxes are parsed with spaces preserved", {
  dir <- tempfile("sas_")
  dir.create(dir)
  p <- file.path(dir, "labels.sas")

  writeLines(c(
    '      ERT0001    LABEL="1968 INTERVIEW NUMBER"                       format=f4.',
    '      ERT0002    LABEL="SEQUENCE NUMBER                       69"    format=f2.',
    '      ERT0003    LABEL="RELATION TO REFERENCE PERSON          23"    format=f2.'
  ), p)

  res <- locapsidR:::psid_parse_sas_labels(p)

  expect_identical(res$var, c("ERT0001", "ERT0002", "ERT0003"))
  expect_identical(res$label,
                   c("1968 INTERVIEW NUMBER",
                     "SEQUENCE NUMBER                       69",
                     "RELATION TO REFERENCE PERSON          23"))
})

test_that("lower-case label= is accepted and non-LABEL or comment lines are ignored", {
  dir <- tempfile("sas_")
  dir.create(dir)
  p <- file.path(dir, "mixed.sas")

  writeLines(c(
    '  ERT0001 LABEL="AAA"   format=f1.',
    '  ERT0002 label="BBB"   format=f1.',
    '  ERT0003 FORMAT="CCC"  format=f1.',
    '* ERT0009 LABEL="DDD"   format=f1.',
    '/* ERT0010 LABEL="EEE"  format=f1. */',
    '  ERT0004 LABEL="FFF"   format=f1.'
  ), p)

  res <- locapsidR:::psid_parse_sas_labels(p)

  expect_identical(res$var, c("ERT0001", "ERT0002", "ERT0004"))
  expect_identical(res$label, c("AAA", "BBB", "FFF"))
})

test_that("a 0xA0 byte in a label and a NUL byte elsewhere are cleaned", {
  dir <- tempfile("sas_")
  dir.create(dir)
  p <- file.path(dir, "bytes.sas")

  bytes <- c(
    charToRaw('  ERT0001    LABEL="FOO'),
    as.raw(0xA0),
    charToRaw('BAR"   format=f1.\n'),
    as.raw(0x00),
    charToRaw('  ERT0002    LABEL="BAZ"   format=f1.\n')
  )
  writeBin(bytes, p)

  res <- locapsidR:::psid_parse_sas_labels(p)

  expect_identical(res$var, c("ERT0001", "ERT0002"))
  expect_identical(res$label, c("FOO BAR", "BAZ"))
})

test_that("a file with no label lines returns zero rows with the right columns", {
  dir <- tempfile("sas_")
  dir.create(dir)
  p <- file.path(dir, "nolabel.sas")

  writeLines(c("  ERT0001 FORMAT=f1.", "  ERT0002 VALUE=1."), p)

  res <- locapsidR:::psid_parse_sas_labels(p)

  expect_identical(nrow(res), 0L)
  expect_identical(names(res), c("var", "label"))
})

test_that("a non-existent path errors", {
  expect_error(locapsidR:::psid_parse_sas_labels(file.path(tempfile("no_"), "nope.sas")),
               "does not exist")
})

test_that("2500 label lines are all returned in order and the file is unchanged", {
  dir <- tempfile("sas_")
  dir.create(dir)
  p <- file.path(dir, "many.sas")

  lines <- sprintf("  ERT%04d    LABEL=\"LABEL %04d\"   format=f1.", 1:2500, 1:2500)
  writeLines(lines, p)

  before <- tools::md5sum(p)
  res <- locapsidR:::psid_parse_sas_labels(p)
  after <- tools::md5sum(p)

  expect_identical(nrow(res), 2500L)
  expect_identical(res$var, sprintf("ERT%04d", 1:2500))
  expect_identical(res$label, sprintf("LABEL %04d", 1:2500))
  expect_identical(before, after)
})
