context("import API")

skip_if_not_installed("SAScii")
skip_if_not_installed("zip")

test_that("getPSID imports fake zips and reports created", {
  inputdir <- tempfile("input_")
  outputdir <- tempfile("output_")
  dir.create(inputdir)

  build <- tempfile("build_")
  dir.create(build)
  df <- data.frame(A = c(1L, 22L), B = c(10L, 20L))
  write_fixed_width(df, build, "FAM2021ER")
  write_fixed_width(df, build, "IND2023ER")
  zip_files(file.path(build, c("FAM2021ER.txt", "FAM2021ER.sas")),
            file.path(inputdir, "FAM2021ER.zip"))
  zip_files(file.path(build, c("IND2023ER.txt", "IND2023ER.sas")),
            file.path(inputdir, "IND2023ER.zip"))

  res <- getPSID(inputdir, outputdir)

  expect_s3_class(res, "data.frame")
  expect_identical(nrow(res), 2L)
  expect_identical(sort(res$status), c("created", "created"))
  expect_true(all(c("FAM2021ER.rda", "IND2023ER.rda") %in%
                    list.files(outputdir)))
})

test_that("a second call skips, and overwrite recreates", {
  inputdir <- tempfile("input_")
  outputdir <- tempfile("output_")
  dir.create(inputdir)

  build <- tempfile("build_")
  dir.create(build)
  df <- data.frame(A = c(1L, 22L), B = c(10L, 20L))
  write_fixed_width(df, build, "FAM2021ER")
  zip_files(file.path(build, c("FAM2021ER.txt", "FAM2021ER.sas")),
            file.path(inputdir, "FAM2021ER.zip"))

  first <- getFamilyPSID(inputdir, outputdir)
  expect_identical(first$status, "created")

  second <- getFamilyPSID(inputdir, outputdir)
  expect_identical(second$status, "skipped")

  third <- getFamilyPSID(inputdir, outputdir, overwrite = TRUE)
  expect_identical(third$status, "created")
})

test_that("getFamilyPSID and getIndPSID process only their prefix", {
  inputdir <- tempfile("input_")
  outputdir <- tempfile("output_")
  dir.create(inputdir)

  build <- tempfile("build_")
  dir.create(build)
  df <- data.frame(A = c(1L, 22L), B = c(10L, 20L))
  write_fixed_width(df, build, "FAM2021ER")
  write_fixed_width(df, build, "IND2023ER")
  zip_files(file.path(build, c("FAM2021ER.txt", "FAM2021ER.sas")),
            file.path(inputdir, "FAM2021ER.zip"))
  zip_files(file.path(build, c("IND2023ER.txt", "IND2023ER.sas")),
            file.path(inputdir, "IND2023ER.zip"))

  fam <- getFamilyPSID(inputdir, outputdir)
  expect_identical(nrow(fam), 1L)
  expect_true(grepl("FAM2021ER", fam$file, fixed = TRUE))

  ind <- getIndPSID(inputdir, outputdir)
  expect_identical(nrow(ind), 1L)
  expect_true(grepl("IND2023ER", ind$file, fixed = TRUE))
})

test_that("a broken zip reports an error and leaves no .part", {
  inputdir <- tempfile("input_")
  outputdir <- tempfile("output_")
  dir.create(inputdir)

  writeLines("not a real zip", file.path(inputdir, "FAM2021ER.zip"))

  res <- getFamilyPSID(inputdir, outputdir)

  expect_identical(nrow(res), 1L)
  expect_identical(res$status, "error")
  expect_false(file.exists(file.path(outputdir, "FAM2021ER.rda")))
  expect_false(file.exists(file.path(outputdir, "FAM2021ER.rda.part")))
})

test_that("an empty input folder gives a zero-row result", {
  inputdir <- tempfile("input_")
  outputdir <- tempfile("output_")
  dir.create(inputdir)

  res <- getPSID(inputdir, outputdir)

  expect_s3_class(res, "data.frame")
  expect_identical(nrow(res), 0L)
  expect_identical(names(res), c("file", "status", "message"))
})

test_that("a missing inputdir is an error", {
  inputdir <- tempfile("missing_")
  outputdir <- tempfile("output_")

  expect_error(getPSID(inputdir, outputdir), "inputdir")
})
