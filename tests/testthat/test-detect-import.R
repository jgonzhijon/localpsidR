context("detect import")

skip_if_not_installed("SAScii")
skip_if_not_installed("zip")

# Synthetic IND triple labels for the given years.
ind_labels <- function(years) {
  out <- list()
  k <- 0L
  for (y in years) {
    yy <- y %% 100L
    out[[length(out) + 1L]] <- data.frame(
      var = sprintf("ERT%05d", k + 1:3),
      label = c(
        sprintf("%d INTERVIEW NUMBER", y),
        sprintf("SEQUENCE NUMBER %02d", yy),
        sprintf("RELATION TO REFERENCE PERSON %02d", yy)
      ),
      stringsAsFactors = FALSE
    )
    k <- k + 3L
  }
  do.call(rbind, out)
}

# A one-row data.frame whose columns are the label variable names.
df_for_labels <- function(labels) {
  as.data.frame(setNames(rep(list(1L), nrow(labels)), labels$var))
}

# Write a fixed-width fixture with labels and pack it into a zip.
make_zip <- function(inputdir, build, stem, labels) {
  write_fixed_width(df_for_labels(labels), build, stem, labels = labels)
  zip_files(file.path(build, c(paste0(stem, ".txt"), paste0(stem, ".sas"))),
            file.path(inputdir, paste0(stem, ".zip")))
}

test_that("IND then FAM detection builds and updates the CSV", {
  inputdir <- tempfile("input_")
  dir.create(inputdir)
  outputdir <- tempfile("output_")
  build <- tempfile("build_")
  dir.create(build)
  latest <- psidR:::psid_latest_wave()

  labels <- ind_labels(c(2063, 2065, latest))
  make_zip(inputdir, build, "IND2065ER", labels)

  captured <- capture_flog({
    processPSID(inputdir, outputdir, "ind")
  })
  expect_identical(captured$value$status, "created")

  csv <- file.path(outputdir, "psid_waves_detected.csv")
  expect_true(file.exists(csv))
  got <- data.table::fread(csv, na.strings = "", data.table = FALSE)
  expect_identical(got$year, c(2063L, 2065L))
  expect_false(any(got$year == latest))
  expect_true(all(!is.na(got$ind.interview) & !is.na(got$ind.seq) &
                    !is.na(got$ind.head)))
  expect_true(all(is.na(got$fam.interview)))

  fam_labels <- data.frame(var = "ERT00001",
                           label = "2065 FAMILY INTERVIEW (ID) NUMBER",
                           stringsAsFactors = FALSE)
  make_zip(inputdir, build, "FAM2065ER", fam_labels)

  captured2 <- capture_flog({
    processPSID(inputdir, outputdir, "fam")
  })
  expect_identical(captured2$value$status, "created")

  got2 <- data.table::fread(csv, na.strings = "", data.table = FALSE)
  expect_identical(got2$year, c(2063L, 2065L))
  row2065 <- got2[got2$year == 2065L, ]
  expect_false(is.na(row2065$fam.interview))
  expect_identical(row2065$ind.interview, got[got$year == 2065L, ]$ind.interview)
  expect_identical(row2065$ind.seq, got[got$year == 2065L, ]$ind.seq)
  expect_identical(row2065$ind.head, got[got$year == 2065L, ]$ind.head)
  expect_true(is.na(got2[got2$year == 2063L, ]$fam.interview))

  capture_flog({
    processPSID(inputdir, outputdir, "ind", overwrite = TRUE)
    processPSID(inputdir, outputdir, "fam", overwrite = TRUE)
  })
  got3 <- data.table::fread(csv, na.strings = "", data.table = FALSE)
  expect_identical(got3, got2)
})

test_that("labels for built-in years only do not create a CSV", {
  inputdir <- tempfile("input_")
  dir.create(inputdir)
  outputdir <- tempfile("output_")
  build <- tempfile("build_")
  dir.create(build)

  make_zip(inputdir, build, "IND2065ER", ind_labels(psidR:::psid_latest_wave()))

  captured <- capture_flog({
    processPSID(inputdir, outputdir, "ind")
  })

  expect_identical(captured$value$status, "created")
  expect_true(file.exists(file.path(outputdir, "IND2065ER.rda")))
  expect_false(file.exists(file.path(outputdir, "psid_waves_detected.csv")))
})

test_that("no matching labels still import and warn", {
  inputdir <- tempfile("input_")
  dir.create(inputdir)
  outputdir <- tempfile("output_")
  build <- tempfile("build_")
  dir.create(build)

  df <- data.frame(ERT00001 = 1L, ERT00002 = 2L)
  write_fixed_width(df, build, "IND2065ER")
  zip_files(file.path(build, c("IND2065ER.txt", "IND2065ER.sas")),
            file.path(inputdir, "IND2065ER.zip"))

  captured <- capture_flog({
    processPSID(inputdir, outputdir, "ind")
  })

  expect_identical(captured$value$status, "created")
  expect_true(file.exists(file.path(outputdir, "IND2065ER.rda")))
  expect_false(file.exists(file.path(outputdir, "psid_waves_detected.csv")))
  expect_true(any(grepl("^WARN.*no detected wave", captured$log)))
})

test_that("FAM label year mismatch warns and does not store", {
  inputdir <- tempfile("input_")
  dir.create(inputdir)
  outputdir <- tempfile("output_")
  build <- tempfile("build_")
  dir.create(build)

  fam_labels <- data.frame(var = "ERT00001",
                           label = "2063 FAMILY INTERVIEW (ID) NUMBER",
                           stringsAsFactors = FALSE)
  make_zip(inputdir, build, "FAM2065ER", fam_labels)

  captured <- capture_flog({
    processPSID(inputdir, outputdir, "fam")
  })

  expect_identical(captured$value$status, "created")
  expect_true(file.exists(file.path(outputdir, "FAM2065ER.rda")))
  expect_false(file.exists(file.path(outputdir, "psid_waves_detected.csv")))
  expect_true(any(grepl("^WARN.*differs from file year", captured$log)))
})

test_that("an unreadable existing CSV is left untouched", {
  inputdir <- tempfile("input_")
  dir.create(inputdir)
  outputdir <- tempfile("output_")
  dir.create(outputdir)
  build <- tempfile("build_")
  dir.create(build)

  csv <- file.path(outputdir, "psid_waves_detected.csv")
  writeLines("wrong,header\n1,2", csv)
  before <- tools::md5sum(csv)

  make_zip(inputdir, build, "IND2065ER", ind_labels(c(2063, 2065)))

  captured <- capture_flog({
    processPSID(inputdir, outputdir, "ind")
  })

  expect_identical(captured$value$status, "created")
  expect_identical(tools::md5sum(csv), before)
  expect_true(any(grepl("^WARN.*left untouched", captured$log)))
})

test_that("psid_pick_ind_file never returns the CSV", {
  inputdir <- tempfile("input_")
  dir.create(inputdir)
  outputdir <- tempfile("output_")
  build <- tempfile("build_")
  dir.create(build)

  make_zip(inputdir, build, "IND2065ER", ind_labels(c(2063, 2065)))

  capture_flog({
    processPSID(inputdir, outputdir, "ind")
  })

  files <- list.files(outputdir)
  expect_true("IND2065ER.rda" %in% files)
  expect_true("psid_waves_detected.csv" %in% files)
  expect_identical(psidR:::psid_pick_ind_file(files), "IND2065ER.rda")
})

test_that("psid_merge_detected merges field by field", {
  dir <- tempfile("dir_")
  dir.create(dir)

  new1 <- data.frame(year = 2065L, ind.interview = "ERT00004",
                     stringsAsFactors = FALSE)
  new2 <- data.frame(year = 2063L, ind.interview = "ERT00001",
                     ind.seq = "ERT00002", ind.head = "ERT00003",
                     stringsAsFactors = FALSE)
  new3 <- data.frame(year = 2065L, ind.seq = "ERT00005",
                     ind.head = "ERT00006", stringsAsFactors = FALSE)
  new4 <- data.frame(year = 2063L, fam.interview = "ERT00009",
                     stringsAsFactors = FALSE)

  psidR:::psid_merge_detected(dir, new1)
  psidR:::psid_merge_detected(dir, new2)
  psidR:::psid_merge_detected(dir, new3)
  psidR:::psid_merge_detected(dir, new4)

  got <- data.table::fread(file.path(dir, "psid_waves_detected.csv"),
                           na.strings = "", data.table = FALSE)
  expect_identical(got$year, c(2063L, 2065L))
  expect_identical(got$ind.interview, c("ERT00001", "ERT00004"))
  expect_identical(got$ind.seq, c("ERT00002", "ERT00005"))
  expect_identical(got$ind.head, c("ERT00003", "ERT00006"))
  expect_identical(got$fam.interview, c("ERT00009", NA_character_))
  expect_false(file.exists(file.path(dir, "psid_waves_detected.csv.part")))
})

test_that("process_one without detect creates no CSV", {
  inputdir <- tempfile("input_")
  dir.create(inputdir)
  outputdir <- tempfile("output_")
  dir.create(outputdir)
  build <- tempfile("build_")
  dir.create(build)

  make_zip(inputdir, build, "IND2065ER", ind_labels(c(2063, 2065)))

  out <- file.path(outputdir, "IND2065ER.rda")
  process_one(file.path(inputdir, "IND2065ER.zip"), out, "rda")

  expect_true(file.exists(out))
  expect_false(file.exists(file.path(outputdir, "psid_waves_detected.csv")))
})
