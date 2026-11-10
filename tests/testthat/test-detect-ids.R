context("detect ids")

future_years <- c(2063, 2065)

test_that("makeids appends extra waves and leaves built-in rows untouched", {
  base <- locapsidR:::makeids()

  ids <- locapsidR:::makeids(fake_extra_wave(future_years))

  # no arguments still returns exactly the built-in table
  expect_identical(locapsidR:::makeids(), base)

  # still a keyed data.table with the same key
  expect_true(data.table::is.data.table(ids))
  expect_identical(data.table::key(ids), "year")

  # the keyed join returns the synthetic extra row
  for (y in future_years) {
    row <- ids[list(y)]
    expect_identical(row$year, as.double(y))
    expect_identical(row$ind.interview, sprintf("ERT%05d", y * 10L + 1L))
    expect_identical(row$ind.seq, sprintf("ERT%05d", y * 10L + 2L))
    expect_identical(row$ind.head, sprintf("ERT%05d", y * 10L + 3L))
    expect_identical(row$fam.interview, sprintf("ERT%05d", y * 10L + 4L))
    expect_identical(row$ind.head.num, 10)
    expect_identical(row$stratum, "ER31996")
  }

  # all per-column lengths are equal
  n <- nrow(ids)
  for (col in names(ids)) {
    expect_identical(length(ids[[col]]), n)
  }

  # built-in rows are unchanged
  builtin <- ids[ids$year %in% base$year]
  expect_identical(as.data.frame(builtin), as.data.frame(base))
})

test_that("makeids validates extra and lets built-in years win", {
  base <- locapsidR:::makeids()

  # a built-in year inside extra is silently ignored
  ids <- locapsidR:::makeids(fake_extra_wave(2023))
  expect_identical(as.data.frame(ids), as.data.frame(base))

  # a missing column is an error naming the column
  bad <- fake_extra_wave(2063)
  bad$fam.interview <- NULL
  expect_error(locapsidR:::makeids(bad), "fam.interview")

  # a duplicated year is an error
  expect_error(locapsidR:::makeids(fake_extra_wave(c(2063, 2063))), "duplicated")

  # a non-data.frame is an error
  expect_error(locapsidR:::makeids(1:3), "data.frame")
})

test_that("wave helpers accept extra_years and default to built-in only", {
  waves0 <- locapsidR:::psid_supported_waves()
  latest0 <- locapsidR:::psid_latest_wave()
  ind0 <- locapsidR:::psid_ind_name()
  fam0 <- locapsidR:::psid_family_table()

  fam <- locapsidR:::psid_family_table(2063)
  row <- fam[fam$year == 2063, ]
  expect_identical(nrow(row), 1L)
  expect_true(is.na(row$file))
  expect_identical(fam$year, sort(fam$year))

  sw <- locapsidR:::psid_supported_waves(extra_years = future_years)
  expect_true(all(future_years %in% sw))
  expect_identical(sw, sort(unique(sw)))
  expect_identical(locapsidR:::psid_latest_wave(extra_years = future_years), 2065)
  expect_identical(locapsidR:::psid_ind_name(extra_years = future_years),
                   "IND2065ER")

  expect_silent(locapsidR:::check_supported_waves(2063, extra_years = 2063))
  expect_error(locapsidR:::check_supported_waves(2063), "does not support")

  # no arguments reproduce today's output exactly
  expect_identical(locapsidR:::psid_family_table(), fam0)
  expect_identical(locapsidR:::psid_supported_waves(), waves0)
  expect_identical(locapsidR:::psid_latest_wave(), latest0)
  expect_identical(locapsidR:::psid_ind_name(), ind0)
})

write_detected_csv <- function(dir, df) {
  data.table::fwrite(df, file.path(dir, "psid_waves_detected.csv"),
                     na = "", row.names = FALSE, quote = FALSE)
}

detected_cols <- c("year", "ind.interview", "ind.seq", "ind.head",
                   "fam.interview", "ind.head.num")

test_that("psid_read_detected returns zero rows for an absent CSV", {
  dir <- tempfile("det_")
  dir.create(dir)

  expect_warning(out <- locapsidR:::psid_read_detected(dir), NA)

  expect_true(is.data.frame(out))
  expect_identical(nrow(out), 0L)
  expect_identical(names(out), detected_cols)
  expect_identical(vapply(out, typeof, character(1)),
                   c(year = "double", ind.interview = "character",
                     ind.seq = "character", ind.head = "character",
                     fam.interview = "character", ind.head.num = "double"))
})

test_that("psid_read_detected reads a complete CSV and applies head_code", {
  dir <- tempfile("det_")
  dir.create(dir)
  csv <- data.frame(
    year = c("2063", "2065"),
    ind.interview = c("ERT20631", "ERT20651"),
    ind.seq = c("ERT20632", "ERT20652"),
    ind.head = c("ERT20633", "ERT20653"),
    fam.interview = c("ERT20634", "ERT20654"),
    stringsAsFactors = FALSE
  )
  write_detected_csv(dir, csv)

  out <- locapsidR:::psid_read_detected(dir)
  expect_identical(out$year, c(2063, 2065))
  expect_identical(out$ind.interview, c("ERT20631", "ERT20651"))
  expect_identical(out$ind.seq, c("ERT20632", "ERT20652"))
  expect_identical(out$ind.head, c("ERT20633", "ERT20653"))
  expect_identical(out$fam.interview, c("ERT20634", "ERT20654"))
  expect_identical(out$ind.head.num, c(10, 10))

  out11 <- locapsidR:::psid_read_detected(dir, head_code = 11)
  expect_identical(out11$ind.head.num, c(11, 11))
})

test_that("psid_read_detected drops invalid rows with a warning", {
  dir <- tempfile("det_")
  dir.create(dir)
  built <- locapsidR:::makeids()

  # a built-in year row is ignored silently
  csv <- data.frame(
    year = c("2023", "2063"),
    ind.interview = c("ERT99991", "ERT20631"),
    ind.seq = c("ERT99992", "ERT20632"),
    ind.head = c("ERT99993", "ERT20633"),
    fam.interview = c("ERT99994", "ERT20634"),
    stringsAsFactors = FALSE
  )
  write_detected_csv(dir, csv)
  out <- locapsidR:::psid_read_detected(dir)
  expect_identical(out$year, 2063)
  expect_identical(nrow(out), 1L)

  # an empty fam.interview (IND-only import) is dropped with a warning
  csv <- data.frame(
    year = "2063",
    ind.interview = "ERT20631",
    ind.seq = "ERT20632",
    ind.head = "ERT20633",
    fam.interview = NA_character_,
    stringsAsFactors = FALSE
  )
  write_detected_csv(dir, csv)
  cap <- capture_flog(locapsidR:::psid_read_detected(dir))
  expect_identical(nrow(cap$value), 0L)
  expect_true(any(grepl("^WARN.*dropped detected wave year 2063.*empty fam.interview", cap$log)))

  # an invalid variable name is dropped with a warning
  csv <- data.frame(
    year = "2063",
    ind.interview = "12345",
    ind.seq = "ERT20632",
    ind.head = "ERT20633",
    fam.interview = "ERT20634",
    stringsAsFactors = FALSE
  )
  write_detected_csv(dir, csv)
  cap <- capture_flog(locapsidR:::psid_read_detected(dir))
  expect_identical(nrow(cap$value), 0L)
  pat <- "^WARN.*dropped detected wave year 2063.*invalid variable name in ind.interview"
  expect_true(any(grepl(pat, cap$log)))

  # a duplicated year drops all its rows with a warning
  csv <- data.frame(
    year = c("2063", "2063"),
    ind.interview = c("ERT20631", "ERT20661"),
    ind.seq = c("ERT20632", "ERT20662"),
    ind.head = c("ERT20633", "ERT20663"),
    fam.interview = c("ERT20634", "ERT20664"),
    stringsAsFactors = FALSE
  )
  write_detected_csv(dir, csv)
  cap <- capture_flog(locapsidR:::psid_read_detected(dir))
  expect_identical(nrow(cap$value), 0L)
  expect_true(any(grepl("^WARN.*dropped detected wave year 2063.*duplicated year", cap$log)))

  # a variable colliding with a built-in name is dropped with a warning
  csv <- data.frame(
    year = "2063",
    ind.interview = built$ind.interview[1],
    ind.seq = "ERT20632",
    ind.head = "ERT20633",
    fam.interview = "ERT20634",
    stringsAsFactors = FALSE
  )
  write_detected_csv(dir, csv)
  cap <- capture_flog(locapsidR:::psid_read_detected(dir))
  expect_identical(nrow(cap$value), 0L)
  pat <- "^WARN.*dropped detected wave year 2063.*repeated variable name in ind.interview"
  expect_true(any(grepl(pat, cap$log)))

  # a valid row survives next to a dropped one
  csv <- data.frame(
    year = c("2063", "2065"),
    ind.interview = c("ERT20631", "ERT20651"),
    ind.seq = c("ERT20632", "ERT20652"),
    ind.head = c("ERT20633", "ERT20653"),
    fam.interview = c("ERT20634", NA_character_),
    stringsAsFactors = FALSE
  )
  write_detected_csv(dir, csv)
  cap <- capture_flog(locapsidR:::psid_read_detected(dir))
  expect_identical(cap$value$year, 2063)
  expect_identical(nrow(cap$value), 1L)
  expect_true(any(grepl("^WARN.*dropped detected wave year 2065.*empty fam.interview", cap$log)))
})

test_that("psid_read_detected drops non-positive-integer years", {
  dir <- tempfile("det_")
  dir.create(dir)
  csv <- data.frame(
    year = c("abc", "0"),
    ind.interview = c("ERT20631", "ERT20651"),
    ind.seq = c("ERT20632", "ERT20652"),
    ind.head = c("ERT20633", "ERT20653"),
    fam.interview = c("ERT20634", "ERT20654"),
    stringsAsFactors = FALSE
  )
  write_detected_csv(dir, csv)

  cap <- capture_flog(locapsidR:::psid_read_detected(dir))
  expect_identical(nrow(cap$value), 0L)
  pat_abc <- "^WARN.*dropped detected wave year abc.*not a positive integer"
  pat_zero <- "^WARN.*dropped detected wave year 0.*not a positive integer"
  expect_true(any(grepl(pat_abc, cap$log)))
  expect_true(any(grepl(pat_zero, cap$log)))
})

test_that("psid_read_detected upper-cases names and drops cross-year repeats", {
  dir <- tempfile("det_")
  dir.create(dir)

  # lower-case variable names come back upper-cased
  csv <- data.frame(
    year = "2063",
    ind.interview = "ert20631",
    ind.seq = "ert20632",
    ind.head = "ert20633",
    fam.interview = "ert20634",
    stringsAsFactors = FALSE
  )
  write_detected_csv(dir, csv)
  out <- locapsidR:::psid_read_detected(dir)
  expect_identical(out$ind.interview, "ERT20631")
  expect_identical(out$ind.seq, "ERT20632")
  expect_identical(out$ind.head, "ERT20633")
  expect_identical(out$fam.interview, "ERT20634")

  # two detected years sharing an ind.interview name are both dropped
  csv <- data.frame(
    year = c("2063", "2065"),
    ind.interview = c("ERT99991", "ERT99991"),
    ind.seq = c("ERT20632", "ERT20652"),
    ind.head = c("ERT20633", "ERT20653"),
    fam.interview = c("ERT20634", "ERT20654"),
    stringsAsFactors = FALSE
  )
  write_detected_csv(dir, csv)
  cap <- capture_flog(locapsidR:::psid_read_detected(dir))
  expect_identical(nrow(cap$value), 0L)
  pat2063 <- "^WARN.*dropped detected wave year 2063.*repeated variable name in ind.interview"
  pat2065 <- "^WARN.*dropped detected wave year 2065.*repeated variable name in ind.interview"
  expect_true(any(grepl(pat2063, cap$log)))
  expect_true(any(grepl(pat2065, cap$log)))
})

test_that("psid_read_detected warns and returns zero rows on a bad file", {
  dir <- tempfile("det_")
  dir.create(dir)

  # wrong header (missing expected columns)
  writeLines(c("a,b", "1,2"), file.path(dir, "psid_waves_detected.csv"))
  cap <- capture_flog(locapsidR:::psid_read_detected(dir))
  expect_identical(nrow(cap$value), 0L)
  expect_identical(names(cap$value), detected_cols)
  expect_true(any(grepl("expected columns", cap$log)))
})

test_that("psid_read_detected rejects a bad head_code", {
  dir <- tempfile("det_")
  dir.create(dir)
  csv <- data.frame(
    year = "2063",
    ind.interview = "ERT20631",
    ind.seq = "ERT20632",
    ind.head = "ERT20633",
    fam.interview = "ERT20634",
    stringsAsFactors = FALSE
  )
  write_detected_csv(dir, csv)

  expect_error(locapsidR:::psid_read_detected(dir, head_code = "x"), "head_code")
  expect_error(locapsidR:::psid_read_detected(dir, head_code = c(1, 2)),
               "head_code")
  expect_error(locapsidR:::psid_read_detected(dir, head_code = Inf), "head_code")
})

test_that("fake_psid_datadir writes a detected year's files", {
  n <- 20L
  d <- fake_psid_datadir(2063, n = n, extra = fake_extra_wave(2063))

  expect_true("FAM2063ER.rda" %in% list.files(d$dir))
  expect_true("IND2063ER.rda" %in% list.files(d$dir))

  env <- new.env()
  load(file.path(d$dir, "IND2063ER.rda"), envir = env)
  ind <- get(ls(env), env)

  expect_true(all(c("ERT20631", "ERT20632", "ERT20633") %in% names(ind)))
})
