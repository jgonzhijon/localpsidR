context("detect buildpanel")

# Evaluate build.panel and capture both its result (or error) and the log.
# nolint start: object_usage_linter.
run_bp <- function(...) {
  capture_flog(
    tryCatch(build.panel(...), error = function(e) e)
  )
}
# nolint end: object_usage_linter.

# Shift the family interview column of FAM<year>ER.rda by `delta` so that the
# individual and family interview numbers no longer overlap.
shift_fam_interview <- function(dir, year, delta) {
  env <- new.env()
  load(file.path(dir, paste0("FAM", year, "ER.rda")), envir = env)
  x <- get(ls(env), env)
  interview_col <- setdiff(names(x), "FAMVAR1")
  x[[interview_col]] <- x[[interview_col]] + delta
  save(x, file = file.path(dir, paste0("FAM", year, "ER.rda")))
}

test_that("detected wave builds with detect_waves = TRUE", {
  n <- 20L
  d <- fake_psid_datadir(2063, n = n, extra = fake_extra_wave(2063))
  write_fake_detected(d$dir, fake_extra_wave(2063))

  # without detect_waves the call fails even though the CSV is present
  res0 <- run_bp(datadir = d$dir,
                 fam.vars = data.frame(year = 2063, v = "FAMVAR1"),
                 heads.only = FALSE, design = "all", loglevel = WARN)
  expect_true(inherits(res0$value, "error"))
  expect_true(grepl("does not support", conditionMessage(res0$value)))

  # with detect_waves = TRUE it returns n rows with the expected columns
  res <- run_bp(datadir = d$dir,
                fam.vars = data.frame(year = 2063, v = "FAMVAR1"),
                heads.only = FALSE, design = "all", loglevel = WARN,
                detect_waves = TRUE)
  out <- res$value
  expect_equal(nrow(out), n)
  expect_true(all(c("pid", "year", "interview", "sequence", "relation.head",
                    "v") %in% names(out)))

  # WARN lines naming the four synthetic variables and the head-code carry-over
  expect_true(any(grepl("ERT20631.*ERT20632.*ERT20633.*ERT20634", res$log)))
  expect_true(any(grepl("carried over", res$log)))
})

test_that("detected wave builds alongside built-in years", {
  n <- 20L
  d <- fake_psid_datadir(c(2021, 2063), n = n, extra = fake_extra_wave(2063))
  write_fake_detected(d$dir, fake_extra_wave(2063))

  res <- run_bp(datadir = d$dir,
                fam.vars = data.frame(year = c(2021, 2063),
                                      v = c("FAMVAR1", "FAMVAR1")),
                heads.only = FALSE, design = "all", loglevel = WARN,
                detect_waves = TRUE)
  expect_equal(nrow(res$value), 2 * n)
})

test_that("head_code is used for detected waves and ignored otherwise", {
  n <- 20L
  d <- fake_psid_datadir(2063, n = n,
                         extra = fake_extra_wave(2063, head_num = 11))
  write_fake_detected(d$dir, fake_extra_wave(2063, head_num = 11))

  res <- run_bp(datadir = d$dir,
                fam.vars = data.frame(year = 2063, v = "FAMVAR1"),
                heads.only = TRUE, design = "all", loglevel = WARN,
                detect_waves = TRUE, head_code = 11)
  expect_equal(nrow(res$value), n)
  expect_true(any(grepl("given by head_code", res$log)))

  # without head_code the carried-over code (10) does not match the
  # fixture's head_num = 11, so no rows are heads
  res_nohead <- run_bp(datadir = d$dir,
                       fam.vars = data.frame(year = 2063, v = "FAMVAR1"),
                       heads.only = TRUE, design = "all", loglevel = WARN,
                       detect_waves = TRUE)
  expect_equal(nrow(res_nohead$value), 0L)

  # head_code is ignored unless detect_waves = TRUE
  res2 <- run_bp(datadir = d$dir,
                 fam.vars = data.frame(year = 2063, v = "FAMVAR1"),
                 heads.only = TRUE, design = "all", loglevel = WARN,
                 head_code = 11)
  expect_true(inherits(res2$value, "error"))
  expect_true(any(grepl("head_code is ignored", res2$log)))
})

test_that("extra waves in the CSV are ignored when not requested", {
  n <- 20L
  d <- fake_psid_datadir(c(2021, 2063), n = n, extra = fake_extra_wave(2063))
  write_fake_detected(d$dir, fake_extra_wave(2063))

  res <- run_bp(datadir = d$dir,
                fam.vars = data.frame(year = 2021, v = "FAMVAR1"),
                heads.only = FALSE, design = "all", loglevel = WARN,
                detect_waves = TRUE)
  expect_equal(nrow(res$value), n)
  expect_false(any(grepl("detected variables|head code|psid_waves_detected",
                         res$log)))
})

test_that("detect_waves handles a missing or incomplete CSV", {
  n <- 20L
  d <- fake_psid_datadir(2063, n = n, extra = fake_extra_wave(2063))
  # no CSV written

  res <- run_bp(datadir = d$dir,
                fam.vars = data.frame(year = 2063, v = "FAMVAR1"),
                heads.only = FALSE, design = "all", loglevel = WARN,
                detect_waves = TRUE)
  expect_true(inherits(res$value, "error"))
  expect_true(grepl("does not support", conditionMessage(res$value)))
  expect_true(any(grepl("psid_waves_detected.csv", res$log)))

  # a CSV row with an empty fam.interview (IND-only import) is dropped
  ind_only <- fake_extra_wave(2063)
  ind_only$fam.interview <- NA_character_
  write_fake_detected(d$dir, ind_only)
  res2 <- run_bp(datadir = d$dir,
                 fam.vars = data.frame(year = 2063, v = "FAMVAR1"),
                 heads.only = FALSE, design = "all", loglevel = WARN,
                 detect_waves = TRUE)
  expect_true(inherits(res2$value, "error"))
  expect_true(grepl("does not support", conditionMessage(res2$value)))
  expect_true(any(grepl("psid_waves_detected.csv", res2$log)))
  expect_true(any(grepl("2063", res2$log)))

  # detect_waves = TRUE with built-in years only and no CSV works quietly
  d21 <- fake_psid_datadir(2021, n = n)
  res3 <- run_bp(datadir = d21$dir,
                 fam.vars = data.frame(year = 2021, v = "FAMVAR1"),
                 heads.only = FALSE, design = "all", loglevel = WARN,
                 detect_waves = TRUE)
  expect_equal(nrow(res3$value), n)
  expect_false(any(grepl("detected wave|psid_waves_detected.csv", res3$log)))
})

test_that("detect_waves = TRUE matches FALSE for built-in years", {
  n <- 20L
  d <- fake_psid_datadir(c(1985, 1986), n = n)

  out_t <- run_bp(datadir = d$dir,
                  fam.vars = data.frame(year = c(1985, 1986),
                                        v = c("FAMVAR1", "FAMVAR1")),
                  heads.only = FALSE, design = "all", loglevel = WARN,
                  detect_waves = TRUE)$value
  out_f <- run_bp(datadir = d$dir,
                  fam.vars = data.frame(year = c(1985, 1986),
                                        v = c("FAMVAR1", "FAMVAR1")),
                  heads.only = FALSE, design = "all", loglevel = WARN,
                  detect_waves = FALSE)$value

  data.table::setorder(out_t, pid, year)
  data.table::setorder(out_f, pid, year)
  expect_equal(as.data.frame(out_t), as.data.frame(out_f))
})

test_that("overlap check stops for detected waves but not built-in ones", {
  n <- 20L
  d <- fake_psid_datadir(2063, n = n, extra = fake_extra_wave(2063))
  write_fake_detected(d$dir, fake_extra_wave(2063))
  shift_fam_interview(d$dir, 2063, 1000)

  res <- run_bp(datadir = d$dir,
                fam.vars = data.frame(year = 2063, v = "FAMVAR1"),
                heads.only = FALSE, design = "all", loglevel = WARN,
                detect_waves = TRUE)
  expect_true(inherits(res$value, "error"))
  expect_true(grepl("2063", conditionMessage(res$value)))
  expect_true(grepl("interview numbers", conditionMessage(res$value),
                    fixed = TRUE))

  # the same shift on a built-in year raises no overlap error
  d21 <- fake_psid_datadir(2021, n = n)
  shift_fam_interview(d21$dir, 2021, 1000)
  res21 <- run_bp(datadir = d21$dir,
                  fam.vars = data.frame(year = 2021, v = "FAMVAR1"),
                  heads.only = FALSE, design = "all", loglevel = WARN)
  expect_false(inherits(res21$value, "error"))
  expect_equal(nrow(res21$value), 0L)
})

test_that("missing index names the detected IND file when detect_waves = TRUE", {
  n <- 20L
  d <- fake_psid_datadir(2063, n = n, extra = fake_extra_wave(2063))
  write_fake_detected(d$dir, fake_extra_wave(2063))
  file.remove(file.path(d$dir, "IND2063ER.rda"))

  res <- run_bp(datadir = d$dir,
                fam.vars = data.frame(year = 2063, v = "FAMVAR1"),
                heads.only = FALSE, design = "all", loglevel = WARN,
                detect_waves = TRUE)
  expect_true(inherits(res$value, "error"))
  expect_true(grepl("IND2063ER", conditionMessage(res$value)))

  res2 <- run_bp(datadir = d$dir,
                 fam.vars = data.frame(year = 2063, v = "FAMVAR1"),
                 heads.only = FALSE, design = "all", loglevel = WARN)
  expect_true(inherits(res2$value, "error"))
  expect_true(grepl("does not support", conditionMessage(res2$value)))
})

test_that("index missing the detected columns errors clearly", {
  n <- 20L
  d <- fake_psid_datadir(2063, n = n, extra = fake_extra_wave(2063))
  write_fake_detected(d$dir, fake_extra_wave(2063))

  env <- new.env()
  load(file.path(d$dir, "IND2063ER.rda"), envir = env)
  ind <- get(ls(env), env)
  ind <- ind[, !(names(ind) %in% c("ERT20631", "ERT20632", "ERT20633"))]
  x <- ind
  save(x, file = file.path(d$dir, "IND2063ER.rda"))

  res <- run_bp(datadir = d$dir,
                fam.vars = data.frame(year = 2063, v = "FAMVAR1"),
                heads.only = FALSE, design = "all", loglevel = WARN,
                detect_waves = TRUE)
  expect_true(inherits(res$value, "error"))
  expect_true(grepl("ERT20631", conditionMessage(res$value)))
  expect_true(grepl("2063", conditionMessage(res$value)))
})

test_that("detect_waves must be a single non-NA logical", {
  n <- 20L
  d <- fake_psid_datadir(2021, n = n)
  fam.vars <- data.frame(year = 2021, v = "FAMVAR1")

  for (bad in list("yes", NA, c(TRUE, FALSE))) {
    res <- run_bp(datadir = d$dir, fam.vars = fam.vars,
                  heads.only = FALSE, design = "all", loglevel = WARN,
                  detect_waves = bad)
    expect_true(inherits(res$value, "error"))
    expect_true(grepl("detect_waves must be TRUE or FALSE",
                      conditionMessage(res$value)))
  }
})
