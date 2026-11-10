context("wave detection")

# Synthetic IND label triples (obviously fake names ERTxxxxx).
make_ind_labels <- function(years) {
  rel_form <- function(y) {
    if (y <= 1987L) {
      "RELATIONSHIP TO HEAD"
    } else if (y <= 2015L) {
      "RELATION TO HEAD"
    } else {
      "RELATION TO REFERENCE PERSON"
    }
  }

  out <- list()
  for (y in years) {
    base <- as.integer(y) * 10L
    yy <- y %% 100L
    vars <- sprintf("ERT%05d", base + 0:2)
    labs <- c(
      sprintf("%d INTERVIEW NUMBER", y),
      sprintf("SEQUENCE NUMBER %02d", yy),
      sprintf("%s %02d", rel_form(y), yy)
    )
    if (y == 1968L) {
      vars <- vars[c(1L, 3L)]
      labs <- labs[c(1L, 3L)]
    }
    out[[length(out) + 1L]] <- data.frame(var = vars, label = labs,
                                          stringsAsFactors = FALSE)
  }
  do.call(rbind, out)
}

test_that("a full synthetic set is detected with no rejections", {
  years <- c(1968:1997, as.integer(seq(1999, 2025, by = 2)))
  labels <- make_ind_labels(years)

  res <- localpsidR:::psid_detect_ind_waves(labels)

  expect_identical(names(res$waves),
                   c("year", "ind.interview", "ind.seq", "ind.head"))
  expect_identical(res$waves$year, sort(years))
  expect_identical(nrow(res$rejected), 0L)
  expect_true(is.na(res$waves$ind.seq[res$waves$year == 1968L]))
  expect_identical(res$waves$ind.interview, sprintf("ERT%05d", years * 10L))
  expect_identical(res$waves$ind.head, sprintf("ERT%05d", years * 10L + 2L))
})

test_that("a missing variable rejects only that wave", {
  labels <- make_ind_labels(c(1969, 1970))
  labels <- labels[!(labels$var == sprintf("ERT%05d", 1970L * 10L + 2L)), ]

  res <- localpsidR:::psid_detect_ind_waves(labels)

  expect_true(1969 %in% res$waves$year)
  expect_true(1970 %in% res$rejected$year)
  expect_match(res$rejected$reason[res$rejected$year == 1970], "1970")
})

test_that("a duplicated label rejects only that wave", {
  labels <- make_ind_labels(c(1969, 1970))
  labels <- rbind(labels,
                  data.frame(var = "ERT99999",
                             label = "RELATION TO HEAD 70",
                             stringsAsFactors = FALSE))

  res <- localpsidR:::psid_detect_ind_waves(labels)

  expect_true(1969 %in% res$waves$year)
  expect_true(1970 %in% res$rejected$year)
  expect_match(res$rejected$reason[res$rejected$year == 1970], "1970")
})

test_that("a broken consecutive rule rejects only that wave", {
  labels <- make_ind_labels(1969)
  labels$var[3L] <- sprintf("ERT%05d", 1969L * 10L + 3L)  # make relation non-consecutive

  res <- localpsidR:::psid_detect_ind_waves(labels)

  expect_true(1969 %in% res$rejected$year)
  expect_match(res$rejected$reason[res$rejected$year == 1969], "1969")
})

test_that("a two-digit year that disagrees rejects only that wave", {
  labels <- make_ind_labels(1969)
  labels$label[3L] <- "RELATION TO HEAD 70"  # 1970, not 1969

  res <- localpsidR:::psid_detect_ind_waves(labels)

  expect_false(1969 %in% res$waves$year)
  expect_true(1969 %in% res$rejected$year)
  expect_match(res$rejected$reason[res$rejected$year == 1969], "1969")
})

test_that("the RELATIONSHIP TO RESPONDENT lookalike does not match", {
  labels <- make_ind_labels(1995)
  labels <- rbind(labels,
                  data.frame(var = "ERT99998",
                             label = "RELATIONSHIP TO RESPONDENT 95",
                             stringsAsFactors = FALSE))

  res <- localpsidR:::psid_detect_ind_waves(labels)

  expect_true(1995 %in% res$waves$year)
  expect_identical(res$waves$ind.head[res$waves$year == 1995],
                   sprintf("ERT%05d", 1995L * 10L + 2L))
})

test_that("an interview-only label is rejected by the IND detector", {
  labels <- data.frame(var = "ERT0001",
                       label = "1980 INTERVIEW NUMBER",
                       stringsAsFactors = FALSE)

  res <- localpsidR:::psid_detect_ind_waves(labels)

  expect_equal(nrow(res$waves), 0L)
  expect_true(1980 %in% res$rejected$year)
  expect_match(res$rejected$reason[res$rejected$year == 1980], "1980")
})

test_that("an invalid relation variable name rejects only that wave", {
  labels <- make_ind_labels(c(1969, 1970))
  labels$var[labels$var == sprintf("ERT%05d", 1969L * 10L + 2L)] <- "AGE_1"

  res <- expect_warning(localpsidR:::psid_detect_ind_waves(labels), NA)

  expect_true(1969 %in% res$rejected$year)
  expect_match(res$rejected$reason[res$rejected$year == 1969], "1969")
  expect_true(1970 %in% res$waves$year)
})

test_that("the FAM detector handles modern, older, duplicated and missing forms", {
  modern <- data.frame(var = "ERT0001",
                       label = "2023 FAMILY INTERVIEW (ID) NUMBER",
                       stringsAsFactors = FALSE)
  res <- localpsidR:::psid_detect_fam_wave(modern)
  expect_identical(res$wave$year, 2023L)
  expect_identical(res$wave$fam.interview, "ERT0001")
  expect_identical(res$reason, "")

  for (lab in c("1980 INTERVIEW NUMBER", "1994 INTERVIEW #",
                "INTERVIEW NUMBER 68")) {
    old_labels <- data.frame(var = "ERT0002", label = lab,
                             stringsAsFactors = FALSE)
    res_old <- localpsidR:::psid_detect_fam_wave(old_labels)
    expect_null(res_old$wave)
    expect_true(nzchar(res_old$reason))
  }

  dup <- data.frame(
    var = c("ERT0003", "ERT0004"),
    label = c("2023 FAMILY INTERVIEW (ID) NUMBER",
              "2023 FAMILY INTERVIEW (ID) NUMBER"),
    stringsAsFactors = FALSE
  )
  res_dup <- localpsidR:::psid_detect_fam_wave(dup)
  expect_null(res_dup$wave)
  expect_true(nzchar(res_dup$reason))

  none_labels <- data.frame(var = "ERT0005", label = "AGE",
                            stringsAsFactors = FALSE)
  res_none <- localpsidR:::psid_detect_fam_wave(none_labels)
  expect_null(res_none$wave)
  expect_true(nzchar(res_none$reason))
})

test_that("empty labels produce zero rows / NULL without error", {
  empty <- data.frame(var = character(), label = character(),
                      stringsAsFactors = FALSE)

  res_ind <- localpsidR:::psid_detect_ind_waves(empty)
  expect_equal(nrow(res_ind$waves), 0L)
  expect_equal(nrow(res_ind$rejected), 0L)
  expect_identical(names(res_ind$waves),
                   c("year", "ind.interview", "ind.seq", "ind.head"))

  res_fam <- localpsidR:::psid_detect_fam_wave(empty)
  expect_null(res_fam$wave)
  expect_true(nzchar(res_fam$reason))
})

test_that("local SAS dir reproduces built-in years", {
  dir <- Sys.getenv("PSID_LOCAL_SAS_DIR")
  skip_if_not(nzchar(dir))

  ids <- localpsidR:::makeids()

  ind_files <- list.files(dir, pattern = "^IND[0-9]{4}ER\\.sas$",
                          recursive = TRUE, full.names = TRUE,
                          ignore.case = TRUE)
  fam_files <- list.files(dir, pattern = "^FAM[0-9]{4}ER\\.sas$",
                          recursive = TRUE, full.names = TRUE,
                          ignore.case = TRUE)

  # The test must never be empty.
  expect_true(length(ind_files) > 0L || length(fam_files) > 0L)

  if (length(ind_files) > 0L) {
    ind_year <- as.integer(sub(".*IND([0-9]{4})ER\\.sas$", "\\1",
                               basename(ind_files)))
    ind_files <- ind_files[which.max(ind_year)]
    labels <- do.call(rbind, lapply(ind_files, localpsidR:::psid_parse_sas_labels))
    res <- localpsidR:::psid_detect_ind_waves(labels)
    o <- match(as.integer(ids$year), res$waves$year)
    expect_identical(res$waves$year[o], as.integer(ids$year))
    expect_identical(res$waves$ind.interview[o],
                     as.character(ids$ind.interview))
    expect_identical(res$waves$ind.seq[o], as.character(ids$ind.seq))
    expect_identical(res$waves$ind.head[o], as.character(ids$ind.head))
  }

  for (f in fam_files) {
    y <- as.integer(sub(".*FAM([0-9]{4})ER\\.sas$", "\\1", basename(f)))
    if (y >= 1999L) {
      labels <- localpsidR:::psid_parse_sas_labels(f)
      res_fam <- localpsidR:::psid_detect_fam_wave(labels)
      y_idx <- which(as.integer(ids$year) == y)
      expected_fam <- as.character(ids$fam.interview[y_idx])
      expect_identical(res_fam$wave$fam.interview, expected_fam)
    }
  }
})
