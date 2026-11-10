context("extendVarsPSID")

test_that("alive concept is extended with anchor dataset and label", {
  cwf <- data.frame(Y2017 = "a2017", Y2019 = "a2019", Y2021 = "a2021",
                    stringsAsFactors = FALSE)
  vars <- data.frame(dataset = "fam", year = 2017L, variable = "a2017",
                     label = "Label A", name = "conceptA",
                     stringsAsFactors = FALSE)

  ext <- NULL
  expect_warning(ext <- extendVarsPSID(vars, cwf), NA)

  expect_equal(ext$vars$year, c(2017L, 2019L, 2021L))
  expect_equal(ext$vars$variable, c("a2017", "a2019", "a2021"))
  expect_true(all(ext$vars$dataset == "fam"))
  expect_true(all(ext$vars$label == "Label A"))
})

test_that("discontinued concept is skipped by default and extended with names", {
  cwf <- data.frame(
    Y2017 = c("alive2017", "disc2016"),
    Y2019 = c("alive2019", "disc2019"),
    Y2021 = c("alive2021", "disc2021"),
    stringsAsFactors = FALSE
  )
  vars <- data.frame(
    dataset = c("fam", "fam"),
    year = c(2017L, 2016L),
    variable = c("alive2017", "disc2016"),
    label = c("Alive", "Disc"),
    name = c("alive", "disc"),
    stringsAsFactors = FALSE
  )

  ext <- NULL
  expect_warning(ext <- extendVarsPSID(vars, cwf), NA)
  expect_equal(ext$review$name[ext$review$status == "skipped: discontinued"],
               "disc")
  expect_equal(nrow(ext$vars[ext$vars$name == "disc", ]), 1L)

  ext2 <- NULL
  expect_warning(ext2 <- extendVarsPSID(vars, cwf, names = "disc"), NA)
  disc_rows <- ext2$vars[ext2$vars$name == "disc", ]
  expect_equal(disc_rows$year, c(2016L, 2019L, 2021L))
  expect_equal(disc_rows$variable, c("disc2016", "disc2019", "disc2021"))
})

test_that("a concept missing for a year is flagged and warned", {
  cwf <- data.frame(Y2017 = "g2017", Y2019 = "g2019", Y2021 = NA,
                    stringsAsFactors = FALSE)
  vars <- data.frame(dataset = "fam", year = 2017L, variable = "g2017",
                     label = "Gone", name = "gone", stringsAsFactors = FALSE)

  ext <- NULL
  expect_warning(ext <- extendVarsPSID(vars, cwf),
                 "no variable in crosswalk for this year")

  expect_equal(ext$vars$year, c(2017L, 2019L))
  expect_true(any(ext$review$year == 2021L &
                    ext$review$status == "no variable in crosswalk for this year"))
})

test_that("an anchor missing from the crosswalk is flagged and warned", {
  cwf <- data.frame(Y2017 = "a2017", Y2019 = "a2019", Y2021 = "a2021",
                    stringsAsFactors = FALSE)
  vars <- data.frame(dataset = "fam", year = 2017L, variable = "ghost2017",
                     label = "Ghost", name = "ghost", stringsAsFactors = FALSE)

  ext <- NULL
  expect_warning(ext <- extendVarsPSID(vars, cwf),
                 "anchor not found in crosswalk")

  expect_equal(nrow(ext$vars), 1L)
  expect_true(any(ext$review$status == "anchor not found in crosswalk"))
})

test_that("existing rows are unchanged and a second run adds nothing", {
  cwf <- data.frame(Y2017 = "a2017", Y2019 = "a2019", Y2021 = "a2021",
                    stringsAsFactors = FALSE)
  vars <- data.frame(dataset = "fam", year = 2017L, variable = "a2017",
                     label = "Label A", name = "conceptA",
                     stringsAsFactors = FALSE)

  ext1 <- NULL
  expect_warning(ext1 <- extendVarsPSID(vars, cwf), NA)

  expect_identical(ext1$vars$year[1], vars$year)
  expect_identical(ext1$vars$variable[1], vars$variable)
  expect_identical(ext1$vars$dataset[1], vars$dataset)
  expect_identical(ext1$vars$label[1], vars$label)
  expect_identical(ext1$vars$name[1], vars$name)

  ext2 <- NULL
  expect_warning(ext2 <- extendVarsPSID(ext1$vars, cwf), NA)
  expect_identical(ext2$vars$year, ext1$vars$year)
  expect_identical(ext2$vars$variable, ext1$vars$variable)
  expect_identical(ext2$vars$name, ext1$vars$name)
})

test_that("names with surrounding spaces are trimmed", {
  cwf <- data.frame(Y2017 = "s2017", Y2019 = "s2019", Y2021 = "s2021",
                    stringsAsFactors = FALSE)
  vars <- data.frame(dataset = "fam", year = 2017L, variable = "s2017",
                     label = "Spaced", name = "  spaced  ",
                     stringsAsFactors = FALSE)

  ext <- NULL
  expect_warning(ext <- extendVarsPSID(vars, cwf), NA)

  expect_true(all(ext$vars$name == "spaced"))
})

test_that("explicit years without a crosswalk column do not leak a getNamesPSID warning", {
  cwf <- data.frame(Y2017 = "a2017", Y2019 = "a2019", Y2021 = "a2021",
                    stringsAsFactors = FALSE)
  vars <- data.frame(dataset = "fam", year = 2017L, variable = "a2017",
                     label = "A", name = "conceptA", stringsAsFactors = FALSE)

  warns <- character()
  ext <- withCallingHandlers(
    extendVarsPSID(vars, cwf, years = c(2021, 2023)),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_false(any(grepl("no crosswalk column", warns, fixed = TRUE)))
  expect_true(any(grepl("no variable in crosswalk for this year", warns,
                        fixed = TRUE)))
  expect_true(any(ext$review$year == 2023L &
                    ext$review$status == "no variable in crosswalk for this year"))
  expect_true(2021L %in% ext$vars$year)
})

test_that("unsorted explicit years give the variable of each year", {
  cwf <- data.frame(Y2017 = "a2017", Y2019 = "a2019", Y2021 = "a2021",
                    stringsAsFactors = FALSE)
  vars <- data.frame(dataset = "fam", year = 2017L, variable = "a2017",
                     label = "A", name = "conceptA", stringsAsFactors = FALSE)

  ext_sorted <- NULL
  expect_warning(ext_sorted <- extendVarsPSID(vars, cwf, years = c(2019, 2021)),
                 NA)
  ext_unsorted <- NULL
  expect_warning(ext_unsorted <- extendVarsPSID(vars, cwf, years = c(2021, 2019)),
                 NA)

  ord_sorted <- order(ext_sorted$vars$name, ext_sorted$vars$year)
  ord_unsorted <- order(ext_unsorted$vars$name, ext_unsorted$vars$year)
  expect_identical(ext_sorted$vars[ord_sorted, ], ext_unsorted$vars[ord_unsorted, ])

  added <- ext_unsorted$vars[ext_unsorted$vars$year %in% c(2019L, 2021L), ]
  expect_equal(added$variable[added$year == 2019L], "a2019")
  expect_equal(added$variable[added$year == 2021L], "a2021")
})

