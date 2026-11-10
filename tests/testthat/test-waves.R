context("wave helpers")

test_that("the first 42 supported waves match the historical prefix", {
  waves <- psidR:::psid_supported_waves()

  expect_identical(waves[seq_len(42)], c(1968:1997, seq(1999, 2021, 2)))
  expect_true(!is.unsorted(waves, strictly = TRUE))
  expect_identical(waves, unique(waves))
})

test_that("check_supported_waves accepts supported years and rejects others", {
  expect_silent(psidR:::check_supported_waves(2021))
  expect_error(psidR:::check_supported_waves(1998), "1998")
  expect_error(psidR:::check_supported_waves(c(2021, 2099)), "2099")
})

test_that("psid_family_table years are a subset of makeids years", {
  fam_years <- psidR:::psid_family_table()$year
  ids_years <- psidR:::makeids()$year

  expect_true(all(fam_years %in% ids_years))
})

test_that("makeids per-year vectors are internally consistent", {
  ids <- psidR:::makeids()
  n <- nrow(ids)

  for (col in c("ind.interview", "ind.seq", "ind.head", "fam.interview",
                "ind.head.num")) {
    expect_equal(length(ids[[col]]), n)
  }

  for (col in c("ind.interview", "ind.seq", "ind.head", "fam.interview")) {
    vals <- ids[[col]]
    non_na <- vals[!is.na(vals)]
    expect_identical(non_na, unique(non_na))
  }

  expect_true(all(ids$ind.head.num[ids$year <= 1982] == 1))
  expect_true(all(ids$ind.head.num[ids$year > 1982] == 10))

  expect_equal(ids$year[is.na(ids$ind.seq)], 1968)
})

test_that("psid_pick_ind_file picks the highest year and prefers .rda", {
  expect_identical(
    psidR:::psid_pick_ind_file(
      c("IND2019ER.rda", "IND2021ER.RData", "FAM2021ER.rda", "psid_waves.csv")
    ),
    "IND2021ER.RData"
  )

  expect_identical(
    psidR:::psid_pick_ind_file(c("IND2021ER.rda", "IND2021ER.RData")),
    "IND2021ER.rda"
  )
})

test_that("psid_pick_ind_file returns character(0) when no index matches", {
  expect_identical(
    psidR:::psid_pick_ind_file(c("FAM2021ER.rda", "psid_waves.csv")),
    character(0)
  )
})

test_that("psid_pick_ind_file accepts lower-case names and ignores lookalikes", {
  expect_identical(psidR:::psid_pick_ind_file("ind2021er.rda"), "ind2021er.rda")
  expect_identical(psidR:::psid_pick_ind_file("IND2021ER_old.rda"), character(0))
})
