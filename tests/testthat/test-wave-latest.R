context("latest wave")

test_that("psid_latest_wave and psid_ind_name track makeids", {
  ids <- localpsidR:::makeids()

  expect_equal(localpsidR:::psid_latest_wave(), max(ids$year))
  expect_equal(localpsidR:::psid_ind_name(),
               paste0("IND", localpsidR:::psid_latest_wave(), "ER"))
})

test_that("the latest wave row of makeids is complete and consistent", {
  ids <- localpsidR:::makeids()
  ys <- sort(unique(ids$year), decreasing = TRUE)
  latest <- ids[ids$year == ys[1]]
  previous <- ids[ids$year == ys[2]]

  id_cols <- c(latest$ind.interview, latest$ind.seq, latest$ind.head,
               latest$fam.interview)

  expect_false(any(is.na(id_cols)))
  expect_true(all(grepl("^(ER|V)[0-9]+$", id_cols)))
  expect_equal(latest$ind.head.num, previous$ind.head.num)
})

test_that("build.panel builds a panel for the latest wave end to end", {
  n <- 20L
  w <- localpsidR:::psid_latest_wave()
  d <- fake_psid_datadir(w, n = n)

  out <- build.panel(
    datadir = d$dir,
    fam.vars = data.frame(year = w, v = "FAMVAR1"),
    heads.only = FALSE,
    design = "all",
    loglevel = WARN
  )

  expect_equal(nrow(out), n)
})

test_that("2023 is a supported wave (pinned)", {
  expect_true(2023 %in% localpsidR:::psid_supported_waves())
})
