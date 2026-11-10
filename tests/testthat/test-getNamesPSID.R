context("getNamesPSID")

test_that("getNamesPSID returns the requested year with no warning", {
  cwf <- data.frame(Y2019 = c("A1", "B1"), Y2021 = c("A2", "B2"),
                    stringsAsFactors = FALSE)

  res <- NULL
  expect_warning(res <- getNamesPSID("A1", cwf, years = 2021), NA)

  expect_equal(res$variable, "A2")
})

test_that("getNamesPSID warns for years missing from the crosswalk", {
  cwf <- data.frame(Y2019 = c("A1", "B1"), Y2021 = c("A2", "B2"),
                    stringsAsFactors = FALSE)

  res <- NULL
  expect_warning(res <- getNamesPSID("A1", cwf, years = c(2021, 2023)), "2023")

  expect_equal(res$variable, "A2")
})

test_that("getNamesPSID does not warn when years is NULL", {
  cwf <- data.frame(Y2019 = c("A1", "B1"), Y2021 = c("A2", "B2"),
                    stringsAsFactors = FALSE)

  expect_warning(getNamesPSID("A1", cwf, years = NULL), NA)
})
