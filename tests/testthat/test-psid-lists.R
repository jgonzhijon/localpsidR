context("shipped psid lists")

files <- c("famvars.txt", "indvars.txt",
           "famvars-small.txt", "indvars-small.txt")

test_that("shipped lists resolve via system.file", {
  for (f in files) {
    p <- system.file("psid-lists", f, package = "psidR")
    expect_true(nzchar(p))
    expect_true(file.exists(p))
  }
})

test_that("shipped lists have the expected columns and supported years", {
  for (f in files) {
    d <- data.table::fread(system.file("psid-lists", f, package = "psidR"),
                           na.strings = "")
    expect_true(all(c("dataset", "year", "variable", "label", "name") %in%
                      names(d)))
    expect_true(all(d$year %in% psidR:::psid_supported_waves()))
  }
})

test_that("shipped lists have unique (year, name) and clean name/variable", {
  for (f in files) {
    d <- data.table::fread(system.file("psid-lists", f, package = "psidR"),
                           na.strings = "")
    expect_false(any(duplicated(d[, .(year, name)])))
    expect_true(all(d$name == trimws(d$name)))
    expect_true(all(!is.na(d$variable) & nzchar(d$variable)))
  }
})
