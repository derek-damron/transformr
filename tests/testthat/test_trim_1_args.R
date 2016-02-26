context("Trim - Arguments")

x <- 1:10

test_that("Check - x", {
  expect_error(trim(),
               "Please provide a vector x to trim")
  expect_error(trim("a"),
               "x must be a numeric vector")
})

test_that("Check - lo/hi - general", {
  expect_error(trim(x),
               "Please provide at least one lo or hi value")
  expect_error(trim(x, lo="a"),
               "lo must be a numeric value if specified")
  expect_error(trim(x, lo=1:2),
               "lo must be a single value if specified")
  expect_error(trim(x, hi="a"),
               "hi must be a numeric value if specified")
  expect_error(trim(x, hi=1:2),
               "hi must be a single value if specified")
})

test_that("Check - lo/hi - value", {
  expect_error(trim(x, "value", lo=1, hi=0),
               "lo must be less than or equal to the hi")
})

test_that("Check - lo/hi - percentile", {
  expect_error(trim(x, "percentile", lo=-1),
               "lo must be in the range 0 <= lo <= 1 for method='percentile'")
  expect_error(trim(x, "percentile", lo=2),
               "lo must be in the range 0 <= lo <= 1 for method='percentile'")
  expect_error(trim(x, "percentile", hi=-1),
               "hi must be in the range 0 <= hi <= 1 for method='percentile'")
  expect_error(trim(x, "percentile", hi=2),
               "hi must be in the range 0 <= hi <= 1 for method='percentile'")
  expect_error(trim(x, "percentile", lo=-1, hi=2),
               "lo must be in the range 0 <= lo <= 1 for method='percentile'")
  expect_error(trim(x, "percentile", lo=1, hi=0),
               "lo must be less than or equal to the hi")
})

test_that("Check - replace", {
  expect_error(trim(x, lo=0, replace="a"),
               "replace must be a numeric value or NA if specified")
  expect_error(trim(x, lo=0, replace=1:2),
               "replace must be a single value if specified")
  expect_equal(trim(x, "value", lo=2, hi=9, replace=NA),
               c(NA, 2:9, NA))
  expect_equal(trim(x, "value", lo=2, hi=9, replace=-1),
               c(-1, 2:9, -1))
  expect_equal(trim(x, "percentile", lo=.1, hi=.9, replace=NA),
               c(NA, 2:9, NA))
  expect_equal(trim(x, "percentile", lo=.1, hi=.9, replace=-1),
               c(-1, 2:9, -1))
})
