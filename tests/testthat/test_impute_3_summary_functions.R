context('Impute - Summary functions')

x <- c(NA, 1, 1, 1, NA, 1, 2, 3, NA)

test_that('mean', {
  expect_equal(impute(x, mean),
               c(1.5, 1, 1, 1, 1.5, 1, 2, 3, 1.5))
})

test_that('median', {
  expect_equal(impute(x, median),
               c(1, 1, 1, 1, 1, 1, 2, 3, 1))
})

test_that('min/max', {
  expect_equal(impute(x, min),
               c(1, 1, 1, 1, 1, 1, 2, 3, 1))
  expect_equal(impute(x, max),
               c(3, 1, 1, 1, 3, 1, 2, 3, 3))
})
