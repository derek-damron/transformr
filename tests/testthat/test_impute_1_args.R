context('Impute - Arguments')

x <- 1:10

test_that('Check - x', {
  expect_error(impute(),
               'Please provide a vector x to impute')
  expect_error(impute(x),
               'x contains no missing values to impute')
})

x <- c(x, NA)

test_that('Check - method', {
  expect_error(impute(x),
               'Please provide a method for imputation')
})
