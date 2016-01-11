context('Corral - Arguments')

x <- 1:10

test_that('Check - x', {
  expect_error(corral(),
               'Please provide a vector x')
  expect_error(corral(x),
               'Please provide a value for groups')
  expect_error(corral(x, groups=0),
               'groups must be a positive integer if supplied as a numeric value')
})
