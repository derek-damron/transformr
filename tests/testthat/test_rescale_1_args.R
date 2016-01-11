context('Rescale - Arguments')

x <- 1:10

test_that('Check - x', {
  expect_error(rescale(),
               'Please provide a vector x')
  expect_error(rescale(x='a'),
               'x must be integer or numeric')
})

test_that('Check - normal', {
  expect_error(rescale(x, 'normal', mean='a'),
               'mean must be a numeric or integer value')
  expect_error(rescale(x, 'normal', mean=0, sd='a'),
               'sd must be a positive numeric or integer value')
  expect_error(rescale(x, 'normal', mean=0, sd=-1),
               'sd must be a positive numeric or integer value')
})

test_that('Check - uniform', {
  expect_error(rescale(x, 'uniform', min='a'),
               'min must be a numeric or integer value')
  expect_error(rescale(x, 'uniform', min=0, max='a'),
               'max must be a numeric or integer value')
  expect_error(rescale(x, 'uniform', min=0, max=-1),
               'min must be less than or equal to max')
})
