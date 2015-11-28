context('Trim - Arguments')

x <- 1:10

test_that('Check x', {
  expect_error(trim(),
               'Please provide a vector x')
  expect_error(trim('a'),
               'x must be integer or numeric')
})

test_that('Check lo/hi', {
  expect_error(trim(x, lo='a'),
               'lo must be integer or numeric if specified')
  expect_error(trim(x, lo=1:2),
               'lo must be a single value')
  expect_error(trim(x, hi='a'),
               'hi must be integer or numeric if specified')
  expect_error(trim(x, hi=1:2),
               'hi must be a single value')
  expect_error(trim(x),
               'Please provide at least one lo or hi value')
})

test_that('Check lo/hi for type="value"', {
  expect_error(trim(x, lo=1, hi=0, type='v'),
               'lo must be less than or equal to the hi')
})

test_that('Check lo/hi for type="percentile"', {
  expect_error(trim(x, lo=-1, type='p'),
               'lo must be in the range 0 <= lo <= 1')
  expect_error(trim(x, lo=2, type='p'),
               'lo must be in the range 0 <= lo <= 1')
  expect_error(trim(x, hi=-1, type='p'),
               'hi must be in the range 0 <= hi <= 1')
  expect_error(trim(x, hi=2, type='p'),
               'hi must be in the range 0 <= hi <= 1')
  expect_error(trim(x, lo=1, hi=0, type='p'),
               'lo must be less than or equal to the hi')
})
