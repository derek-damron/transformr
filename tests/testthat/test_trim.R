context('Test trim')

x <- 1:10

# Value lo/hi input
lo_val <- 2
hi_val <- 9

# Percent lo/hi input
lo_per <- .1
hi_per <- .9

# Function outputs
out_val_smart <- trim(x, lo_val, hi_val)
out_per_smart <- trim(x, lo_per, hi_per)
out_val_explicit <- trim(x, lo_val, hi_val, 'v')
out_per_explicit <- trim(x, lo_per, hi_per, 'p')
# Round to two decimal places for identical checks
out_val_smart <- round(out_val_smart, 2)
out_per_smart <- round(out_per_smart, 2)
out_val_explicit <- round(out_val_explicit, 2)
out_per_explicit <- round(out_per_explicit, 2)

# Expected outputs
exact_val <- c(2,2:9,9)
exact_per <- c(1.37,2:9,9.63)

test_that('Argument existence checks', {
  expect_error(trim(),
               'Please provide a vector x')
  expect_error(trim(x),
               'Please provide a lower argument lo')
  expect_error(trim(x, lo_val),
               'Please provide a upper argument hi')
})

test_that('Argument validity checks', {
  # General
  expect_error(trim('a', lo_val, hi_val),
               'x must be integer or numeric')
  expect_error(trim(x, 'a', hi_val),
               'lo must be integer or numeric')
  expect_error(trim(x, lo_val, 'a'),
               'hi must be integer or numeric')
})

test_that('Specific type=="v" checks', {
  expect_error(trim(x, hi_val, lo_val, 'v'),
               'Lower argument lo must be smaller than upper argument hi')
})

test_that('Specific type=="p" checks', {
  expect_error(trim(x, -1, hi_per, 'p'),
               'Lower argument lo must be in the range 0 <= lo <= 1')
  expect_error(trim(x, 2, hi_per, 'p'),
               'Lower argument lo must be in the range 0 <= lo <= 1')
  expect_error(trim(x, lo_per, -1, 'p'),
               'Upper argument hi must be in the range 0 <= hi <= 1')
  expect_error(trim(x, lo_per, 2, 'p'),
               'Upper argument hi must be in the range 0 <= hi <= 1')
  expect_error(trim(x, hi_per, lo_per, 'p'),
               'Lower argument lo must be smaller than upper argument hi')
})

test_that('Smart type check', {
  expect_identical(out_val_smart,
                   exact_val)
  expect_identical(out_per_smart,
                   exact_per)
})

test_that('Explicit type check', {
  expect_identical(out_val_explicit,
                   exact_val)
  expect_identical(out_per_explicit,
                   exact_per)
})
