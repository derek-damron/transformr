context('Trim - Two-sided Calculations')

x <- c(NA,1:5,NA,6:10,NA)

# Value lo/hi input
lo_val <- 2
hi_val <- 9

# Percent lo/hi input
lo_per <- .1
hi_per <- .9

# Function outputs
out_val_explicit <- trim(x, 'value', lo=lo_val, hi=hi_val)
out_per_explicit <- trim(x, 'percentile', lo=lo_per, hi=hi_per)
out_val_smart <- trim(x, lo=lo_val, hi=hi_val)
out_per_smart <- trim(x, lo=lo_per, hi=hi_per)
# Round to two decimal places for identical checks
out_val_explicit <- round(out_val_explicit, 2)
out_per_explicit <- round(out_per_explicit, 2)
out_val_smart <- round(out_val_smart, 2)
out_per_smart <- round(out_per_smart, 2)

# Expected outputs
exact_val <- c(NA,2   ,2:5,NA,6:9,9   ,NA)
exact_per <- c(NA,1.37,2:5,NA,6:9,9.63,NA)

test_that('Value checks', {
  expect_identical(out_val_explicit, exact_val)
  expect_identical(out_val_explicit, exact_val)
  expect_identical(out_val_smart,    exact_val)
  expect_identical(out_val_smart,    exact_val)
})

test_that('Percentile checks', {
  expect_identical(out_per_explicit, exact_per)
  expect_identical(out_per_explicit, exact_per)
  expect_identical(out_per_smart,    exact_per)
  expect_identical(out_per_smart,    exact_per)
})
