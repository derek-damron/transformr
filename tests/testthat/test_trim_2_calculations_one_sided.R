context('Trim - One-sided Calculations')

x <- c(NA,1:5,NA,6:10,NA)

# Value lo/hi input
lo_val <- 2
hi_val <- 9

# Percent lo/hi input
lo_per <- .1
hi_per <- .9

# Function outputs
out_val_explicit_lo <- trim(x, 'value', lo=lo_val)
out_val_explicit_hi <- trim(x, 'value', hi=hi_val)
out_per_explicit_lo <- trim(x, 'percentile', lo=lo_per)
out_per_explicit_hi <- trim(x, 'percentile', hi=hi_per)
out_val_smart_lo <- trim(x, lo=lo_val)
out_val_smart_hi <- trim(x, hi=hi_val)
out_per_smart_lo <- trim(x, lo=lo_per)
out_per_smart_hi <- trim(x, hi=hi_per)
# Round to two decimal places for identical checks
out_val_explicit_lo <- round(out_val_explicit_lo, 2)
out_val_explicit_hi <- round(out_val_explicit_hi, 2)
out_per_explicit_lo <- round(out_per_explicit_lo, 2)
out_per_explicit_hi <- round(out_per_explicit_hi, 2)
out_val_smart_lo <- round(out_val_smart_lo, 2)
out_val_smart_hi <- round(out_val_smart_hi, 2)
out_per_smart_lo <- round(out_per_smart_lo, 2)
out_per_smart_hi <- round(out_per_smart_hi, 2)

# Expected outputs
exact_val_lo <- c(NA,2   ,2:5,NA,6:10     ,NA)
exact_val_hi <- c(NA,1:5     ,NA,6:9 ,9   ,NA)
exact_per_lo <- c(NA,1.37,2:5,NA,6:10     ,NA)
exact_per_hi <- c(NA,1:5     ,NA,6:9 ,9.63,NA)

test_that('Value checks', {
  expect_identical(out_val_explicit_lo, exact_val_lo)
  expect_identical(out_val_explicit_hi, exact_val_hi)
  expect_identical(out_val_smart_lo,    exact_val_lo)
  expect_identical(out_val_smart_hi,    exact_val_hi)
})

test_that('Percentile checks', {
  expect_identical(out_per_explicit_lo, exact_per_lo)
  expect_identical(out_per_explicit_hi, exact_per_hi)
  expect_identical(out_per_smart_lo,    exact_per_lo)
  expect_identical(out_per_smart_hi,    exact_per_hi)
})

