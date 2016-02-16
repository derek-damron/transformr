context('Rescale - type="minmax"')

x <- c(NA, 1:5, NA, 6:10, NA)

# Min/max input
minmax_min <- -5
minmax_max <- 7

# Function outputs
out_minmax <- rescale(x, 'minmax', min=minmax_min, max=minmax_max)

# Round to two decimal places for identical checks
out_minmax <- round(out_minmax, 2)

# Expected outputs
exact_minmax <- c(NA, -5.00, -3.67, -2.33, -1.00, 0.33,
                  NA, 1.67, 3.00, 4.33, 5.67, 7.00,
                  NA)

test_that('Check - minmax', {
  expect_identical(out_minmax, exact_minmax)
})
