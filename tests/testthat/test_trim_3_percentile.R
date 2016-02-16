context('Trim - type="percentile"')

x <- c(NA, 1:5, NA, 6:10, NA)

# lo/hi input
lo <- .1
hi <- .9

# Function outputs
out_both <- trim(x, "percentile", lo=lo, hi=hi)
out_lo   <- trim(x, "percentile", lo=lo)
out_hi   <- trim(x, "percentile", hi=hi)

# Round to two decimal places for identical checks
out_both <- round(out_both, 2)
out_lo   <- round(out_lo, 2)
out_hi   <- round(out_hi, 2)

# Expected outputs
exact_both <- c(NA, 1.37, 2:5, NA, 6:9, 9.63, NA)
exact_lo   <- c(NA, 1.37, 2:5, NA, 6:10,      NA)
exact_hi   <- c(NA, 1:5,       NA, 6:9, 9.63, NA)

test_that('Check - percentile', {
    expect_identical(out_both, exact_both)
    expect_identical(out_lo,   exact_lo)
    expect_identical(out_hi,   exact_hi)
})
