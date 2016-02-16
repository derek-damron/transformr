context('Trim - type="value"')

x <- c(NA, 1:5, NA, 6:10, NA)

# lo/hi input
lo <- 2
hi <- 9

# Function outputs
out_both <- trim(x, lo=lo, hi=hi)
out_lo   <- trim(x, lo=lo)
out_hi   <- trim(x, hi=hi)

# Expected outputs
exact_both <- c(NA, 2, 2:5, NA, 6:9, 9, NA)
exact_lo   <- c(NA, 2, 2:5, NA, 6:10,   NA)
exact_hi   <- c(NA, 1:5,    NA, 6:9, 9, NA)

test_that('Check - value', {
    expect_identical(out_both, exact_both)
    expect_identical(out_lo,   exact_lo)
    expect_identical(out_hi,   exact_hi)
    expect_identical(out_both, trim(x, "value", lo=lo, hi=hi))
    expect_identical(out_lo,   trim(x, "value", lo=lo))
    expect_identical(out_hi,   trim(x, "value", hi=hi))
})

