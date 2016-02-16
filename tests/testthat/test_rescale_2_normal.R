context('Rescale - type="normal"')

x <- c(NA, 1:5, NA, 6:10, NA)

# Normal mean/sd input
normal_mean <- 5
normal_sd <- 2

# Function outputs
out_normal <- rescale(x, mean=normal_mean, sd=normal_sd)

# Round to two decimal places for identical checks
out_normal <- round(out_normal, 2)

# Expected outputs
exact_normal  <- c(NA, 2.03, 2.69, 3.35, 4.01, 4.67,
                   NA, 5.33, 5.99, 6.65, 7.31, 7.97,
                   NA)

test_that('Check - normal', {
  expect_identical(out_normal, exact_normal)
  expect_identical(out_normal, round(rescale(x, "normal", mean=normal_mean, sd=normal_sd), 2))
})
