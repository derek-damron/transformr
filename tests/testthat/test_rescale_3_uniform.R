context('Rescale - type="uniform"')

x <- c(NA, 1:5, NA, 6:10, NA)

# Uniform min/max input
uniform_min <- -5
uniform_max <- 7

# Function outputs
out_uniform <- rescale(x, 'uniform', min=uniform_min, max=uniform_max)

# Round to two decimal places for identical checks
out_uniform <- round(out_uniform, 2)

# Expected outputs
exact_uniform <- c(NA, -5.00, -3.67, -2.33, -1.00, 0.33,
                   NA, 1.67, 3.00, 4.33, 5.67, 7.00,
                   NA)

test_that('Check - uniform', {
  expect_identical(out_uniform, exact_uniform)
})
