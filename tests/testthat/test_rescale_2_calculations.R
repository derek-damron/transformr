context('Rescale - Calculations')

x <- c(NA,1:5,NA,6:10,NA)

# Normal mean/sd input
normal_mean <- 5
normal_sd <- 2

# Uniform min/max input
uniform_min <- -5
uniform_max <- 7

# Function outputs
out_normal <- rescale(x, 'n', mean=normal_mean, sd=normal_sd)
out_uniform <- rescale(x, 'u', min=uniform_min, max=uniform_max)
# Round to two decimal places for identical checks
out_normal <- round(out_normal, 2)
out_uniform <- round(out_uniform, 2)

# Expected outputs
exact_normal  <- c(NA,2.03,2.69,3.35,4.01,4.67,NA,5.33,5.99,6.65,7.31,7.97,NA)
exact_uniform <- c(NA,-5.00,-3.67,-2.33,-1.00,0.33,NA,1.67,3.00,4.33,5.67,7.00,NA)

test_that('Normal check', {
  expect_identical(out_normal, exact_normal)
})

test_that('Uniform check', {
  expect_identical(out_uniform, exact_uniform)
})

