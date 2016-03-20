context('Impute - impute_ecdf')

test_that('args', {
  expect_error(impute_ecdf(),
               'Please provide a vector x to use for ecdf sampling')
  expect_error(impute_ecdf(1, n="a"),
               'n must be a positive integer')
  expect_error(impute_ecdf(1, n=0),
               'n must be a positive integer')
})

#####
# Numeric
#

x <- c(NA, 1, 1, 1, NA, 1, 2, 3, NA)

test_that('ecdf', {
    set.seed(666)
    expect_equal(impute(x, impute_ecdf),
                 c(1.871842, 1, 1, 1, 1, 1, 2, 3, 2.890069),
                 tolerance=1e-6)
})
