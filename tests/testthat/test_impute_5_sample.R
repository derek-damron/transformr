context('Impute - impute_sample')

test_that('args', {
  expect_error(impute_sample(),
               'Please provide a vector x to use for sampling')
  expect_error(impute_sample(1, n="a"),
               'n must be a positive integer')
  expect_error(impute_sample(1, n=0),
               'n must be a positive integer')
})

#####
# Numeric
#

x <- c(NA, 1, 1, 1, NA, 1, 2, 3, NA)

test_that('numeric - no ties', {
    set.seed(666)
    expect_equal(impute(x, impute_sample),
                 c(3, 1, 1, 1, 1, 1, 2, 3, 2))
})

x <- c(NA, 1, 1, 1, NA, 2, 2, 2, NA)

test_that('numeric - ties', {
    set.seed(666)
    expect_equal(impute(x, impute_sample),
                 c(1, 1, 1, 1, 2, 2, 2, 2, 1))
})

#####
# Character
#

x <- c(NA, "a", "a", "a", NA, "a", "b", "c", NA)

test_that('no ties', {
    set.seed(666)
    expect_equal(impute(x, impute_sample),
                 c("c", "a", "a", "a", "a", "a", "b", "c", "b"))
})

x <- c(NA, "a", "a", "a", NA, "b", "b", "b", NA)

test_that('ties', {
    set.seed(666)
    expect_equal(impute(x, impute_sample),
                 c("a", "a", "a", "a", "b", "b", "b", "b", "a"))
})
