context('Impute - impute_mode')

test_that('args', {
  expect_error(impute_mode(),
               'Please provide a vector x to compute the mode')
})

#####
# Numeric
#

x <- c(NA, 1, 1, 1, NA, 1, 2, 3, NA)

test_that('numeric - no ties', {
    expect_equal(impute(x, impute_mode),
                 c(1, 1, 1, 1, 1, 1, 2, 3, 1))
})

x <- c(NA, 1, 1, 1, NA, 2, 2, 2, NA)

test_that('numeric - ties', {
    set.seed(666)
    expect_equal(impute(x, impute_mode),
                 c(2, 1, 1, 1, 1, 2, 2, 2, 2))
})

#####
# Character
#

x <- c(NA, "a", "a", "a", NA, "a", "b", "c", NA)

test_that('character - no ties', {
    expect_equal(impute(x, impute_mode),
                 c("a", "a", "a", "a", "a", "a", "b", "c", "a"))
})

x <- c(NA, "a", "a", "a", NA, "b", "b", "b", NA)

test_that('character - ties', {
    set.seed(666)
    expect_equal(impute(x, impute_mode),
                 c("b", "a", "a", "a", "a", "b", "b", "b", "b"))
})
