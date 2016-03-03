context('Impute - Values')

#####
# Numeric
#

x <- c(NA, 1, 1, 1, NA, 1, 2, 3, NA)

test_that('Numeric - single value', {
  expect_equal(impute(x, -1),
               c(-1, 1, 1, 1, -1, 1, 2, 3, -1))
})

test_that('Numeric - multiple values (recycling)', {
  expect_warning(impute(x, -1:-2),
                 "number of items to replace is not a multiple of replacement length")
  expect_equal(suppressWarnings(impute(x, -1:-2)),
               c(-1, 1, 1, 1, -2, 1, 2, 3, -1))
})

test_that('Numeric - multiple values (no recycling)', {
  expect_equal(impute(x, -1:-3),
               c(-1, 1, 1, 1, -2, 1, 2, 3, -3))
})

#####
# Character
#

x <- c(NA, "a", "a", "a", NA, "a", "b", "c", NA)

test_that('Character - single value', {
  expect_equal(impute(x, "new"),
               c("new", "a", "a", "a", "new", "a", "b", "c", "new"))
})

test_that('Character - multiple values (recycling)', {
  expect_warning(impute(x, c("new1", "new2")),
                 "number of items to replace is not a multiple of replacement length")
  expect_equal(suppressWarnings(impute(x, c("new1", "new2"))),
               c("new1", "a", "a", "a", "new2", "a", "b", "c", "new1"))
})

test_that('Character - multiple values (no recycling)', {
  expect_equal(impute(x, c("new1", "new2", "new3")),
               c("new1", "a", "a", "a", "new2", "a", "b", "c", "new3"))
})
