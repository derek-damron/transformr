context('Corral - Arguments')

x <- 1:10

test_that('Check - x', {
  expect_error(corral(),
               'Please provide a vector x to corral')
})

test_that('Check - groups', {
  expect_error(corral(x, groups=0),
               'groups must be a positive integer if supplied as a numeric value')
})

test_that('Check - collect', {
  expect_error(corral(x, groups=1, collect=c("Other", "Misc")),
               "collect must be a single character string")
  expect_identical(corral(x, groups=1),
                   factor(rep("Other", 10), levels="Other"))
  expect_identical(corral(x, groups=1, collect="Misc"),
                   factor(rep("Misc", 10), levels="Misc"))
  expect_identical(corral(x, groups=2, collect=NA),
                   factor(c(1, rep(NA, 9)), levels="1"))
})
