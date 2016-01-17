context('Corral - type="name"')

x <- c(NA,'a','b','b','c',NA,'c','c','d','d','d','d',NA)

#####
# groups is missing
#

out_missing_name <- corral(x, 'name')
exact_missing_name <- factor(x, levels=letters[1:4])

test_that('Check - groups is missing', {
  expect_identical(out_missing_name, exact_missing_name)
})

#####
# groups is numeric
#

# Function outputs
out_1_name   <- corral(x, 'name', 1)
out_1.5_name <- corral(x, 'name', 1.5)
out_2_name   <- corral(x, 'name', 2)
out_3_name   <- corral(x, 'name', 3)
out_4_name   <- corral(x, 'name', 4)
out_5_name   <- corral(x, 'name', 5)

# Expected outputs
exact_1_name <- factor(c(NA,'Other','Other','Other','Other',NA,'Other','Other','Other','Other','Other','Other',NA),
                       levels=c('Other'))
exact_1.5_name <- exact_1_name
exact_2_name <- factor(c(NA,'a','Other','Other','Other',NA,'Other','Other','Other','Other','Other','Other',NA),
                       levels=c(letters[1],'Other'))
exact_3_name <- factor(c(NA,'a','b','b','Other',NA,'Other','Other','Other','Other','Other','Other',NA),
                       levels=c(letters[1:2],'Other'))
exact_4_name <- factor(c(NA,'a','b','b','c',NA,'c','c','d','d','d','d',NA),
                       levels=c(letters[1:4]))
exact_5_name <- exact_4_name

test_that('Check - groups is numeric', {
  expect_identical(out_1_name,   exact_1_name)
  expect_identical(out_1.5_name, exact_1.5_name)
  expect_identical(out_2_name,   exact_2_name)
  expect_identical(out_3_name,   exact_3_name)
  expect_identical(out_4_name,   exact_4_name)
  expect_identical(out_5_name,   exact_5_name)
})

#####
# groups is not numeric
#

# Function outputs
out_a_name     <- corral(x, 'name', letters[1])
out_ab_name    <- corral(x, 'name', letters[1:2])
out_abc_name   <- corral(x, 'name', letters[1:3])
out_abcd_name  <- corral(x, 'name', letters[1:4])
out_abcde_name <- corral(x, 'name', letters[1:5])

# Expected outputs
exact_a_name     <- factor(c(NA,'a','Other','Other','Other',NA,'Other','Other','Other','Other','Other','Other',NA),
                           levels=c(letters[1], 'Other'))
exact_ab_name    <- factor(c(NA,'a','b','b','Other',NA,'Other','Other','Other','Other','Other','Other',NA),
                           levels=c(letters[1:2],'Other'))
exact_abc_name   <- factor(c(NA,'a','b','b','c',NA,'c','c','Other','Other','Other','Other',NA),
                           levels=c(letters[1:3],'Other'))
exact_abcd_name  <- factor(c(NA,'a','b','b','c',NA,'c','c','d','d','d','d',NA),
                           levels=c(letters[1:4]))
exact_abcde_name <- exact_abcd_name

test_that('Check - groups is not numeric', {
  expect_identical(out_a_name,     exact_a_name)
  expect_identical(out_ab_name,    exact_ab_name)
  expect_identical(out_abc_name,   exact_abc_name)
  expect_identical(out_abcd_name,  exact_abcd_name)
  expect_identical(out_abcde_name, exact_abcde_name)
})
