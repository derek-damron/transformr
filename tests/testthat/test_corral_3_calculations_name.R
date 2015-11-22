context('Corral - Calculations - By Name')

x <- c(NA,'a','b','b','c',NA,'c','c','d','d','d','d',NA)

#####
# group is numeric
#

# Function outputs
out_1_name   <- corral(x, 1,   'name')
out_1.5_name <- corral(x, 1.5, 'name')
out_2_name   <- corral(x, 2,   'name')
out_3_name   <- corral(x, 3,   'name')
out_4_name   <- corral(x, 4,   'name')
out_5_name   <- corral(x, 5,   'name')

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

# Test
test_that('group is numeric', {
  expect_identical(out_1_name,   exact_1_name   )
  expect_identical(out_1.5_name, exact_1.5_name )
  expect_identical(out_2_name,   exact_2_name   )
  expect_identical(out_3_name,   exact_3_name   )
  expect_identical(out_4_name,   exact_4_name   )
  expect_identical(out_5_name,   exact_5_name   )
})

#####
# group is not numeric
#

# Function outputs
out_a_name     <- corral(x, letters[1],   'name')
out_ab_name    <- corral(x, letters[1:2], 'name')
out_abc_name   <- corral(x, letters[1:3], 'name')
out_abcd_name  <- corral(x, letters[1:4], 'name')
out_abcde_name <- corral(x, letters[1:5], 'name')

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

# Test
test_that('group is not numeric', {
  expect_identical(out_a_name,     exact_a_name     )
  expect_identical(out_ab_name,    exact_ab_name    )
  expect_identical(out_abc_name,   exact_abc_name   )
  expect_identical(out_abcd_name,  exact_abcd_name  )
  expect_identical(out_abcde_name, exact_abcde_name )
})
