context('Corral - Calculations - By Size')

x <- c(NA,'a','b','b','c',NA,'c','c','d','d','d','d',NA)

#####
# group is numeric
#

# Function outputs
out_1_size   <- corral(x, 1,   'size')
out_1.5_size <- corral(x, 1.5, 'size')
out_2_size   <- corral(x, 2,   'size')
out_3_size   <- corral(x, 3,   'size')
out_4_size   <- corral(x, 4,   'size')
out_5_size   <- corral(x, 5,   'size')

# Expected outputs
exact_1_size <- factor(c(NA,'Other','Other','Other','Other',NA,'Other','Other','Other','Other','Other','Other',NA),
                       levels=c('Other'))
exact_1.5_size <- exact_1_size
exact_2_size <- factor(c(NA,'Other','Other','Other','Other',NA,'Other','Other','d','d','d','d',NA),
                       levels=c(letters[4],'Other'))
exact_3_size <- factor(c(NA,'Other','Other','Other','c',NA,'c','c','d','d','d','d',NA),
                       levels=c(letters[4:3],'Other'))
exact_4_size <- factor(c(NA,'a','b','b','c',NA,'c','c','d','d','d','d',NA),
                       levels=c(letters[4:1]))
exact_5_size <- exact_4_size

# Test
test_that('group is numeric', {
  expect_identical(out_1_size,   exact_1_size   )
  expect_identical(out_1.5_size, exact_1.5_size )
  expect_identical(out_2_size,   exact_2_size   )
  expect_identical(out_3_size,   exact_3_size   )
  expect_identical(out_4_size,   exact_4_size   )
  expect_identical(out_5_size,   exact_5_size   )
})

#####
# group is not numeric
#

# Function outputs
out_a_size     <- corral(x, letters[1],   'size')
out_ab_size    <- corral(x, letters[1:2], 'size')
out_abc_size   <- corral(x, letters[1:3], 'size')
out_abcd_size  <- corral(x, letters[1:4], 'size')
out_abcde_size <- corral(x, letters[1:5], 'size')

# Expected outputs
exact_a_size     <- factor(c(NA,'a','Other','Other','Other',NA,'Other','Other','Other','Other','Other','Other',NA),
                           levels=c(letters[1], 'Other'))
exact_ab_size    <- factor(c(NA,'a','b','b','Other',NA,'Other','Other','Other','Other','Other','Other',NA),
                           levels=c(letters[2:1],'Other'))
exact_abc_size   <- factor(c(NA,'a','b','b','c',NA,'c','c','Other','Other','Other','Other',NA),
                           levels=c(letters[3:1],'Other'))
exact_abcd_size  <- factor(c(NA,'a','b','b','c',NA,'c','c','d','d','d','d',NA),
                           levels=c(letters[4:1]))
exact_abcde_size <- exact_abcd_size

# Test
test_that('group is not numeric', {
  expect_identical(out_a_size,     exact_a_size     )
  expect_identical(out_ab_size,    exact_ab_size    )
  expect_identical(out_abc_size,   exact_abc_size   )
  expect_identical(out_abcd_size,  exact_abcd_size  )
  expect_identical(out_abcde_size, exact_abcde_size )
})
