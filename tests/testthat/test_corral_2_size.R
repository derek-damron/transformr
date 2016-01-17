context('Corral - type="size"')

x <- c(NA,'a','b','b','c',NA,'c','c','d','d','d','d',NA)

#####
# groups is missing
#

out_missing_size <- corral(x, 'size')
exact_missing_size <- factor(x, levels=letters[4:1])

test_that('Check - groups is missing', {
  expect_identical(out_missing_size, exact_missing_size)
})

#####
# groups is numeric
#

# Function outputs
out_1_size   <- corral(x, 'size', 1)
out_1.5_size <- corral(x, 'size', 1.5)
out_2_size   <- corral(x, 'size', 2)
out_3_size   <- corral(x, 'size', 3)
out_4_size   <- corral(x, 'size', 4)
out_5_size   <- corral(x, 'size', 5)

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

test_that('Check - groups is numeric', {
  expect_identical(out_1_size,   exact_1_size)
  expect_identical(out_1.5_size, exact_1.5_size)
  expect_identical(out_2_size,   exact_2_size)
  expect_identical(out_3_size,   exact_3_size)
  expect_identical(out_4_size,   exact_4_size)
  expect_identical(out_5_size,   exact_5_size)
})

#####
# groups is not numeric
#

# Function outputs
out_a_size     <- corral(x, 'size', letters[1])
out_ab_size    <- corral(x, 'size', letters[1:2])
out_abc_size   <- corral(x, 'size', letters[1:3])
out_abcd_size  <- corral(x, 'size', letters[1:4])
out_abcde_size <- corral(x, 'size', letters[1:5])

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

test_that('Check - groups is not numeric', {
  expect_identical(out_a_size,     exact_a_size)
  expect_identical(out_ab_size,    exact_ab_size)
  expect_identical(out_abc_size,   exact_abc_size)
  expect_identical(out_abcd_size,  exact_abcd_size)
  expect_identical(out_abcde_size, exact_abcde_size)
})
