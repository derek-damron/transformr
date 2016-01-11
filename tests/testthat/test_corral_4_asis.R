context('Corral - type="asis"')

x <- c(NA,'a','b','b','c',NA,'c','c','d','d','d','d',NA)

#####
# group is numeric
#

# Function outputs
out_1_name   <- corral(x, 'asis', 1)
out_1.5_name <- corral(x, 'asis', 1.5)
out_2_name   <- corral(x, 'asis', 2)
out_3_name   <- corral(x, 'asis', 3)
out_4_name   <- corral(x, 'asis', 4)
out_5_name   <- corral(x, 'asis', 5)

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

test_that('Check - group is numeric', {
  expect_identical(out_1_name,   exact_1_name)
  expect_identical(out_1.5_name, exact_1.5_name)
  expect_identical(out_2_name,   exact_2_name)
  expect_identical(out_3_name,   exact_3_name)
  expect_identical(out_4_name,   exact_4_name)
  expect_identical(out_5_name,   exact_5_name)
})

#####
# group is not numeric
#

# Function outputs
out_d_name     <- corral(x, 'asis', letters[4])
out_dc_name    <- corral(x, 'asis', letters[4:3])
out_dcb_name   <- corral(x, 'asis', letters[4:2])
out_dcba_name  <- corral(x, 'asis', letters[4:1])
out_edcba_name <- corral(x, 'asis', letters[5:1])

# Expected outputs
exact_d_name     <- factor(c(NA,'Other','Other','Other','Other',NA,'Other','Other','d','d','d','d',NA),
                           levels=c(letters[4], 'Other'))
exact_dc_name    <- factor(c(NA,'Other','Other','Other','c',NA,'c','c','d','d','d','d',NA),
                           levels=c(letters[4:3],'Other'))
exact_dcb_name   <- factor(c(NA,'Other','b','b','c',NA,'c','c','d','d','d','d',NA),
                           levels=c(letters[4:2],'Other'))
exact_dcba_name  <- factor(c(NA,'a','b','b','c',NA,'c','c','d','d','d','d',NA),
                           levels=c(letters[4:1]))
exact_edcba_name <- exact_dcba_name

test_that('Check - group is not numeric', {
  expect_identical(out_d_name,     exact_d_name)
  expect_identical(out_dc_name,    exact_dc_name)
  expect_identical(out_dcb_name,   exact_dcb_name)
  expect_identical(out_dcba_name,  exact_dcba_name)
  expect_identical(out_edcba_name, exact_edcba_name)
})
