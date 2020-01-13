# Tests for 'coding' function
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2019-03-16

# Table of contents
# 1) Initial setup
# 2) Tests for 'coding' function
#   2.1) default calls to 'coding' works
#   2.2) 'coding' can apply to a specific variable
#   2.3) output from 'coding' can be passed to 'designmatrix' object
#   2.4) location output from 'coding' is stored can be changed
#   2.5) reference group for effect/dummy coding can be changed

###
### 1) Initial setup
###

# Specify context
context("coding function")

# library( testthat)

# Load in package
library( designmatrix )

# Example data
df = ToothGrowth
slct = list( 'len', c('supp','dose') )

###
### 2) Tests for 'coding' function
###

# 2.1)
test_that( "default calls to 'coding' works", {

  # Create 'designmatrix' object
  dm = designmatrix( df, slct )

  # Base comparison matrix
  SM = matrix( 0, 6, 6 )
  colnames( SM ) = paste( 'X', 1:6, sep = '' )

  # Intercept
  cmp = cbind( X1 = rep( 1, 6 ) )
  expect_equal( coding( dm ), cmp )

  # Identity coding
  cmp = SM; diag(cmp) = 1
  expect_equal( coding( dm, type = 'identity' ), cmp )

  # Dummy coding
  cmp = SM; diag( cmp ) = 1; cmp = cmp[,-1]
  expect_equal( coding( dm, type = 'dummy' ), cmp )

  # Effect coding
  cmp = SM; diag( cmp ) = 1; cmp = cmp[,-1]; cmp[1,] = -1
  expect_equal( coding( dm, type = 'effect' ), cmp )

  # Linear trend
  cmp = cbind( X2 = 1:6 - mean(1:6) )
  expect_equal( coding( dm, type = 'linear' ), cmp )

} )

# 2.2)
test_that( "'coding' can apply to a specific variable", {

  # Create 'designmatrix' object
  dm = designmatrix( df, slct )

  # Base comparison matrix
  SM = matrix( 0, 6, 6 )
  colnames( SM ) = paste( 'X', 1:6, sep = '' )

  # Identity coding
  cmp = SM[,1:2]; cmp[1:3,1] = 1; cmp[4:6,2] = 1
  expect_equal( coding( dm, type = 'identity', variables = 'supp' ),
                cmp )

  # Dummy coding
  cmp = cbind( X2 = rep(0,6) ); cmp[4:6,1] = 1
  expect_equal( coding( dm, type = 'dummy', variables = 'supp' ),
                cmp )

  # Effect coding
  cmp = SM[,2:3]; cmp[c(1,4),] = -1;
  diag(cmp[2:3,]) = 1; diag( cmp[5:6,] ) = 1
  expect_equal( coding( dm, type = 'effect', variables = 'dose' ),
                cmp )

  # Linear trend
  cmp = cbind( X2 = rep( 1:3 - mean(1:3), 2 ) )
  expect_equal( coding( dm, type = 'linear', variables = 'dose' ),
                cmp )

} )

# 2.3)
test_that( "output from 'coding' can be passed to 'designmatrix' object", {

  # Reset 'designmatrix' object
  dm = designmatrix( df, slct )
  SM = subset( dm )
  # Update design matrix
  subset( dm ) = coding( dm, type = 'identity', variables = 'supp' )
  # Comparison
  SM[1:3,1] = 1; SM[4:6,2] = 1; SM[4:6,1] = 0;
  expect_equal( subset( dm ), SM )

  # Reset 'designmatrix' object
  dm = designmatrix( df, slct )
  SM = subset( dm )
  # Update design matrix
  subset( dm ) = coding( dm, type = 'dummy', variables = 'supp' )
  # Comparison
  SM[4:6,2] = 1
  expect_equal( subset( dm ), SM )

  # Reset 'designmatrix' object
  dm = designmatrix( df, slct )
  SM = subset( dm )
  # Update design matrix
  subset( dm ) = coding( dm, type = 'effect', variables = 'dose' )
  # Comparison
  SM[c(1,4),2:3] = -1;
  diag(SM[2:3,2:3]) = 1; diag( SM[5:6,2:3] ) = 1
  expect_equal( subset( dm ), SM )

  # Reset 'designmatrix' object
  dm = designmatrix( df, slct )
  SM = subset( dm )
  # Update design matrix
  subset( dm ) = coding( dm, type = 'linear', variables = 'dose' )
  # Comparison
  SM[,2] = rep( 1:3 - mean(1:3), 2 )
  expect_equal( subset( dm ), SM )

} )

# 2.4)
test_that( "location output from 'coding' is stored can be changed", {

  # Create 'designmatrix' object
  dm = designmatrix( df, slct )
  SM = subset( dm )
  # Apply dummy coding separately for both variables
  subset( dm ) = coding( dm, type = 'dummy', variables = 'supp', start = 2 )
  subset( dm ) = coding( dm, type = 'dummy', variables = 'dose', start = 3 )
  # Comparison
  SM[4:6,2] = 1; diag(SM[2:3,3:4]) = 1; diag(SM[5:6,3:4]) = 1
  expect_equal( subset( dm ), SM )

} )

# 2.5)
test_that( "reference group for effect/dummy coding can be changed", {

  # Create 'designmatrix' object
  dm = designmatrix( df, slct )
  order_1 = coding( dm, type = 'dummy', variables = 'supp' )
  order_2 = coding( dm, type = 'dummy', variables = 'supp', index = 2 )
  cmp = c( rep(-1,3), rep(1,3) )
  expect_equal( as.vector( order_1 - order_2 ), cmp )

} )

