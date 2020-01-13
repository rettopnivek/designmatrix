# Tests for 'design_col' function
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2019-03-16

# Table of contents
# 1) Initial setup
# 2) Tests for 'design_col' function
#   2.1) 'design_col' correctly extracts column names
#   2.2) 'design_col' correctly changes column names

###
### 1) Initial setup
###

# Specify context
context("designmatrix function")

# library( testthat)

# Load in package
library( designmatrix )

# Example data
df = PlantGrowth
slct = list( 'weight', 'group' )

###
### 2) Tests for 'design_col' function
###

# 2.1)
test_that( "'design_col' correctly extracts column names", {
  dm = designmatrix( df, slct )
  val = c( 'X1', 'X2', 'X3' )
  expect_equal( design_col( dm ), val )
} )

# 2.2)
test_that( "'design_col' correctly changes column names", {
  dm = designmatrix( df, slct )
  val = c( 'Conrol', 'Treat.1', 'Treat.2' )
  design_col( dm ) = val
  expect_equal( colnames( dm$summary_matrix ), val )
} )

