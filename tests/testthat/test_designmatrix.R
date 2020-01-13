# Tests for 'designmatrix' function
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2019-03-15

# Table of contents
# 1) Initial setup
# 2) Tests for creating 'designmatrix' objects
#   2.1) default example of 'designmatrix' works
#   2.2) 'designmatrix' correctly stops if a data frame isn't provided
#   2.3) 'designmatrix' correctly stops if not enough inputs are provided
#   2.4) 'designmatrix' can take a single list element with DV
#   2.5) 'designmatrix' can take a single character vector for the DV
#   2.6) 'designmatrix' correctly stops if variables aren't in data frame
#   2.7) 'designmatrix' correctly stops if data frame has only one column
# 3) Tests for whether 'designmatrix' creates correct output
#   3.1) 'designmatrix' adjusts DV name if it conflicts with desc. stats
#   3.2) 'designmatrix' adjusts groupng names if they conflict with desc. stats
#   3.3) 'designmatrix' returns correct group means
#   3.4) 'designmatrix' returns correct default summary matrix
#   3.5) 'designmatrix' returns correct default marginal means algebra
# 4) Tests for whether 'designmatrix' updates correctly
#   4.1) subset method extracts summary matrix
#   4.2) subset<- method allows for updating summary matrix
#   4.3) 'designmatrix' can properly update other elements

###
### 1) Initial setup
###

# Specify context
context("designmatrix function")

# library( testthat)

# Load in package
library( designmatrix )

###
### 2) Tests for creating 'designmatrix' objects
###

# Define example data set
df = PlantGrowth
slct = list( 'weight', 'group' )

# 2.1)
test_that( "default example of 'designmatrix' works", {
  dm = designmatrix( df, slct )
  expect_s3_class( dm, "designmatrix" )
} )

# 2.2)
test_that( "'designmatrix' correctly stops if a data frame isn't provided", {
  expect_error( designmatrix( df = slct, select = slct ) )
  expect_error( designmatrix( df = list( A = 1, B = 2 ), select = slct ) )
} )

# 2.3)
test_that( "'designmatrix' correctly stops if not enough inputs are provided", {
  expect_error( designmatrix( df ) )
} )

# 2.4)
test_that( "'designmatrix' can take a single list element with DV", {
  vrb = colnames( df )[2]
  dm = designmatrix( df, list( 'weight' ) )
  expect_equal( dm$variables, vrb )
} )

# 2.5)
test_that( "'designmatrix' can take a single character vector for the DV", {
  vrb = colnames( df )[2]
  dm = designmatrix( df, list( 'weight' ) )
  expect_equal( dm$variables, vrb )
} )

# 2.6)
test_that( "'designmatrix' correctly stops if variables aren't in data frame", {
  expect_error( designmatrix( df, list( 'A', 'B' ) ) )
  expect_error( designmatrix( df, list( 'weight', 'B' ) ) )
} )

# 2.7)
test_that( "'designmatrix' correctly stops if data frame has only one column", {
  expect_error( designmatrix( data.frame( weight = df$weight ), slct ) )
} )

###
### 3) Tests for whether 'designmatrix' creates correct output
###

# 3.1)
test_that( "'designmatrix' adjusts DV name if it conflicts with desc. stats", {

  df2 = df
  colnames( df2 ) = c( 'N', 'group' )
  slct = list( 'N', 'group' )
  dm = designmatrix( df2, slct )
  DV = paste( slct[[1]], '2', sep = '.' )

  expect_warning( designmatrix( df2, slct ) )
  expect_equal( dm$dv, DV )

  colnames( df2 ) = c( 'DV', 'group' )
  slct = list( 'DV', 'group' )
  dm = designmatrix( df2, slct )
  DV = paste( slct[[1]], '2', sep = '.' )
  expect_warning( designmatrix( df2, slct ) )
  expect_equal( dm$dv, DV )
} )

# 3.2)
test_that( "'designmatrix' adjusts grouping names if they conflict with desc. stats", {

  df2 = df
  colnames( df2 ) = c( 'weight', 'M' )
  slct = list( 'weight', 'M' )
  dm = designmatrix( df2, slct )
  vrb = paste( slct[[2]], '2', sep = '.' )
  expect_warning( designmatrix( df2, slct ) )
  expect_equal( dm$variables, vrb )

  colnames( df2 ) = c( 'weight', 'DV' )
  slct = list( 'weight', 'DV' )
  dm = designmatrix( df2, slct )
  vrb = paste( slct[[2]], '2', sep = '.' )
  expect_warning( designmatrix( df2, slct ) )
  expect_equal( dm$variables, vrb )

} )

# 3.3)
test_that( "'designmatrix' returns correct group means", {

  gm = df %>%
    group_by( group ) %>%
    summarise(
      M = round( mean( weight ), 2 ),
      SD = round( sd( weight ), 2 ),
      SEM = round( sd( weight )/sqrt( length( weight ) ), 2 ),
      N = length( weight )
    ) %>%
    as.data.frame()
  slct = list( 'weight', 'group' )
  dm = designmatrix( df, slct )
  expect_equal( dm$group_means, gm )
} )

# 3.4)
test_that( "'designmatrix' returns correct default summary matrix", {

  sm = matrix( 0, 3, 3 )
  colnames( sm ) = paste( 'X', 1:3, sep = '' )
  sm[,1] = 1
  slct = list( 'weight', 'group' )
  dm = designmatrix( df, slct )
  expect_equal( dm$summary_matrix, sm )
} )

# 3.5)
test_that( "'designmatrix' returns correct default marginal means algebra", {

  df = PlantGrowth
  slct = list( 'weight', 'group' )
  dm = designmatrix( df, slct )

  sm = dm$summary_matrix
  gm = dm$group_means

  mm = matrix( " ", nrow( sm ), ncol( sm ) )
  colnames( mm ) = colnames( sm )
  mm[,1] = 'B0'

  mm = cbind( as.character( gm[,dm$variables] ),
              mm )
  colnames( mm )[1] = dm$variables
  mm = rbind( mm, rep( '-', ncol(mm) ), rep( " ", ncol(mm) ) )
  mm[5,1] = 'Avg.'
  mm[5,2] = 'B0'
  mm = data.frame( mm, stringsAsFactors = F )
  expect_equal( dm$algebra_group_means, mm )
} )

###
### 4) Tests for whether 'designmatrix' updates correctly
###

df = PlantGrowth
slct = list( 'weight', 'group' )

# 4.1)
test_that( "subset method extracts summary matrix", {

  dm = designmatrix( df, slct )

  sm = matrix( 0, 3, 3 )
  colnames( sm ) = paste( 'X', 1:3, sep = '' )
  sm[,1] = 1

  expect_equal( subset( dm ), sm )
} )

# 4.2)
test_that( "subset<- method allows for updating summary matrix", {

  dm = designmatrix( df, slct )

  sm = matrix( 0, 3, 3 )
  colnames( sm ) = paste( 'X', 1:3, sep = '' )
  sm[,1] = 1
  sm[2,2] = 1
  sm[3,3] = 1

  subset( dm )[2,2] = 1
  subset( dm )[3,3] = 1

  expect_equal( subset( dm ), sm )
} )

# 4.3)
test_that( "'designmatrix' can properly update other elements", {

  # Initialize 'designmatrix' object
  dm = designmatrix( df, slct )

  # Expected updates
  cmb = dm$combined
  cmb[2,3] = 1
  cmb[3,4] = 1

  DM = dm$design_matrix
  DM[ dm$data$group == 'trt1', 2 ] = 1
  DM[ dm$data$group == 'trt2', 3 ] = 1

  mm = algebra( dm )
  mm[2,3] = '+ B1'
  mm[3,4] = '+ B2'
  mm[5,3] = '+ (0.3) B1'
  mm[5,4] = '+ (0.3) B2'

  subset( dm )[2,2] = 1
  subset( dm )[3,3] = 1
  dm = designmatrix( dm )

  expect_equal( dm$design_matrix, DM )
  expect_equal( dm$combined, cmb )
  expect_equal( algebra( dm ), mm )
} )

