# The 'design_col' function
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2019-03-16

# Table of contents
# 1) design_col
# 2) design_col<-

###
### 1)
###

#' Extract/Change Column Names for a Design Matrix
#'
#' Extracts the column names for the design matrix
#' stored in a \code{designmatrix} object. Column
#' names can also be changed and updated in the
#' object.
#'
#' @param x An object of class \code{designmatrix}.
#'
#' @return The column names of the design matrix
#'   stored internally.
#'
#' @examples
#' # Example with R data set 'PlantGrowth'
#' dm = designmatrix( PlantGrowth, list( 'weight', 'group' ) )
#' design_col( dm )
#' # Change names
#' design_col( dm ) = c( 'Control', 'Treat.1', 'Treat.2' )
#' print( dm )
#'
#' @export

design_col = function( x ) {

  if ( !is.designmatrix( x ) ) {
    err_msg = paste( "Object being subsetted",
                     "must be of class",
                     "'designmatrix'" )
    stop( err_msg, call. = F )
  } else {
    return( colnames( dm$summary_matrix ) )
  }

}

###
### 2)
###

#' @rdname design_col
#' @export

`design_col<-` = function(x, value) {

  if ( !is.designmatrix( x ) ) {
    err_msg = paste( "Object being subsetted",
                     "must be of class",
                     "'designmatrix'" )
    stop( err_msg, call. = F )
  } else {
    colnames( x$summary_matrix ) = value
    x = designmatrix( x )
    return( x )
  }

}

