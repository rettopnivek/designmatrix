# The 'getdata' function
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2019-03-15

# Table of contents
# 1) getdata

###
### 1)
###

#' Combined Data and Specified Design Matrices
#'
#' Extracts the dependent variable and full design
#' matrix from an object of class \code{designmatrix} and
#' combined them into a single data frame.
#'
#' @param dm An object of class \code{designmatrix}.
#'
#' @return A data frame with a column for the dependent variable
#'   (labeled \code{DV}), and a remaining set of columns for the
#'   full design matrix extracted from the \code{designmatrix}
#'   object.
#'
#' @examples
#' # Example with R data set 'PlantGrowth'
#' dm = designmatrix( PlantGrowth, list( 'weight', 'group' ) )
#' df = getdata( dm )
#'
#' @export

getdata = function( dm ) {

  if ( is.designmatrix( dm ) ) {
    out = data.frame(
      DV = dm$data$DV
    )
    out = cbind( out, subset( dm, F ) )

    return( out )
  } else {
    stop( paste(
      "Input must be of class 'designmatrix'"
    ), call. = F )
  }

}

