# The 'algebra' function
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2019-03-15

# Table of contents
# 1) algebra

###
### 1)
###

#' Algebra for Predicted Marginal Group Means
#'
#' Extracts a table displaying how the predicted marginal
#' group means would be calculated based on the
#' specified design matrix and a set of regression
#' coefficients. Also displays the algebra for the
#' overall group mean averaging over all groups.
#'
#' @param dm An object of class \code{designmatrix}.
#'
#' @return A data frame giving the combinations of grouping
#'   variables and then the sums of the regression
#'   coefficients needed to predict the marginal group means
#'   based on the specified design matrix. The final row
#'   (separated by '-') shows the sums over the regression
#'   coefficients for the predicted overal average.
#'
#' @examples
#' # Example with R data set 'PlantGrowth'
#' dm = designmatrix( PlantGrowth, list( 'weight', 'group' ) )
#' df = algebra( dm )
#'
#' @export

algebra = function( dm ) {

  if ( is.designmatrix( dm ) ) {
    return( dm$algebra_group_means )
  } else {
    stop( paste(
      "Input must be of class 'designmatrix'"
    ), call. = F )
  }

}

