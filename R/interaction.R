# The 'interaction' function
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2019-03-16

# Table of contents
# 1) interaction

###
### 1)
###

#' Forthcoming
#'
#' Forthcoming
#'
#' @param dm Forthcoming.
#' @param V1 Forthcoming.
#' @param V2 Forthcoming.
#' @param start Forthcoming.
#'
#' @return Forthcoming.
#'
#' @examples
#' # Example with R data set 'PlantGrowth'
#' dm = designmatrix( ToothGrowth, 'len' )
#'
#' # Implement effect coding
#' subset( dm, T ) = coding( dm, type = 'EC', variable = 'supp', start = 2 )
#' subset( dm, T ) = coding( dm, type = 'EC', variable = 'dose', start = 3 )
#' subset( dm, T ) = interaction( dm, 'X2', c('X3','X4'), start = 5 )
#' print( dm )
#'
#' @export

interaction = function( dm, V1, V2, start = NULL ) {

  # Extract details
  clm = design_col( dm )
  sm = dm$summary_matrix

  if ( !is.null( start ) ) {
    sel = 1:length( clm ) >= start
  } else {
    sel = apply( sm, 2, function(x) all( x == 0 ) )
  }

  # Number of interaction terms to compute
  N_combo = length(V1) * length(V2)

  # Check if number of terms is correct
  if ( N_combo <= sum(sel) ) {

    # Extract columns to replace
    sel = clm[sel]
    sel = sel[ 1:N_combo ]

    inc = 1

    # Loop over for set of variables
    for ( i in 1:length( V1 ) ) {
      # Loop over second set of variables
      for ( j in 1:length( V2 ) ) {

        if ( all( sm[,V1[i]] == 1 ) |
             all( sm[,V2[i]] == 1 ) ) {
          stop( paste(
            "Intercept cannot be included",
            "when computing interaction terms"
          ), call. = FALSE )
        }

        # Compute interaction
        val = sm[,V1[i]] * sm[,V2[j]]
        # Update summary matrix
        sm[,sel[inc]] = val

        inc = inc + 1
      }
    }

  } else {
    stop( paste(
      "More interaction terms than columns",
      " available in design matrix"
    ), call. = FALSE )
  }

  # Output
  out = as.matrix( sm[,sel] )
  if ( length(sel) == 1 ) colnames( out ) = sel
  return( out )
}

