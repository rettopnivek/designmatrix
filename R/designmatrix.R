# The 'designmatrix' function
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2019-03-13

# Table of contents
# 1) designmatrix
# 2) Methods
#   2.1) is.designmatrix
#   2.2) print.designmatrix
#   2.3) summary.designmatrix
#   2.4) plot.designmatrix
#   2.5) subset.designmatrix
#   2.6) subset<-

###
### 1)
###

#' Initialize and Update Design Matrices
#'
#' Either 1) initialize a design matrix given a data frame and
#' a list specifying the dependent variable and associated
#' grouping variables, or 2) update a design matrix after
#' modification of of the element summarizing the design matrix
#' over all possible combinations of levels for the grouping
#' variables.
#'
#' @param df A data frame with the observations for
#'   the dependent variable and associated grouping variables.
#' @param select A list with...
#'   \enumerate{
#'     \item The column name for the dependent variable.
#'     \item The set of column names for the grouping variables.
#'   }
#' @param dm An object of class \code{designmatrix}. If provided,
#'   the function will update the object based on the specifications
#'   of the \code{summary_matrix} element.
#' @param digits The number of digits to round to when
#'   computing the group means.
#'
#' @return An object of class \code{designmatrix}. The \code{print}
#'   method displays the combination of levels and associated
#'   rows of the design matrix. The \code{summary} method
#'   displays the combination of levels and their associated
#'   descriptive statistics (mean, standard deviation,
#'   standard error of the mean, and sample size). The method
#'   \code{subset} can be used to access the summary matrix
#'   (or the full design matrix if \code{summary} is \code{TRUE}),
#'   or to replace elements of the summary matrix.
#'
#'   The \code{plot} method generates a simple plot of the group
#'   means (filled circles). If elements of the design matrix are
#'   non-zero, the method fits a simple linear model to the data
#'   using the specified design matrix and shows the resulting
#'   estimates for the group means (empty circles). If the option
#'   \code{intercept} is set to \code{TRUE} (the default), the method
#'   assumes the intercept term is defined in the user-submitted
#'   design matrix. The option \code{error_bars}, if \code{TRUE},
#'   adds approximate 95% uncertainty intervals with boundaries
#'   equal to +/- 2 standard errors relative to the observed means.
#'   The option \code{exclude_effects} takes a string character
#'   with the column names for the design matrix. The method
#'   then displays what the estimated means would be were these
#'   columns excluded from from the design matrix (empty blue circles).
#'   Additional plotting options for \code{\link[graphics]{plot}},
#'   excluding \code{xlab}, \code{ylab}, \code{xaxt}, \code{pch},
#'   and \code{bty}, can be supplied.
#'
#' @examples
#' # Example with R data set 'PlantGrowth'
#' dm = designmatrix( PlantGrowth, list( 'weight', 'group' ) )
#' # Summarize group means
#' summary( dm )
#'
#' # Specify design matrix
#' # Intercept
#' subset( dm )[,1] = 1
#' # Main effect of treatment 1
#' subset( dm )[1:2,2] = c(-1,1)
#' # Main effect of treatment 2
#' subset( dm )[c(1,3),3] = c(-1,1)
#'
#' # Update summaries and full design matrix
#' dm = designmatrix( dm = dm )
#'
#' # Example of methods
#' print( dm )
#' plot( dm, intercept = T, error_bars = T, exclude_effects = c( 'X2', 'X3' ) )
#'
#' # Example analysis
#' dtba = data.frame( y = PlantGrowth$weight )
#' # Extract the full design matrix
#' X = subset( dm, F )
#' # Create data to be analyzed
#' dtba = cbind( dtba, X )
#' lmf = lm( y ~ -1 + ., data = dtba )
#'
#' @export

designmatrix = function( df = NULL,
                         select = NULL,
                         dm = NULL,
                         digits = 2 ) {

  # Labels for descriptive statistics
  ds_vrb = c( 'M', 'SD', 'SEM', 'N' )

  if ( !is.null( df ) ) {

    if ( !is.null( select ) ) {

      DV = NULL
      vrb = NULL

      if ( length( select ) >= 2 ) {

        DV = select[[1]][1]
        vrb = select[[2]]

      }

      if ( length( select ) == 1 ) {
        DV = select[[1]][1]
        vrb = colnames( df )[ colnames( df ) != DV ]
      }

    } else {
      stop( paste(
        "Provide a list giving 1) the dependent variable name,",
        "and 2) the set of names for the independent variables." ),
        call. = F )
    }

    # Generic name for dependent variable
    df$DV = df[[ DV ]]

    # Summary of group means based on
    # subset of grouping variables
    sm = df %>%
      group_by_at( vars(one_of(vrb)) ) %>%
      summarize(
        M = round( mean( DV ), digits = digits ),
        SD = round( sd( DV ), digits = digits ),
        SEM = round( sd( DV )/sqrt( length( DV ) ), digits = digits ),
        N = length( DV )
      ) %>%
      as.data.frame()

    # Initialize design matrix
    DM = matrix( 0, nrow( df ), nrow( sm ) )
    colnames( DM ) = paste( 'X', 1:nrow( sm ), sep = '' )
    # Initialize summary matrix
    X = matrix( 0, nrow( sm ), nrow( sm ) )
    colnames( X ) = paste( 'X', 1:nrow( sm ), sep = '' )

    out = list(
      group_means = sm,
      summary_matrix = X,
      design_matrix = DM,
      combined = cbind( sm, X ),
      data = df,
      dv = DV,
      variables = vrb
    )
    out$combined = out$combined[ , !(colnames( out$combined ) %in% ds_vrb) ]
    class( out ) = "designmatrix"
    return( out )
  }

  if ( !is.null( dm ) ) {

    if ( is.designmatrix( dm ) ) {

      df = dm$data
      X = dm$summary_matrix
      DM = dm$design_matrix
      sm = dm$group_means
      nc = ncol( sm ) - 4
      vrb = colnames( sm )[1:nc]

      for ( i in 1:nrow( sm ) ) {
        sel = logical( nrow( df ) )
        for ( j in 1:nrow( df ) ) {
          sel[j] = all( df[j,vrb] == sm[i,vrb] )
        }
        DM[sel,] = matrix( X[i,], sum(sel), ncol( X ), byrow = T )

      }
      colnames( DM ) = colnames( X )

      dm$design_matrix = DM
      dm$combined = cbind( sm, X )
      dm$combined = dm$combined[ , !(colnames( dm$combined ) %in% ds_vrb) ]

      return( dm )

    } else {
      stop( "Input must be a 'designmatrix' object.",
            call. = F )
    }

  }

}

###
### 2) Methods
###

# 2.1)
#' @rdname designmatrix
#' @export

is.designmatrix = function( x )
  inherits( x, "designmatrix" )

# 2.2)
#' @rdname designmatrix
#' @export

print.designmatrix = function( x ) {
  print( x$combined )
}

# 2.3)
#' @rdname designmatrix
#' @export

summary.designmatrix = function( x ) {
  print( x$group_means )
}

# 2.4)
#' @rdname designmatrix
#' @export

plot.designmatrix = function( x,
                              intercept = T,
                              exclude_effects = NULL,
                              error_bars = F,
                              ... ) {

  df = x$data
  DM = x$design_matrix
  sel = apply( DM, 2, function(x) any( x != 0 ) )

  if ( any( sel ) ) {

    dtba = data.frame(
      y = df[[ x$dv ]]
    )

    cur_DM = as.matrix( DM[,sel] )
    colnames( cur_DM ) = colnames( DM )[sel]
    nd = x$combined
    nd_exclude = NULL
    if ( !is.null( exclude_effects ) ) {
      nd_exclude = nd
      nd_exclude[, colnames(nd) %in% exclude_effects ] = 0
    }

    dtba = cbind( dtba, cur_DM )

    if ( !intercept ) {
      lmf = lm( y ~ ., data = dtba )
    } else {
      lmf = lm( y ~ -1 + ., data = dtba )
    }
    prd = predict( lmf, newdata = nd )
    prd_exclude = NULL
    if ( !is.null( exclude_effects ) ) {
      prd_exclude = predict( lmf, newdata = nd_exclude )
    }
  }

  if ( error_bars ) {
    yl = range( c( x$group_means$M - 2*x$group_means$SEM,
                   x$group_means$M + 2*x$group_means$SEM ) )

    plot( 1:nrow( x$combined ),
          x$group_means$M,
          pch = 19,
          xlab = 'Conditions',
          ylab = 'Group means',
          bty = 'n',
          xaxt = 'n',
          ylim = yl,
          ...
    )
    axis( 1, 1:nrow( x$combined ) )

    segments( 1:nrow( x$group_means ),
              x$group_means$M - 2*x$group_means$SEM,
              1:nrow( x$group_means ),
              x$group_means$M + 2*x$group_means$SEM
    )

  } else {

    plot( 1:nrow( x$combined ),
          x$group_means$M,
          pch = 19,
          xlab = 'Conditions',
          ylab = 'Group means',
          bty = 'n',
          xaxt = 'n',
          ...
    )
    axis( 1, 1:nrow( x$combined ) )
  }

  if ( any( sel ) ) {

    points( 1:length( prd ),
            prd,
            pch = 21,
            bg = NA,
            cex = 2 )

    if ( !is.null( prd_exclude ) ) {
      points( 1:length( prd_exclude ),
              prd_exclude,
              pch = 21,
              col = 'blue',
              bg = NA,
              cex = 2 )
    }


  }

}

# 2.5)
#' @rdname designmatrix
#' @export

subset.designmatrix = function( x, summary = T ) {

  if ( summary ) {
    # Return the summary matrix
    return( x$summary_matrix )

  } else {
    # Return the full design matrix
    return( x$design_matrix )

  }
}

# 2.6)
#' @rdname designmatrix
#' @export

`subset<-` = function(x, value) {
  x$summary_matrix <- value
  x
}

