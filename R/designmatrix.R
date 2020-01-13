# The 'designmatrix' function
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2019-03-15

# Table of contents
# 1) Internal functions
#   1.1) marginal_means_algebra
# 2) designmatrix
# 3) Methods
#   3.1) is.designmatrix
#   3.2) print.designmatrix
#   3.3) summary.designmatrix
#   3.4) plot.designmatrix
#   3.5) subset.designmatrix
#   3.6) subset<-
#   3.7) data.frame.designmatrix

###
### 1) Internal functions
###

# 1.1)
marginal_means_algebra = function( dm ) {
  # Purpose:
  # Creates a table showing the algebra used
  # to compute the marginal group means based
  # on a specified design matrix.
  # Arguments:
  # dm - An object of class 'designmatrix'
  # Returns:
  # A data frame.

  # Extract summary matrix
  sm = dm$summary_matrix
  # Define total number of possible regression
  # coefficients
  cf = paste( 'B', 0:( ncol( sm ) - 1 ), sep = '' )

  # Initialize matrix showing algebra for
  # marginal group means
  mm = matrix( " ", nrow(sm), ncol(sm) )
  colnames( mm ) = colnames( sm )

  # Loop over rows
  for ( i in 1:nrow( mm ) ) {

    # Identify coefficients to exclude
    sel = sm[i,] == 0
    # Create display for weights/coefficient values
    # (round to one decimal place)
    val = paste( '(',round(sm[i,],1),')', sep = '' )
    # Add coefficient label
    res = paste( val, cf )
    # Specify additive operator when appropriate
    sel2 = cumsum( !sel ) > 1
    if ( any( sel2 ) ) {
      res[sel2] = paste( '+', res[sel2] )
    }
    mm[i,!sel] = res[!sel]

    # Remove weights of 1
    mm[i,] = gsub( '(1) ', '', mm[i,], fixed = T )
    # Remove weights of -1 and update operator
    mm[i,] = gsub( '+ (-1) ', '- ', mm[i,], fixed = T )

  }
  # Convert to data frame
  mm = data.frame( mm, stringsAsFactors = F )


  # Determine labels and weights for overall average
  avg = colSums( sm )/nrow( sm )
  # Identify coefficients to exclude
  sel = avg == 0
  # Round to one decimal place
  avg = round( avg, 1 )
  # Create display for weights/coefficient values
  avg = paste( '(', avg, ')', sep = '' )
  # Add coefficient label
  fin_avg = paste( avg, cf )
  # Specify additive operator when appropriate
  sel2 = cumsum( !sel ) > 1
  if ( any( sel2 ) ) {
    fin_avg[sel2] = paste( '+', fin_avg[sel2] )
  }
  fin_avg[sel] = " "

  # Remove weights of 1
  fin_avg = gsub( '(1) ', '', fin_avg, fixed = T )
  # Remove weights of -1 and update operator
  fin_avg = gsub( '+ (-1) ', '- ', fin_avg, fixed = T )

  # Add grouping variables
  mm = cbind( dm$group_means[,dm$variables],
              mm )
  # Convert everything to character strings
  for ( i in 1:ncol(mm) ) mm[[i]] = as.character( mm[[i]] )
  # Add overall average
  tmp = character( length( dm$variables ) )
  tmp[ length( tmp ) ] = 'Avg.'
  tmp = c( tmp, fin_avg )
  names( tmp ) = colnames( mm )
  # Add separator for overall average
  tmp2 = rep( '-', ncol( mm ) )
  names( tmp2 ) = colnames( mm )
  mm = rbind( mm, tmp2, tmp )

  if ( length( dm$variables ) == 1 )
    colnames( mm )[1] = dm$variables[1]

  return( mm )
}

###
### 2)
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
#'   If only the column name for the dependent variable is provided,
#'   the function assumes all remaining columns are the set of
#'   grouping variables.
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
#' # Main effect of treatment 1
#' subset( dm )[1:2,2] = c(-1,1)
#' # Main effect of treatment 2
#' subset( dm )[c(1,3),3] = c(-1,1)
#'
#' # Update summaries and full design matrix
#' dm = designmatrix( dm )
#'
#' # Example of methods
#' print( dm )
#' plot( dm, intercept = T, error_bars = T, exclude_effects = c( 'X2', 'X3' ) )
#'
#' # Example analysis
#' dtba = getdata( dm )
#' lmf = lm( DV ~ -1 + ., data = dtba )
#'
#' @export

designmatrix = function( df = NULL,
                         select = NULL,
                         dm = NULL,
                         digits = 2 ) {

  # Labels for descriptive statistics
  ds_vrb = c( 'M', 'SD', 'SEM', 'N' )

  # Check if 'df' input is provided
  if ( !is.null( df ) ) { # Open (1)

    # If is of class 'designmatrix' proceed to next segment
    if ( is.designmatrix( df ) ) {

      dm = df

    } else { # Open (2)

      # Check input is a data frame
      if ( !is.data.frame( df ) ) {
        stop( paste(
          "Either need 1) a data frame of conditions and observations",
          "(and a list with at least the column name for the dependent",
          "variable), or 2) an object of class 'designmatrix'"
        ), call. = FALSE )

      }

      # Define error message to return for issues related
      # to the 'select' input
      err_msg = function( type = 1 ) {

        if ( type == 1 ) {
          stop( paste(
            "Provide a list giving 1) the dependent variable name,",
            "and 2) the set of names for the grouping variables" ),
            call. = F )
        }
        if ( type == 2 ) {
          warning( paste(
            "Only first element in character string was used",
            "as column name for dependent variable"
          ), call. = FALSE )
        }
        if ( type == 3 ) {
          stop( paste(
            "At least some provided column names are",
            "not present in data frame"
          ), call. = FALSE )
        }
      }

      # Initialize variables
      DV = NULL
      vrb = NULL

      # If no 'select' input is provided
      if ( is.null( select ) ) {
        err_msg()
      } else { # Open (3)

        # If a character string is provided
        # convert it to a list
        if ( is.character( select ) ) {
          if ( length( select ) == 1 ) {
            select = list( select )
          } else {
            select = list( select[1] )
            err_msg( type = 2 )
          }
        }

        # If 'select' input is not a list
        if ( !is.list( select ) ) {
          err_msg()
        } else { # Open (4)

          # If excess list elements are provided
          if ( length( select ) > 2 ) {
            warning( paste(
              "Excess list elements provided when",
              "specifying dependent and grouping",
              "variables"
            ), call. = FALSE )
          }

          # Extract dependent variable
          DV = select[[1]][1]
          if ( length( select[[1]] ) > 1 ) {
            err_msg( type = 2 )
          }

          cn = colnames( df )

          # If no other elements provided in data frame
          if ( length( cn ) == 1 ) {
            stop( paste(
              "Data frame should have both a dependent",
              "variable and at least one grouping variable"
            ), call. = FALSE )
          }

          # If provided name is not in column names
          if ( !( DV %in% cn ) ) {
            err_msg( type = 3 )
          } else { # Open (5)

            # If only one element, assume
            # remaining columns are grouping variables
            if ( length( select ) == 1 ) {
              vrb = cn[ cn != DV ]
            } else {
              vrb = select[[2]]
            }

            # If grouping variables are not in data frame
            if ( !all( vrb %in% cn ) ) {
              err_msg( type = 3 )
            }

          } # Close (5)

        } # Close (4)

        # Final check to make sure a dependent variable
        # and set of grouping variables were extracted
        if ( is.null( DV ) | is.null( vrb ) ) {
          err_msg()
        }

      } # Close (3)

      # Check if any of the selected columns have
      # names that overlap with the descriptive statistics
      comp = c( ds_vrb, 'DV' )

      # For dependent variable
      if ( DV %in% comp ) {

        orig_DV = DV
        sel = colnames( df ) %in% DV
        colnames( df )[sel] = paste( DV, '2', sep = '.' )
        DV = paste( DV, '2', sep = '.' )

        warning( paste(
          "Changed", orig_DV, "to", DV,
          "to avoid conflict with labels for descriptive statistics"
        ), call. = FALSE )

      }

      # For grouping variables
      if ( any( vrb %in% comp ) ) {

        orig_vrb = vrb
        sel1 = vrb %in% comp
        sel2 = colnames( df ) %in% vrb[sel1]
        colnames( df )[sel2] = paste( vrb[sel1], '2', sep = '.' )
        vrb[sel1] = paste( vrb[sel1], '2', sep = '.' )

        if ( sum(sel1) > 1 ) {

          string = c(
            paste( '{', paste( orig_vrb[sel1], collapse = ', ' ), '}', sep = '' ),
            paste( '{', paste( vrb[sel1], collapse = ', ' ), '}', sep = '' )
          )

          warning( paste(
            "Changed", string[1], "to", string[2],
            "to avoid conflict with labels for descriptive statistics"
          ), call. = FALSE )
        } else {
          warning( paste(
            "Changed", orig_vrb[sel1], "to", vrb[sel1],
            "to avoid conflict with labels for descriptive statistics"
          ), call. = FALSE )
        }

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
      DM[,1] = 1 # By default, set first column as an intercept
      colnames( DM ) = paste( 'X', 1:nrow( sm ), sep = '' )
      # Initialize summary matrix
      X = matrix( 0, nrow( sm ), nrow( sm ) )
      X[,1] = 1 # By default, set first column as an intercept
      colnames( X ) = paste( 'X', 1:nrow( sm ), sep = '' )

      out = list(
        group_means = sm,
        summary_matrix = X,
        design_matrix = DM,
        combined = cbind( sm, X ),
        data = df,
        dv = DV,
        variables = vrb,
        algebra_group_means = NULL
      )
      out$combined = out$combined[ , !(colnames( out$combined ) %in% ds_vrb) ]
      # Determine algebra for marginal means
      out$algebra_group_means =
        marginal_means_algebra( out )
      class( out ) = "designmatrix"

      return( out )

    } # Close (2)

  } # Close (1)

  # If a object of class 'designmatrix' is provided,
  # update based on summary matrix
  if ( !is.null( dm ) ) { # Open (1)

    if ( is.designmatrix( dm ) ) { # Open (2)

      df = dm$data
      X = dm$summary_matrix
      DM = dm$design_matrix
      sm = dm$group_means
      nc = ncol( sm ) - length( ds_vrb )
      vrb = colnames( sm )[1:nc]

      nrow_sm = nrow( sm )
      nrow_df = nrow( df )
      n_vrb = length( vrb )
      log_val = rep( F, n_vrb )

      sm = as.list( sm )
      df = as.list( df )

      # Loop over rows of summary matrix
      for ( i in 1:nrow_sm ) {

        sel = logical( nrow_df )

        # Loop over rows for data
        for ( j in 1:nrow_df ) {

          # Loop over subset of columns
          for ( k in 1:n_vrb ) {
            # Identify which row of summary matrix current
            # column matches
            log_val[k] = df[[ vrb[k] ]][j] == sm[[ vrb[k] ]][i]
          }
          sel[j] = all( log_val )

        }
        DM[sel,] = matrix( X[i,], sum(sel), ncol( X ), byrow = T )

      }
      colnames( DM ) = colnames( X )

      # Convert back to data frames
      sm = data.frame( sm, stringsAsFactors = F )
      df = data.frame( df, stringsAsFactors = F )

      dm$design_matrix = DM
      dm$combined = cbind( sm, X )
      dm$combined = dm$combined[ , !(colnames( dm$combined ) %in% ds_vrb) ]
      dm$algebra_group_means =
        marginal_means_algebra( dm )

      return( dm )

    } else {
      stop( "Input must be a 'designmatrix' object.",
            call. = F )
    } # Close (2)

  } # Close (1)
}

###
### 3) Methods
###

# 3.1)
#' @rdname designmatrix
#' @export

is.designmatrix = function( x )
  inherits( x, "designmatrix" )

# 3.2)
#' @rdname designmatrix
#' @export

print.designmatrix = function( x ) {
  print( x$combined )
}

# 3.3)
#' @rdname designmatrix
#' @export

summary.designmatrix = function( x ) {
  return( x$group_means )
}

# 3.4)
#' @rdname designmatrix
#' @export

plot.designmatrix = function( x,
                              intercept = T,
                              exclude_effects = NULL,
                              error_bars = F,
                              average = T,
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

  if ( average ) {
    abline( h = mean( x$group_means$M ), lty = 2 )
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

  # Extract plotting dimensions
  plt_dm = par('usr')

  # Add legend
  legend( plt_dm[1],
          plt_dm[4] + diff( plt_dm[3:4] )*.1,
          c( 'Observed',
             'Predicted',
             'Predicted - excluded'
          ),
          pch = c( 19, 21, 21 ),
          col = c( 'black',
                   'black',
                   'blue' ),
          horiz = T,
          bty = 'n',
          xpd = T
  )

  # legend(
  # )

}

# 3.5)
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

# 3.6)
#' @rdname designmatrix
#' @export

`subset<-` = function( x, value, update = F ) {

  if ( !is.designmatrix( x ) ) {
    err_msg = paste( "Object being subsetted",
                     "must be of class",
                     "'designmatrix'" )
    stop( err_msg, call. = F )
  }

  if ( !is.matrix( value ) ) {
    err_msg = paste( "Input must be a matrix" )
    stop( err_msg, call. = F )
  }

  sm.nr = nrow( x$summary_matrix )
  sm.nc = ncol( x$summary_matrix )

  nr = nrow( value )
  nc = ncol( value )

  if ( nr != sm.nr ) {
    err_msg = paste( "Input must have same number of rows" )
    stop( err_msg, call. = F )
  }

  if ( nc > sm.nc ) {
    err_msg = paste( "Input cannot have more columns" )
    stop( err_msg, call. = F )
  }

  if ( nc == sm.nc ) {
    x$summary_matrix <- value
    if ( update ) x = designmatrix( x )
    return(x)
  }

  if ( nc < sm.nc ) {

    cn = colnames( value )
    sm.cn = colnames( x$summary_matrix )

    if ( all( cn %in% sm.cn ) ) {
      x$summary_matrix[,cn] <- value
      if ( update ) x = designmatrix( x )
      return(x)
    } else {
      err_msg = paste( "Column names must match" )
      stop( err_msg, call. = F )
    }

  }

}

