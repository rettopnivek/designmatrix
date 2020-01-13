# The 'coding' function
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2020-01-13

# Table of contents
# 1) Internal functions
#   1.1) create_coding_key
#   1.2) unique_combos
#   1.3) check_columns
#   1.4) match_cond
#   1.5) check_index
# 2) coding_categorical

###
### 1) Internal functions
###

# 1.1)
create_coding_key = function() {
  # Purpose:
  # Generates a list with keywords for
  # different types of coding schemes.
  # Returns:
  # A list of keywords for different
  # types of coding schemes for
  # categorical predictors.

  # Define key to look up type of coding scheme to use
  coding_key = list(
    intercept = c( 'intercept',
                   'Intercept',
                   'grand mean',
                   'Grand mean',
                   'Int', 'int', '' ),
    identity = c( 'Identity',
                  'identity',
                  'I' ),
    effect_coding = c( 'effect', 'effects',
                       'Effect', 'Effects',
                       'Effect coding',
                       'Effects coding',
                       'ANOVA', 'anova',
                       'aov', 'Sum', 'sum',
                       'EC', 'AOV' ),
    dummy_coding = c( 'dummy', 'Dummy',
                      'Dummy coding',
                      'dummy coding',
                      'Treatment',
                      'treatment',
                      'treat', 'Treat',
                      'DC' ),
    linear_trend = c( 'linear',
                      'Linear',
                      'linear trend',
                      'Linear trend',
                      'Trend', 'trend',
                      'L' )
  )

  return( coding_key )
}

# 1.2)
unique_combos = function( variables, lev, gm ) {
  # Purpose:
  # Determines the unique combinations of
  # levels for the subset of grouping variables.
  # Arguments:
  # variables - A vector with the column names
  #             for the subset of grouping variables
  # lev       - A data frame with the full set of
  #             combinations over all grouping variables
  # gm        - The data frame with the descriptive
  #             statistics for the grouping variable
  #             combinations
  # Returns:
  # A list with...
  # 1) the column names for the subset of grouping variables;
  # 2) a data frame with the combination of levels for
  #    the subest of grouping variables.

  if ( is.null( variables ) ) {
    variables = colnames( lev )
  }
  if ( !( all( variables %in% colnames( lev ) ) ) ) {
    err_msg = ""
    stop( err_msg, call. = F )
  }

  # Determine unique combinations based
  # on specified variables
  cond = gm %>%
    group_by_at( variables ) %>%
    summarize(
      M = mean( M )
    )

  out = list(
    variables = variables,
    conditions = cond
  )

  return( out )
}

# 1.3)
check_columns = function( columns, type, dm.col,
                          coding_key,
                          start = NULL,
                          cond = NULL ) {
  # Purpose:
  # Either checks or creates the subset of columns in the
  # design matrix to be updated.
  # Arguments:
  # columns    - The subset of columns to consider
  #              for the design matrix
  # type       - The keyword for the type of
  #              coding scheme to employ
  # dm.col     - A vector of the column names for
  #              the design matrix
  # coding_key - A list mapping different keywords
  #              to the coding schemes
  # start      - An optional index indicating which
  #              column to start at when updating
  # cond       - Output from the 'unique_combos'
  #              function
  # Returns:
  # A vector of the column names in the design
  # matrix to update.

  if ( type %in% coding_key$intercept ) {
    # Default - select first column
    if ( is.null( columns ) ) columns = dm.col[1]
  }

  if ( type %in% coding_key$identity ) {

    if ( !is.null( start ) ) {

      ind = 1:nrow( cond ) + start
      if ( max( ind ) <= length( dm.col ) ) {
        columns = dm.col[ind]
      }

    }

    # Determine columns
    if ( is.null( columns ) ) {
      columns  = dm.col[ 1:nrow( cond ) ]
    }

    # Check that number of columns match
    # number of conditions
    if ( length( columns ) < nrow( cond ) |
         length( columns ) > nrow( cond ) ) {

      warn_message = paste( 'Number of specified columns does not',
                            'match number of conditions - selecting',
                            'initial columns up to number of conditions',
                            'instead' )
      warning( warn_message, call. = F )

      columns  = dm.col[ 1:nrow( cond ) ]

    }

  }

  if ( (type %in% coding_key$dummy_coding) |
       (type %in% coding_key$effect_coding) ) {

    if ( !is.null( start ) ) {

      ind = 1:( nrow( cond ) - 1 ) + (start-1)
      if ( max( ind ) <= length( dm.col ) ) {
        columns = dm.col[ind]
      }

    }

    # Determine columns
    if ( is.null( columns ) ) {
      columns  = dm.col[ 1:( nrow( cond ) - 1) + 1 ]
    }

    # Check that number of columns match
    # number of conditions
    if ( length( columns ) < ( nrow( cond ) - 1) |
         length( columns ) > ( nrow( cond ) - 1) ) {

      warn_message = paste( 'Number of specified columns does not',
                            'match number of conditions - selecting',
                            'second column up to number of conditions',
                            'instead' )
      warning( warn_message, call. = F )

      columns  = dm.col[ 1:( nrow( cond ) - 1 ) + 1 ]

    }

  }

  if ( type %in% coding_key$linear_trend ) {

    if ( !is.null( start ) ) {

      if ( start %in% 1:length( dm.col ) ) {
        columns = dm.col[start]
      } else {
        # WARNING
        columns = dm.col[2]
      }

      if ( length( columns ) > 1 ) {
        # WARNING
        columns = columns[1]
      }

      if ( !(columns %in% dm.col) ) {
        # WARNING
        columns = dm.col[2]
      }

    }

    if ( is.null( columns ) ) {
      columns = dm.col[2]
    }

  }

  if ( any( !( columns %in% dm.col ) ) ) {
    err_msg = paste( "Mismatch between column names for design matrix",
                     "provided by user and those found" )
    stop( err_msg, call. = F )
  }

  return( columns )
}

# 1.4)
match_cond = function( index, cond, lev, K ) {
  # Purpose:
  # Creates a logical matrix tracking the rows in
  # the design matrix that match the combinations
  # of levels for the subset of grouping variables.
  # Arguments:
  # index   - An index of the columns in the logical
  #           matrix to consider
  # cond    - Output from the 'unique_combos'
  #           function
  # lev     - A data frame with the full set of
  #           combinations over all grouping variables
  # K       - The number of columns to create.
  # Returns:
  # A logical matrix indicating which row in each
  # column of the subset of the design matrix
  # that should be updated

  # Initialize output
  out = matrix( FALSE, nrow( lev ), K )

  # Number of variables being considered
  nv = ncol( cond ) - 1
  variables = colnames( cond )[1:nv]

  # Loop over specified conditions
  inc = 1
  for ( i in index ) {
    # Initialize a matrix tracking the match between
    # specified conditions and the full set of conditions
    mtch = matrix( FALSE, nrow( lev ), nv )
    # Loop over variables
    for ( j in 1:nv ) {
      # Evaluate match
      mtch[,j] = lev[[ variables[j] ]] == unlist( cond[i,j] )
    }
    # Save matches
    sel = apply( mtch, 1, all )
    out[,inc] = sel
    inc = inc + 1
  }

  return( out )
}

# 1.5)
check_index = function( index, mtch, type, coding_key ) {
  # Purpose:
  # Creates/checks the index controlling the order/reference
  # group for effect/dummy/linear coding schemes.
  # Arguments:
  # index      - An index of the reference group or the
  #              order to assign columns for the coding
  #              schemes
  # mtch       - The output from the 'match_cond' function
  # type       - The keyword for the type of
  #              coding scheme to employ
  # coding_key - A list mapping different keywords
  #              to the coding schemes
  # Returns:
  # An index of the order to which the 'mtch'
  # logical matrix should be used to update the subset
  # of columns in the design matrix.

  if ( (type %in% coding_key$dummy_coding) |
       (type %in% coding_key$effect_coding) ) {

    if ( is.null( index ) ) {

      # If no index is provided, assume
      # first column is reference
      index = 2:ncol( mtch )

    } else {

      # If single value of index is used,
      # assume it indicates reference group
      if ( length( index ) == 1 ) {

        # Check that supplied index is
        # consistent with number of columns
        if ( index %in% 1:ncol( mtch ) ) {
          index = (1:ncol(mtch))[ -index ]
        } else {

          warn_msg = paste( "Index for reference group is",
                            "inconsistent with number of groups -",
                            "setting first group as reference",
                            "by default" )
          warning( warn_msg, call. = F )
          index = 2:ncol( mtch )

        }
      } else if ( ( length( index ) != ( ncol( mtch ) - 1 ) ) |
                  any( !( index %in% 1:ncol( mtch ) ) ) ) {

        warn_msg = paste( "Indices for non-reference groups are",
                          "inconsistent with number of groups -",
                          "setting first group as reference",
                          "by default" )
        warning( warn_msg, call. = F )
        index = 2:ncol( mtch )

      }

    }
  }

  if ( type %in% coding_key$linear_trend ) {

    if ( is.null( index ) ) {

      # If no index is provided, assume
      # sequential order
      index = 1:ncol( mtch )

    }

    if ( ( length( index ) != ( ncol( mtch ) ) ) |
         any( !( index %in% 1:ncol( mtch ) ) ) ) {
      warn_msg = paste( "Indices are inconsistent with number",
                        "of groups for linear trend -",
                        "using sequential order by default" )
      warning( warn_msg, call. = F )
      index = 1:ncol( mtch )
    }
  }

  return( index )
}

###
### 2)
###

#' Generate Common Coding Schemes for Categorical Predictors
#'
#' A convenience function for implementing common coding schemes
#' used with categorical variables (e.g., dummy/treatment coding,
#' effect/sum coding, etc.).
#'
#' @param dm An object of class \code{designmatrix}.
#' @param type A keyword (can be capitalized) indicating the type
#'   of coding scheme to apply. Currently the function implements
#'   5 types:
#'   \describe{
#'     \item{Intercept}{Generates a column of ones. Keywords
#'       include 'intercept', 'grand mean', and 'int'.}
#'     \item{Identity}{For K levels in the subset of grouping
#'       variables, generates K columns with a value of one
#'       for the kth level and zero otherwise. Keywords include
#'       'identity' or 'I'.}
#'     \item{Dummy}{For K levels in the subset of grouping
#'       variables, generates K - 1 columns. Assigns
#'       a value of one to the kth group and zero otherwise.
#'       One level is excluded as a reference group (by
#'       default the first level) with values set to 0
#'       across the K - 1 columns. Keywords include
#'       'dummy', 'treatment', and 'DC'.}
#'     \item{Effect}{For K levels in the subset of grouping
#'       variables, generates K - 1 columns. Assigns
#'       a value of one to the kth group and zero otherwise.
#'       One level is excluded as a reference group (by
#'       default the first level) with values set to -1
#'       across the K - 1 columns. Keywords include
#'       'effect', 'effects', 'sum', 'anova', 'aov', and
#'       'EC'.}
#'     \item{Linear}{For K levels in the subset of grouping
#'       variables, generates a single column with one to K
#'       even steps, adjusted to be orthogonal (i.e., sum to
#'       zero). Keywords include 'linear', 'trend', and 'L'.}
#'   }
#' @param variables The subset of grouping variables to
#'   consider when implementing the coding scheme.
#' @param columns An optional vector indicating which
#'   columns of the design matrix to update.
#' @param index An optional value/vector for changing the
#'   reference group/order when implement dummy/effect
#'   coding or linear trends. Cannot exceed the number of levels
#'   for the subset of grouping variables.
#' @param start An optional value indicating the starting
#'   column in the design matrix to begin updating.
#'
#' @details
#' \describe{
#'   \item{Dummy/Treatment coding}{For K levels, K - 1 dichotomous
#'     variables are created where each level of the subset of
#'     grouping variables is contrasted against a reference level.
#'     The intercept has a specific interpretation - the mean of the
#'     reference level. Coefficients associated with the K - 1
#'     dichotomous variables indicate the difference in means of the
#'     given level relative to the reference level. As an example,
#'     consider the data set \code{PlantGrowth}, which as a single
#'     grouping variable 'group', with three levels: 'ctrl', 'trt1',
#'     and 'trt2'. Dummy coding is useful here, setting the 'ctrl'
#'     level as the reference and creating 2 dichotomous variables
#'     to estimate the difference between 'ctrl' and 'trt1' and 'trt2'
#'     respectively.}
#'   \item{Effect/Sum coding}{For K levels, K - 1 dichotomous
#'     variables are created where each level of the subset of
#'     grouping variables is contrasted against the grand mean.
#'     One level must be specified as a reference, with a value
#'     fixed to -1 across the dichotomous variables. The intercept
#'     has a specific interpretation - the grand mean of the sample.
#'     Coefficients associated with the K - 1 dichotomous variables
#'     indicate the difference in means of the given level relative
#'     to the grand mean. The difference between the grand mean
#'     and the reference level is the negative of the sum of the
#'     coefficients. This is the typical coding scheme used in
#'     the linear model underlying analysis of variance (i.e., ANOVA).}
#'   \item{Linear trends}{If one can assume the levels of the
#'     predictor are evenly spaced (i.e., an interval variable),
#'     a linear trend can be specified. There are several ways
#'     to specify a linear trend - here, the trend is specified
#'     as to be orthogonal, by setting the values as 1 to K and then
#'     centering them (i.e., subtracting the mean). This means that
#'     the intercept can be interpreted as the grand mean.}
#' }
#'
#' @return A matrix, the subset of columns in the summary matrix
#'   in the \code{designmatrix} object. Note the \code{subset<-}
#'   method can infer which columns should be updated based on the
#'   output of the \code{coding} function.
#'
#' @examples
#'
#' # Identity coding
#' dm = designmatrix( PlantGrowth, list( 'weight', 'group' ) )
#' # Update summary matrix
#' subset( dm ) = coding( dm, type = 'I' )
#' # Update full design matrix
#' dm = designmatrix( dm ); print( dm )
#'
#' # Dummy coding
#' dm = designmatrix( PlantGrowth, list( 'weight', 'group' ) )
#' subset( dm ) = coding( dm, type = 'DC' )
#' # Update full design matrix
#' dm = designmatrix( dm ); print( dm )
#'
#' # Effect coding
#' dm = designmatrix( ToothGrowth, list( 'len', c( 'supp', 'dose' ) ) )
#' # Specify coding separately for each variable
#' subset( dm ) = coding( dm, type = 'EC', variables = 'supp' )
#' # Second row already has coding for variable 'supp'
#' subset( dm ) = coding( dm, type = 'EC', variables = 'dose', start = 3 )
#' # Update full design matrix
#' dm = designmatrix( dm ); print( dm )
#'
#' # Linear trend
#' dm = designmatrix( ToothGrowth, list( 'len', c( 'supp', 'dose' ) ) )
#' # Implement linear trend only for 'dose' variable
#' subset( dm ) = coding( dm, type = 'L', variables = 'dose' )
#' # Different coding schemes can be mixed and matched
#' subset( dm ) = coding( dm, type = 'EC', variables = 'supp', start = 3 )
#' # Update full design matrix
#' dm = designmatrix( dm ); print( dm )
#'
#' @export

coding = function( dm,
                   type = 'Intercept',
                   variables = NULL,
                   columns = NULL,
                   index = NULL,
                   start = NULL ) {

  # Initialize output
  out = NULL

  # Check that a 'designmatrix' object was provided
  if ( !is.designmatrix( dm ) ) {
    err_msg = "First input must be an object of class 'designmatrix'"
    stop( err_msg, call. = F )
  }

  # Extract conditions
  gm = dm$group_means
  ds = c( 'M', 'SD', 'SEM', 'N' )
  cn = colnames( gm )
  cn = cn[ !(cn %in% ds) ]
  lev = data.frame( gm[,!(colnames(gm) %in% ds)] )
  colnames( lev ) = cn

  # Extract summary matrix
  sm = subset( dm )
  # Extract column names for design matrix
  dm.col = colnames( sm )

  # Number of rows
  nr = nrow( lev )
  # Number of variables
  nv = ncol( lev )

  # Keywords for coding schemes
  coding_key = create_coding_key()

  ### Intercept ###

  if ( type %in% coding_key$intercept ) {

    columns = check_columns( columns, type, dm.col, coding_key )

    sm[,columns[1]] = 1
    out = as.matrix( sm[,columns[1]] )
    colnames( out ) = columns[1]
  }

  ### Identity ###

  if ( type %in% coding_key$identity ) {

    tmp = unique_combos( variables, lev, gm )
    variables = tmp$variables
    cond = tmp$conditions

    columns = check_columns( columns, type,
                             dm.col, coding_key,
                             cond = cond, start = start )

    mtch = match_cond( 1:nrow( cond ), cond, lev, length( columns ) )
    for ( i in 1:ncol( mtch ) ) {
      if ( all( sm[,columns[i]] == 1 ) ) sm[,columns[i]] = 0
      sm[mtch[,i],columns[i]] = 1
    }

    out = as.matrix( sm[,columns] )
    colnames( out ) = columns
  }

  ### Dummy coding ###
  if ( type %in% coding_key$dummy_coding ) {

    tmp = unique_combos( variables, lev, gm )
    variables = tmp$variables
    cond = tmp$conditions
    columns = check_columns( columns, type,
                             dm.col, coding_key,
                             cond = cond, start = start )

    mtch = match_cond( 1:nrow( cond ), cond, lev, length( columns ) + 1 )

    index = check_index( index, mtch, type, coding_key )
    inc = 1
    for ( i in index ) {
      sm[ mtch[,i],columns[inc] ] = 1
      inc = inc + 1
    }

    out = as.matrix( sm[,columns] )
    colnames( out ) = columns
  }

  ### Effect coding ###

  if ( type %in% coding_key$effect_coding ) {

    tmp = unique_combos( variables, lev, gm )
    variables = tmp$variables
    cond = tmp$conditions

    columns = check_columns( columns, type,
                             dm.col, coding_key,
                             cond = cond, start = start )

    mtch = match_cond( 1:nrow( cond ), cond, lev, length( columns ) + 1 )

    index = check_index( index, mtch, type, coding_key )
    inc = 1
    for ( i in index ) {
      sm[ mtch[,i],columns[inc] ] = 1
      inc = inc + 1
    }
    # Specify reference group
    sel = (1:ncol(mtch) )[ !(1:ncol( mtch ) %in% index) ]
    sm[ mtch[,sel], ] = -1

    out = as.matrix( sm[,columns] )
    colnames( out ) = columns
  }

  ### Linear trend ###

  if ( type %in% coding_key$linear_trend ) {

    tmp = unique_combos( variables, lev, gm )
    variables = tmp$variables
    cond = tmp$conditions

    # Compute linear trend
    lt = 1:nrow( cond )
    lt = lt - mean( lt )

    mtch = match_cond( 1:nrow( cond ), cond, lev, nrow( cond ) )

    index = check_index( index, mtch, type, coding_key )

    columns = check_columns( columns, type,
                             dm.col, coding_key,
                             cond = cond, start = start )

    inc = 1
    for ( i in index ) {
      sm[ mtch[,i], columns ] = lt[inc]
      inc = inc + 1
    }

    out = as.matrix( sm[,columns] )
    colnames( out ) = columns

  }

  return( out )
}

