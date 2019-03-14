# The 'designmatrix_gui' function
# Written by Kevin Potter
# email: kevin.w.potter@gmail.com
# Please email me directly if you
# have any questions or comments
# Last updated 2019-03-13

# Table of contents
# 1) designmatrix_gui

###
### 1)
###

#' Graphical Interface for Specifying Design Matrices
#'
#' Loads a graphical interface (via Shiny) to specify
#' and adjust a design matrix given a data set with
#' categorical predictors.
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
#' @details The app displays four tabs:
#' \describe{
#'   \item{Conditions}{Summary of conditions and their associated
#'     descriptive statistcs.}
#'   \item{Design matrix}{Matrix of inputs to allow users to
#'     customize the design matrix. Clicking the 'Update' button
#'     updates the design matrix across the other tabs.}
#'   \item{Group means}{Plot of the observed group means against
#'     the predicted values. Columns of the design matrix can be
#'     excluded to examine the impact of different effects.}
#'   \item{Coefficients}{Table showing how coefficients are
#'     combined to estimate the group means.}
#' }
#' Once the user has finished customizing the design matrix,
#' clicking the 'Save' button will stop the app and return
#' the updated \code{\link{designmatrix}} object.
#'
#' @return An object of class \code{\link{designmatrix}}.
#'
#' @examples
#' # Use 'crabs' data set from 'MASS' package
#' dm = designmatrix_gui( MASS::crabs, list( 'FL', c( 'sp', 'sex' ) ) )
#'
#' @export

designmatrix_gui = function( df = NULL, select = NULL, dm = NULL, digits = 2 ) {

  # Initialize design matrix structures
  if ( is.null( dm ) ) {

    if ( is.null( df ) | is.null( select) ) {
      err_message = paste( 'Function needs a data frame of',
                           'observations and a list with',
                           'the column name of the dependent',
                           'variable and a vector of',
                           'the categorical predictor names.' )

      stop( err_message, call. = F )
    }

    dm = designmatrix( df = df, select = select, digits = digits )
  }

  # Define user interface
  ui = fluidPage(

    title = "Design Matrix Specification",

    # Add row for main content
    fluidRow(
      # Multiple tabs for different types of output
      tabsetPanel( type = "tabs",
                   # Summary of conditions
                   tabPanel( "Conditions", tableOutput("cond") ),
                   # Panel for adjusting design matrix
                   tabPanel( "Design matrix", tableOutput("sm_cells") ),
                   # Panel for plotting group means and predictions
                   tabPanel( "Group means", plotOutput("gm") ),
                   # Panel for coefficient combinations
                   tabPanel( "Coefficients", tableOutput("coef") )
      )
    ),

    # Add row for inputs
    fluidRow(
      sidebarPanel(
        # Button to update design matrix
        actionButton("goButton", "Update design matrix")
      ),
      mainPanel(
        # Selection wheel for variables to exclude
        selectInput(
          "vrb_to_exclude",
          label = "Variables to exclude",
          choices = colnames( dm$summary_matrix ),
          multiple = T
        ),

        # Button to stop app and return
        # results
        actionButton( "save_res", "Save")

      )
    )

  )

  # Define
  server = function(input, output) {

    # Extract summary matrix
    x = dm$summary_matrix

    # Conditions
    output$cond = renderTable({
      tbl = dm$group_means
      tbl = cbind( Rows = paste( 'Condition', 1:nrow( tbl ) ), tbl )
      tbl
    })

    # Create matrix of numeric inputs for adjusting matrix
    nr = nrow(x) # Number of rows
    nc = ncol(x) # Number of inputs
    # Initialize matrix
    mat = matrix( '', nr, nc )
    # Insert html code directly
    for ( i in 1:nc ) {

      val = 0; if ( i == 1 ) val = 1
      mat[,i] = paste0( "<input id='c", i, "r",
                        1:nr,
                        "' class='shiny-bound-input' type='number', value='",
                        val,
                        "'>" )
    }
    # Relabel columns
    colnames( mat ) = colnames( x )
    # Relabel rows
    rownames( mat ) = paste0( 'Condition ', 1:nr )
    mat = as.data.frame( mat )
    # Render cells
    output$sm_cells = renderTable(
      mat,
      sanitize.text.function = function(x) { x },
      rownames = T
    )
    # Define data frame to access elements
    mat_inp = data.frame(
      row = rep( 1:nr, each = nc ),
      col = rep( 1:nc, nr ),
      label = ' ',
      stringsAsFactors = F
    )
    mat_inp$label = inp = paste0( 'c', mat_inp$col, 'r', mat_inp$row )

    # Create a reactive expression that updates the
    # design matrix based on changes to the matrix
    # of inputs after a user clicks the 'Update'
    # button
    cur_dm = eventReactive( input$goButton, {

      new_sm = matrix( 0, nr, nc )
      for ( i in 1:nrow( mat_inp ) ) {
        new_sm[ mat_inp$row[i], mat_inp$col[i] ] =
          as.numeric( input[[ mat_inp$label[i] ]] )
      }
      colnames( new_sm ) = colnames( x )
      rownames( new_sm ) = rownames( x )
      new_dm = dm
      new_dm$summary_matrix = new_sm
      new_dm = designmatrix( dm = new_dm )
      new_dm

    })

    # Generates a plot of the group means
    # and the predicted means based on
    # the design matrix
    output$gm = renderPlot({

      vte = as.character( input$vrb_to_exclude )

      if ( length( vte ) == 0 ) {
        vte = NULL
      }

      plot( cur_dm(), error_bars = T, exclude_effects = vte )
    })

    # Generates a table showing how the marginal
    # means are computed based on the
    # regression coefficients
    output$coef = renderTable({

      sm = cur_dm()$summary_matrix
      cf = paste( 'B', 0:( ncol(sm) - 1 ), sep = '' )
      tbl = matrix( " ", nrow( sm ), ncol( sm ) )
      for ( i in 1:nrow( sm ) ) {
        for ( j in 1:nrow( sm ) ) {

          if ( sm[i,j] != 0 ) {
            val = paste( '(', round( sm[i,j], 1 ), ')',
                         cf[j], sep = '' )
            if ( j > 1 ) {
              if ( any( sm[i,-(j:ncol(sm))] != 0 ) ) {
                val = paste( '+', val )
              }
            }
            tbl[i,j] = val
          }
        }
      }
      tbl = data.frame( tbl, stringsAsFactors = F )
      rownames( tbl ) = paste( 'Condition', 1:nrow( sm ) )
      tbl

    })

    observe({

      if ( input$save_res > 0 ) {
        stopApp( cur_dm() )
      }
    })

  }

  out = runApp( list( ui = ui, server = server) )
  return( out )
}

