% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/designmatrix_gui.R
\name{designmatrix_gui}
\alias{designmatrix_gui}
\title{Graphical Interface for Specifying Design Matrices}
\usage{
designmatrix_gui(df = NULL, select = NULL, dm = NULL, digits = 2)
}
\arguments{
\item{df}{A data frame with the observations for
the dependent variable and associated grouping variables.}

\item{select}{A list with...
\enumerate{
  \item The column name for the dependent variable.
  \item The set of column names for the grouping variables.
}}

\item{dm}{An object of class \code{designmatrix}. If provided,
the function will update the object based on the specifications
of the \code{summary_matrix} element.}

\item{digits}{The number of digits to round to when
computing the group means.}
}
\value{
An object of class \code{\link{designmatrix}}.
}
\description{
Loads a graphical interface (via Shiny) to specify
and adjust a design matrix given a data set with
categorical predictors.
}
\details{
The app displays four tabs:
\describe{
  \item{Conditions}{Summary of conditions and their associated
    descriptive statistcs.}
  \item{Design matrix}{Matrix of inputs to allow users to
    customize the design matrix. Clicking the 'Update' button
    updates the design matrix across the other tabs.}
  \item{Group means}{Plot of the observed group means against
    the predicted values. Columns of the design matrix can be
    excluded to examine the impact of different effects.}
  \item{Coefficients}{Table showing how coefficients are
    combined to estimate the group means.}
}
Once the user has finished customizing the design matrix,
clicking the 'Save' button will stop the app and return
the updated \code{\link{designmatrix}} object.
}
\examples{
# Use 'crabs' data set from 'MASS' package
dm = designmatrix_gui( MASS::crabs, list( 'FL', c( 'sp', 'sex' ) ) )

}
