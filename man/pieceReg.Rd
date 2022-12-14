% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pieceReg.R
\name{pieceReg}
\alias{pieceReg}
\title{Piecewise Regression Model}
\usage{
pieceReg(xlist, ylist, data, type = "rd")
}
\arguments{
\item{xlist}{String (preferred) or column number from data set referring to
the column of x values.}

\item{ylist}{String (preferred) or column number from data set referring to
the column of y values.}

\item{data}{Data set to create a 2-piece piecewise model}

\item{type}{Type of calculation to use for finding knot position. Default is
a discrete calculation from \eqn{R^2}.}
}
\value{
This function makes a plot of the data (\eqn{x} vs \eqn{y}) and plots
   the calculated piecewise equation of the model and marks the knot position as
   a blue circle. An invisible list of the following is returned:
\describe{
  \item{cord}{The Cartesian coordinate \eqn{(x_k,y_k)} of the knot position}
  \item{r.squared}{\eqn{R^2} value from the piecewise model}
  \item{AIC}{The AIC value of the piecewise model}
}
}
\description{
This function utilizes \code{findKnot} for the main calculation
   of potential \eqn{x_k} values. From the provided data, we will create a piecewise
   linear model to estimate the trend of the data and variables passed in. Using this
   model we calculate the \eqn{y} coordinate of the model and take measures of the
   model: \eqn{R^2} and AIC.
}
\details{
This function utilizes the \code{findKnot} function in this package
   to do the calculations (not the plotting) of the "best" knot (type selected by the user), the
   values are returned back and selected based on a switch statement. (read more
   about \link[PiecwiseSkag0011]{findKnot}). When drawing the piecewise equation
   on the plot we utilize \code{curve} and pass in a sub-function of our model.
}
\examples{
## From DDT data set
print(pieceReg("LENGTH", "WEIGHT", DDT, type = "rd"))

## From Spruce Data Set
print(pieceReg("BHDiameter", "Height", SPRUCE, type = "rc"))
}
