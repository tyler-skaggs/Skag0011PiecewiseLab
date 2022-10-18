#' Piecewise Regression Model
#'
#' @param xlist String (preferred) or column number from data set referring to
#'    the column of x values.
#' @param ylist String (preferred) or column number from data set referring to
#'    the column of y values.
#' @param data Data set to create a 2-piece piecewise model
#' @param type Type of calculation to use for finding knot position. Default is
#'    a discrete calculation from \eqn{R^2}.
#'
#' @importFrom graphics curve par points
#' @importFrom stats AIC coef lm
#' @importFrom Intro2MLR myreadxl
#'
#' @description This function utilizes \code{findKnot} for the main calculation
#'    of potential \eqn{x_k} values. From the provided data, we will create a piecewise
#'    linear model to estimate the trend of the data and variables passed in. Using this
#'    model we calculate the \eqn{y} coordinate of the model and take measures of the
#'    model: \eqn{R^2} and AIC.
#'
#' @details This function utilizes the \code{findKnot} function in this package
#'    to do the calculations (not the plotting) of the "best" knot (type selected by the user), the
#'    values are returned back and selected based on a switch statement. (read more
#'    about \link[PiecwiseSkag0011]{findKnot}). When drawing the piecewise equation
#'    on the plot we utilize \code{curve} and pass in a sub-function of our model.
#'
#' @return This function makes a plot of the data (\eqn{x} vs \eqn{y}) and plots
#'    the calculated piecewise equation of the model and marks the knot position as
#'    a blue circle. An invisible list of the following is returned:
#' \describe{
#'   \item{cord}{The Cartesian coordinate \eqn{(x_k,y_k)} of the knot position}
#'   \item{r.squared}{\eqn{R^2} value from the piecewise model}
#'   \item{AIC}{The AIC value of the piecewise model}
#' }
#' @export
#'
#' @examples
#' ## From DDT data set
#' print(pieceReg("LENGTH", "WEIGHT", DDT, type = "rd"))
#'
#' ## From Spruce Data Set
#' print(pieceReg("BHDiameter", "Height", SPRUCE, type = "rc"))
pieceReg <- function(xlist, ylist, data, type = "rd"){

  ######################## Set Up ##########################

  data <- data.frame(data)

  results <- findKnot(xlist, ylist, data, doPlot = FALSE)

  x_vals <- (as.vector(data[ , xlist])) #List of x values
  y_vals <- (as.vector(data[ , ylist])) #List of y values

  #Switch determines which knot to make model from
  x_k <- switch(type,

                rd = results$x_krd, #default case

                rc = results$x_krc,

                ad = results$x_kad,

                ac = results$x_kac

                )

  ######################## Sub-Functions ##########################

  ## Calculation of new model with 1 knot
  newmodel <- function(xk){
    X <- (x_vals - xk)*(x_vals > xk) #Second line for piecewise function based on knot
    df = within(data, X)
    newlm <- lm(as.vector(y_vals) ~ as.vector(x_vals) + as.vector(X))
    return(newlm)
  }

  ## Piecewise Model for graphing and calculations
  model <- function(xval, xk = x_k, beta = newbeta){
    beta[1] + beta[2] * xval + ((xval > xk) * beta[3] * (xval - xk))
  }

  ######################## Calculations ##########################

  pilm <- newmodel(x_k) #Find New model
  newbeta <- unname(coef(pilm)) #Coefficients of model

  x_rsq <- (summary(pilm))$r.squared
  x_aic <- AIC(pilm)

  y_k = model(x_k, x_k, newbeta) #find position of knot

  ######################## Plotting ##########################

  par(mfrow = c(1,1))

  plot(x_vals, y_vals, xlab = xlist, ylab = ylist,
       col = "black", pch = 20, lwd = 2)
  curve(model, add = TRUE, n = 1000, col = "red", lwd = 2.25)
  points(x_k, y_k, col = "blue", pch = 4, lwd = 2, cex = 1.25)


  ######################## Return ##########################

  my_list <- list(cord = c(x_k, y_k), r.squared = x_rsq, AIC =  x_aic)
  return(invisible(my_list))
}
