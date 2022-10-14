#' Knot Position for Piecewise Model
#'
#' @param xlist String (preferred) or column number from data set referring to
#'    the column of x values.
#' @param ylist String (preferred) or column number from data set referring to
#'    the column of y values.
#' @param data Data set to find knot position.
#' @param doPlot A Boolean to have function create plots or just return list
#'     Default is to create plots.
#'
#' @importFrom graphics abline curve par
#' @importFrom stats AIC coef lm optimize
#' @importFrom purrr map_dbl
#'
#' @description The function \code{findMyKnot()} will calculate the "ideal" knot
#'    placement for a 2-piece (one knot) piecewise regression model. It does this
#'    by finding the maximum \eqn{R^2} value across many different possible \eqn{x_k},
#'    and by finding the minimum AIC value across these possible knot positions.
#'    This is done in both the discrete case (where we use only the \eqn{x} values
#'    of the data to be used as \eqn{x_k}) and the continuous case where we let
#'    \eqn{x_k} potential be outside the data set.
#'
#' @details This function has two "sub-functions" used to create a vector of outputs
#'    (either \eqn{R^2} values or AIC values for \code{findRsq} or \code{findAIC}
#'    respectively). The vector will be the same length as the vector of possible \eqn{x_k}
#'    that are passed into the function. We then use \code{map_dbl} from \code{purrr}
#'    to easily apply this function to our vector of \eqn{x} values. Graphs of the
#'    \eqn{R^2} and AIC vs \eqn{x_k} are made with a continuous line plotted. Vertical
#'    lines are drawn to represent the discrete "best" knot (in red) and the continuous
#'    version (in blue).
#'
#' @return Two plots of the AIC and \eqn{R^2} vs \eqn{x_k} are made. Also a
#'    named list of the following is returned (invisible):
#' \describe{
#'   \item{x_krd}{The best knot position from the discrete \eqn{R^2} calculation}
#'   \item{x_krc}{The best knot position from the continuous \eqn{R^2} calculation}
#'   \item{x_kad}{The best knot position from the discrete AIC calculation}
#'   \item{x_kac}{The best knot position from the continuous AIC calculation}
#'   \item{Rsqs}{The list of all \eqn{R^2} from the discrete calculations}
#'   \item{AICs}{The list of all AIC from the discrete calculations}
#' }
#'
#' @export
#'
#' @examples
#' ## Example with DDT data set
#' temp <- findMyKnot("LENGTH", "WEIGHT", DDT)
#' temp
#'
#' ## Example with Spruce data set
#' print(findMyKnot("BHDiameter", "Height", SPRUCE))
findMyKnot <- function(xlist, ylist, data, doPlot = TRUE){

  ######################## Set Up ##########################

  x_vals <- (as.vector(data[ , xlist]))[[1]] #List of x values
  y_vals <- (as.vector(data[ , ylist]))[[1]] #List of y values
  min_x <- x_vals[which.min(x_vals)] #Max and min of the x values, useful later
  max_x <- x_vals[which.max(x_vals)]

  ######################## Sub-Funcitons ##########################

  findRsq <- function(x_k){ #Function for Purrr map
    Rsq <- vector(mode = "numeric", length = length(x_k))

    #For loop to return vector of Rsq values if a vector of values
    #is passed in, as in the curve function
    for(i in 1:length(x_k)){
      x <- (x_vals > x_k[i])*(x_vals - x_k[i])
      ylm <- lm(as.vector(y_vals) ~ as.vector(x_vals) + as.vector(x))
      Rsq[i] = (summary(ylm))$r.squared
    }
    return(Rsq)
  }

  findAIC <- function(x_k){ #Function for Purrr map
    AICs <- vector(mode = "numeric", length = length(x_k))

    #For loop to return vector of Rsq values if a vector of values
    #is passed in, as in the curve function
    for(i in 1:length(x_k)){
      x <- (x_vals > x_k[i])*(x_vals - x_k[i])
      ylm <- lm(as.vector(y_vals) ~ as.vector(x_vals) + as.vector(x))
      AICs[i] = AIC(ylm)
    }
    return(AICs)
  }

  ######################## Calculations ##########################

  Rsq <- map_dbl(x_vals, findRsq) #using purrr, yay!!
  AICs <- map_dbl(x_vals, findAIC)

  max_dr <- x_vals[which.max(Rsq)] #Best Rsquared is the maximum
  min_da <- x_vals[which.min(AICs)] #Best AIC Value is the minimum value

  c <- 3 * sqrt(max_x - min_x) #used for optimze around an interval of the discrete case
  max_cr <- optimize(findRsq, interval = c(max_dr - c, max_dr + c), maximum = TRUE)
  min_ca <- optimize(findAIC, interval = c(min_da - c, min_da + c), maximum = FALSE)


  ######################## PLOTTING ##########################

  if(doPlot){ #only plot when asked (will plot by default)
    par(mfrow = c(2,1), mar = c(4,5,2,2))

    plot(x_vals, Rsq,
         xlab = expression(x[k]), ylab = expression(R^2),
         col = "black", pch = 20, lwd = 2.5)
    abline(v = max_dr, col = "red", lwd = 0.5)

    curve(findRsq, add = TRUE, lwd = 1.4, col = "black", n = 555) #n was arbitrary chosen
    abline(v = max_cr$maximum, col = "blue", lwd = 1)

    #---------------------------------------------------#

    plot(x_vals, AICs,
         xlab = expression(x[k]), ylab = expression(AIC),
         col = "black", pch = 20, lwd = 2.5)
    abline(v = min_da, col = "red", lwd = 0.5)

    curve(findAIC, add = TRUE, lwd = 1.4, col = "black", n = 555) #n was arbitrary chosen
    abline(v = min_ca$minimum, col = "blue", lwd = 1)
  }

  ######################## RETURN ##########################

  my_list <- list(x_krd = max_dr, x_krc = max_cr$maximum,
                  x_kad = min_da, x_kac = min_ca$minimum,
                  Rsqs = Rsq, AICs = AICs)
  return(invisible(my_list))
}
