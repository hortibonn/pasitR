#' Perform a two-dimensional kernel density estimation for yield and chill.
#'  
#' The function produces a matrix of the estimated density (z) of yield (y) and chill (x). 
#' As the density function restricts the shape of the kernel to a bivariate normal kernel, it looks slightly different compared to the scatter plot estimates in the pasitR::chillscatter() function.
#' Density surface plot of Chill Portions (x) and yield (y). The legend shows the value for the estimated density (z).
#' 
#' @param chill is a list of observed seasonal Chill Portions corresponding to another list with annual yields. 
#' @param yield is a list of observed annual yields corresponding to another list with seasonal Chill Portions. 
#' 
#' @importFrom MASS kde2d
#' @importFrom stats complete.cases
#' @importFrom graphics filled.contour
#' @importFrom assertthat validate_that
#' @importFrom assertthat see_if
#' 
#' @keywords chill yield kernel density
#'
#' @examples
#' chill <- sample(x = 1:50, size = 20, replace = TRUE)
#' yield <- sample(x = 1000:5000, size = 20, replace = TRUE)
#' chillkernel(chill, yield)
#' 
#' @export chillkernel
chillkernel <- function(chill, yield) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package \"stats\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  chillyielddata <- chillyield <- chillyieldkernel <- ylab <- xlab <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
  #add error stops with validate_that   
  assertthat::validate_that(length(chill) == length(yield), msg = "\"chill\" and \"yield\" are not equal lengths.")
  assertthat::validate_that(is.numeric(chill), msg = "\"chill\" is not numeric.")

  assertthat::validate_that(is.numeric(yield), msg = "\"yield\" is not numeric.")

  #create subset-able data
  chillyield <- as.data.frame(cbind(chill, yield)) 
  
  ## Use 'complete.cases' from stats to get to the collection of obs without NA
  chillyielddata <- chillyield[stats::complete.cases(chillyield), ]
  
  #message about complete cases
  assertthat::see_if(length(chillyield) == length(chillyielddata), msg = "Rows with NA were removed.")
  
  #### kernel density estimation ####
  
  ## create a density surface with kde2d
  chillyieldkernel <- MASS::kde2d(chillyielddata$chill, chillyielddata$yield, n = 100)
  
  graphics::filled.contour(chillyieldkernel, xlab = "Chill Portions", ylab = "Yield")
  
  print("Density surface plot of Chill Portions (x) and yield (y).")
}

