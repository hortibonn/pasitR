#' Estimated yield given the expected chill, based on a slice of 'z' from the Kernel density plot of chill and yield data.
#'
#' Plot representing probabilities (shown along the y-axis) for the expected yield (shown along the x-axis). 
#' This is a cut through the density kernel from pasitR::chillkernel() function, which integrates to 1, the probability values are relative, not absolute measures.
#' 
#' @param chill is a list of observed annual chill portions corresponding to another list with annual yields. 
#' @param yield is a list of observed annual yields corresponding to another list with annual chill portions. 
#' @param expectedchill is a value of expected chill for which the yield should be estimated. 
#' 
#' @importFrom MASS kde2d
#' @importFrom stats complete.cases
#' @importFrom graphics filled.contour
#' @importFrom graphics plot
#' @importFrom assertthat validate_that
#' @importFrom assertthat see_if
#' 
#' @keywords chill yield kernel density
#'
#' @examples
#' chill <- sample(x = 1:50, size = 20, replace = TRUE)
#' yield <- sample(x = 1000:5000, size = 20, replace = TRUE)
#' chillkernelslice(chill, yield, expectedchill = 30)
#' 
#' @export chillkernelslice
chillkernelslice <- function(chill, yield, expectedchill) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package \"stats\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  # Setting the variables to NULL first, appeasing R CMD check
  chillyielddata <- chillyield <- ylab <- xlab <- NULL 
  
  #add error stops with validate_that   
  assertthat::validate_that(length(chill) == length(yield), msg = "\"chill\" and \"yield\" are not equal lengths.")
  assertthat::validate_that(is.numeric(chill), msg = "\"chill\" is not numeric.")
  
  assertthat::validate_that(is.numeric(expectedchill), msg = "\"expectedchill\" is not numeric.")

  assertthat::validate_that(is.numeric(yield), msg = "\"yield\" is not numeric.")

  #create subset-able data
  chillyield<-as.data.frame(cbind(chill, yield)) 
  
  ## Use 'complete.cases' from stats to get to the collection of obs without NA
  chillyielddata<-chillyield[stats::complete.cases(chillyield), ]
  #message about complete cases
  assertthat::see_if(length(chillyield) == length(chillyielddata), msg = "Rows with NA were removed.")
  
  #### kernel density estimation ####
  
  ## create a density surface with kde2d
  chillyieldkernel <- MASS::kde2d(chillyielddata$chill, chillyielddata$yield, n = 100)
  
  ## cut through density kernel #####
  graphics::plot(chillyieldkernel$x, chillyieldkernel$z[,expectedchill], type="l", 
       ylab = "Relative probability", xlab = "Yield for chill values", 
       col="seagreen", lwd = 2)
  
  print("Estimated yield given expected chill.")
}
