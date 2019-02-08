#' Estimated yield given the expected chill, based on a slice of 'z' from the Kernel density plot of chill and yield data.
#'
#' Plot representing probabilities (shown along the y-axis) for the expected yield (shown along the x-axis). 
#' This is a broad slice through the density kernel from pasitR::chillkernel() function, which integrates to 1, the probability values are relative, not absolute measures.
#' 
#' @param chill is a list of observed annual chill portions corresponding to another list with annual yields. 
#' @param yield is a list of observed annual yields corresponding to another list with annual chill portions. 
#' @param max_chill is a value of the highest expected  amount of chill for which the yield should be estimated (must be > expectedlowchill). 
#' @param min_chill is a value of the lowest expected amount of chill for which the yield should be estimated (must be < expectedhighchill). 
#' adding some sily stuff
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
#' chillkernelslicerange(chill, yield, 10, 20)
#' 
#' @export chillkernelslicerange
chillkernelslicerange <- function(chill, yield, min_chill, max_chill) {
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
  
  assertthat::validate_that(is.numeric(min_chill), msg = "\"min_chill\" is not numeric.")
  
  assertthat::validate_that(is.numeric(max_chill), msg = "\"max_chill\" is not numeric.")
  
  assertthat::validate_that(is.numeric(yield), msg = "\"yield\" is not numeric.")
  
  #create subset-able data
  chillyield<-as.data.frame(cbind(chill, yield)) 
  
  ## Use 'complete.cases' from stats to get to the collection of obs without NA
  chillyielddata<-chillyield[stats::complete.cases(chillyield), ]
  #message about complete cases
  assertthat::see_if(length(chillyield) == length(chillyielddata), msg = "Rows with NA were removed.")
  
  #compare length of chillyield and chillyielddata and print 'you lost 'x' cases
  
  #### kernel density estimation ####
  
  ## create a density surface with kde2d
  chillyieldkernel <- MASS::kde2d(chillyielddata$chill, chillyielddata$yield, n = 100)
  
  ## Cut through density kernel and averaging over a range of x-values (x = chill portions)
  # sets the boundaries of chill-values over which to average
  lbound <- which(chillyieldkernel$x == min(chillyieldkernel$x[which(chillyieldkernel$x > min_chill)]))
  rbound <- which(chillyieldkernel$x == max(chillyieldkernel$x[which(chillyieldkernel$x <= max_chill)]))
  
 graphics::plot(chillyieldkernel$y, rowMeans(chillyieldkernel$z[,lbound:rbound]), type="l", col="seagreen", lwd = 2,
       xlab = paste("Yield for chill values between", as.character(min_chill), "and", 
                    as.character(max_chill)), ylab = "Relative probability")
  #for print we need x for max(chillyieldkernel$z[,lbound:rbound])
 
  print("Relative probability (y) of yield for the given chill portion interval (x).")
}

