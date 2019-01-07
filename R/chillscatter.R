#' Create a scatter plot of chill and yield
#'
#' Chill (x) and yield (y) scatter plot with associated and estimated densities  (using a loess smooth) given by the red dot (mean) and red ellipses (1 and 2 sigma from mean). The red line going across the plot shows the linear fit. Histograms are shown with smooth lines (loess smooth linear fits) density curves. The numeric value in the upper right gives the Spearman correlation coefficient between chill portions and yield.
#' @param chill is a list of observed annual chill portions corresponding to another list with annual yields. 
#' @param yield is a list of observed annual yields corresponding to another list with annual chill portions. 
#' 
#' @importFrom psych scatter.hist
#' @importFrom stats complete.cases
#' 
#' @keywords chill, chill portions, yield
#'
#' @examples
#' chill <- sample(x = 1:50, size = 20, replace = TRUE)
#' yield <- sample(x = 1000:5000, size = 20, replace = TRUE)
#' chillscatter(chill, yield)
#' 
#' @export chillscatter
chillscatter <- function(chill, yield) {
    if (!requireNamespace("psych", quietly = TRUE)) {
        stop("Package \"psych\" needed for this function to work. Please install it.",
            call. = FALSE)
  }
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package \"stats\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  chillyielddata <- chillyield <- ylab <- xlab <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
  chillyield<-as.data.frame(cbind(chill, yield)) #create subset-able data
  
  ## Use 'complete.cases' from stats to get to the collection of obs without NA
  chillyielddata<-chillyield[stats::complete.cases(chillyield), ]
  
  ## build a scatter plot with a histogram of x and y with 'psych'
  psych::scatter.hist(x=chillyielddata$chill, y=chillyielddata$yield, density=TRUE, 
                      xlab="Chill", ylab="Yield")
  
    print("Chill (x) and yield (y) scatter plot with associated and estimated densities.")
}
