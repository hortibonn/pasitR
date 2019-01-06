#' Create a scatter plot of chill and yield
#'
#' Violin plots of chill portions (x) and yield (y) with six different intervals of chill portions.
#' Determine different possible chill portion intervals by calculating the optimal interval width for chill portions using the IQR() function in the stats() package of R, after the Freedman-Diaconis rule (IQR = interquartile range).
#'
#' @param chill is a list of observed annual chill portions corresponding to another list with annual yields. 
#' @param yield is a list of observed annual yields corresponding to another list with annual chill portions. 
#' 
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 cut_width
#' @importFrom ggplot2 geom_violin
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 scale_color_discrete
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom stats complete.cases
#' @importFrom stats IQR
#' 
#' @keywords chill yield Freedman-Diaconis IQR violin
#'
#' @examples
#' chill <- sample(x = 1:50, size = 20, replace = TRUE)
#' yield <- sample(x = 1000:5000, size = 20, replace = TRUE)
#' chillviolin(chill, yield)
#' 
#' @export chillviolin
chillviolin <- function(chill, yield) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("Package \"stats\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  chillyielddata <- chillyield <- ylab <- xlab <- width <- aes <- cut_width <- NULL # Setting the variables to NULL first, appeasing R CMD check
  
  chillyield<-as.data.frame(cbind(chill, yield)) #create subset-able data
  
  ## Use 'complete.cases' from stats to get to the collection of obs without NA
  chillyielddata<-chillyield[stats::complete.cases(chillyield), ]
  
  ## a method to calculate the optimal bin width for the violin plots 
  ## after the Freedman-Diaconis rule (IQR = interquartile range, buildt-in):
  width <- 2 * stats::IQR(chillyielddata$chill) / length(chillyielddata$chill)^(1/3)
  
  ## violin plot with IQR cut_width
  chillyieldviolin <- ggplot2::ggplot(chillyielddata, ggplot2::aes(ggplot2::cut_width(chill, width=width), yield, 
                          color=ggplot2::cut_width(chill, width=width))) +
    ggplot2::geom_violin() +
    ggplot2::theme_classic()+
    ggplot2::geom_boxplot(width=.1) +
    ggplot2::xlab("Chill portions") +
    ggplot2::ylab("Yield") +  
    ggplot2::scale_color_discrete(name="Chill Portion Intervals")
  
  print(chillyieldviolin)
  
  print("Violin plots of chill portions (x) and yield (y) with six different intervals of chill portions.")

  }



