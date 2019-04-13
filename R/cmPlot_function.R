#' Visualize a Confusion Matrix (ggplot)
#'
#' Given a dataframe and a formula object, will return the necessary dependent
#' variable matrix and the model matrix needed to run the optimization for
#' multinomial regression for a given reference level.
#'
#' @param cm a 3 column dataframe with columns for prediction and reference categroies and corresponding frequency counts (third column). 0 counts should be converted to NA
#' @param red_or_white character string c("red", "white") to specify plot coloring
#' @param pred_first logical, whether first column contains the prediction category (default is TRUE)
#' @param title character string to be used as plot title
#'
#' @return a ggplot object visualizing the confusion matrix of reference versus predicted 
#'
#' @import ggplot2
#'
#' @export
cmPlot <- function(cm, red_or_white, pred_first = TRUE, title){
  
  if(red_or_white == "red"){
    lowcol = "pink1"
    highcol = "firebrick3"
  }else{
    lowcol = "lightyellow"
    highcol = "goldenrod1"
  }
  
  if(pred_first == FALSE){
    cm <- cbind.data.frame(cm[,2], cm[,1], cm[,3])
  }
  
  colnames(cm) <- c("Prediction", "Reference", "Freq")
  
  cmPlot <- ggplot(cm, aes(x = Reference, y = Prediction, size = Freq, fill=Freq, label=Freq)) +
    scale_size(range=c(2,20)) + geom_label(na.rm = T) + theme_minimal() +
    scale_fill_continuous(low=lowcol, high=highcol) + guides(size=FALSE) +
    ggtitle(title) + theme(plot.title = element_text(hjust=.5, size=20))

  return(cmPlot)
}

