#-----------------------------------------------------------------------------#

# Confusion Matrix Plotting Function

#-----------------------------------------------------------------------------#

# cm is confusion matrix formatted as a dataframe
# 0 counts must be changed to NA
# 3 columns, third is frequency counts
# 1st and 2nd cols are pred/reference categories
##### class.red <- as.data.frame(cM.red$table)
##### class.red$Freq[class.red$Freq == 0] <-NA

# red_or_white colors the plot accordingly
# enter "red" or "white" (in quotes)

# pred_first specificies first columns contains the prediction
# category and the second column contains the reference
# set to FALSE if columns reversed

# title is the plot title (in quotes)

# see classify_by_quality.R for examples
# make sure to source the file to use the function
#### source("./R/cmPlot_function.R") 

#-----------------------------------------------------------------------------#

library(ggplot2)

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

#-----------------------------------------------------------------------------#
