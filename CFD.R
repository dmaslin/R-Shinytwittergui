####################################
# Cumulative Frequency/Probability Distribution
####################################

CFD <- function (column, bin_width, tag, xlabel=""){
  
  #column: a numeric vector
  #bin_width
  #tag=1: turn on plots; 0: turn off plots
  
  min <- min(column)
  max <- max(column)
  
  breaks <- seq(min, max+1, by = bin_width)
  cuts <- cut(column, breaks, right=FALSE)
  freq <- table(cuts)
  cumFreq <- cumsum(freq)
  cumProb <- cumFreq/length(column)

  freq <- as.data.frame(freq)
  cumFreq <- as.data.frame(cumFreq)
  cumProb <- as.data.frame(cumProb) 
  
  return(list(breaks, freq$Freq, cumFreq[1], cumProb[1]))
  
#   if (tag == 1) {
#     tag
#     par(mfrow=c(1,3))
#     plot.default(freq$Freq, xlim = c(min,max), xlab = xlabel, ylab = 'Frequency' )
#     plot.default(cumFreq[1], xlim = c(min,max), xlab = xlabel, col = 2, main = 'Cumulative Frequency Distribution')
#     plot.default(cumProb[1], xlim = c(min,max), xlab = xlabel, col = 3, main = 'Cumulative Probability Distribution')
# 
#   }
 
  
}