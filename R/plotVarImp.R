#' Plot of Variable Importance
#'
#' Dotchart of variable importance given by a \code{randomForest} model.
#'
#' @param rf the model class of \code{randomForest}.
#' @param n.var number of variables to display.
#' @param size point size.
#' @param labels logical; does the signature labels should be displayed?
#' @return \code{ggplot} object that can be easily expanded and modified.
#' @author Katarzyna Sobiczewska
#' @examples
#' wmct <- WMCT(methPCBC, ENDOhealthy, G='SC', njob=3)
#' sign <- signatureWMCT(wmct, n=100)
#' train <- prepareTraining(methPCBC,ENDOhealthy,G='SC',signature = sign)
#' rf <- buildScorer(train$X,train$Y, model="RF",ntree=500, njob = 3)
#' plotVarImp(rf, n.var=99, size=0.5)
#' @import ggplot2
#' @export

plotVarImp <- function(rf, n.var=50, size=0.5, labels=TRUE){
  data <- data.frame(Features=rownames(rf$importance),MeanDecreaseGini=rf$importance)
  ord <- order(rf$importance)
  data <- tail(data[ord,], n.var)
  data$Features <- factor(data$Features, levels =
                            data$Features[order(data$MeanDecreaseGini)])
  ggplot(data) + geom_point(aes(MeanDecreaseGini,Features), size=size) +
    ylab("Sorted features") + theme_bw() -> g
  if( labels == FALSE )
    g <- g + theme(axis.text.y=element_blank(),
                   axis.ticks.y=element_blank())
  return(g)
}
