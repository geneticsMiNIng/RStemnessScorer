#' Random Forest Classifier
#'
#' Building a classifier based on \code{randomForest} package with
#' possibility of parallel computing (prefered for large forests).
#'
#' @param xdata The training data. `data.frame` or `matrix` object with numeric columns.
#' @param ydata Labels for training. Two-level factor.
#' @param ntree number of trees in the forest.
#' @param njob number of nodes for parallel computing.
#' @return Object of class `randomForest`.
#' @author Katarzyna Sobiczewska
#' @importFrom randomForest randomForest
#' @import doSNOW
#' @import foreach

buildScorer_RF <- function(xdata,ydata, ntree=500, njob=1){
  stay <- nonNA(cbind(xdata, ydata))
  xdata <- xdata[stay,]
  ydata <- ydata[stay]
  if( njob>1 ){
    cl <- makeCluster(njob, type="SOCK")
    registerDoSNOW(cl)
    ntr <- floor(ntree/njob)
    rest <- ntree-ntr*njob
    model <- foreach(ntree = c(rep(ntr, njob-1),ntr+rest),
                     .combine = combine,
                     .packages = "randomForest") %dopar%
      randomForest(x=xdata, y=ydata, ntree = ntree, nodesize = 1)
    stopCluster(cl)
  } else {
    model <- randomForest(xdata,ydata, ntree = ntree, nodesize = 1)
  }
  return(model)
}
