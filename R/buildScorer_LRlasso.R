#' Logistic Regression (with LASSO penalty) Classifier
#'
#' Model of Logistic Regression with LASSO penalty. \code{lambda} penalty
#' is fitted by cross-validation.
#'
#' @param xdata The training data. `data.frame` or `matrix` object with numeric columns.
#' @param ydata Labels for training. Two-level factor.
#' @param cv.measure one from c("deviance","class","auc").
#' @param njob number of nodes for parallel computing.
#' @return Object of cv.glmnet class.
#' @author Katarzyna Sobiczewska
#' @import parallel
#' @import doSNOW

buildScorer_LRlasso <- function(xdata,ydata, cv.measure, njob=1, ...){

  if( njob>1 ){
    cl <- makeCluster(njob)
    registerDoSNOW(cl)
    message('Loading glmnet package on threads...')
    clusterEvalQ(cl,library(glmnet))
    clusterExport(cl, c("xdata","ydata","cv.measure"), envir = environment())
  }
  model <- cv.glmnet(as.matrix(xdata),ydata,
                     family='binomial',
                     alpha = 1,
                     parallel = ifelse(njob>1,TRUE,FALSE),
                     type.measure = cv.measure,
                     ...)
  if( njob>1 )
    stopCluster(cl)
  return(model)
}
