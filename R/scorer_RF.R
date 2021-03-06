#' Random Forest Scorer
#'
#' @param xtest dataset to test
#' @param object model created by \code{\link{buildScorer}(model='RF')}.
#' @param resp which from object$classes is the response.
#' @return Vector of scores for each observation. The score is a value
#' between 0 and 1.
#' @author Katarzyna Sobiczewska

scorer_RF <- function(xtest, object, resp=2){
  stopifnot( class(object) == 'randomForest' )
  imp <- rownames(importance(object))
  imp0 <- imp[importance(object)==0]
  xtest[, imp0] <- 0
  pred <- rep(NA,nrow(xtest))
  features <- rownames(object$importance)
  xtest <- xtest[, features, with=FALSE]
  nas <- apply(xtest,1,function(x) any(is.na(x)))
  p <- randomForest:::predict.randomForest(object, 
                                           newdata = as.matrix(xtest)[!nas,], 
                                           type='prob')[,resp]
  pred[!nas] <- p
  return(pred)
}
