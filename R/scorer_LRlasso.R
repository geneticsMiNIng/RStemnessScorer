#' Lasso Scorer
#'
#' Get the signture from Wilcoxon Multiple Comparisions Test.
#'
#' @param xtest dataset to test
#' @param object model created by \code{\link{buildScorer}(model='LR')}.
#' @param resp which from object$classes or object$glmnet.fit$classnames
#' is the response.
#' @param alpha take these adjusted p-values that are smaller than alpha.
#' @return Vector of scores for each observation. The score is a value
#' between 0 and 1.
#' @author Katarzyna Sobiczewska
#' @import faraway

scorer_LRlasso <- function(xtest, object, resp=2){
  stopifnot( class(object) == "cv.glmnet" )
  cff <- getCoeffs(object)
  a <- ifelse( "(Intercept)" %in% names(cff), cff["(Intercept)"], 0)
  b <- cff[!names(cff)%in%'(Intercept)']
  if( all(names(b) %in% colnames(xtest)) ){
    xtest <- xtest[,names(b)]
    pred <- a + t(b)%*%t(xtest)
    pred <- ilogit(pred)
    if( resp==1 ) return( 1 - c(pred) ) else
      return(c(pred))
  } else {
    stop('Predictors in new model do not match that to the training.')
  }
}
