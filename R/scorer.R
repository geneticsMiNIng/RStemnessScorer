#' Get a Scores
#'
#' Predict probabilities of membership to selected class based on given
#' model.
#'
#' @param xtest dataset to test
#' @param object model created by \code{\link{buildScorer}}.
#' @param resp which class is the interesting response (respectively to \code{object$glmnet.fit$classnames} (\code{model="LR"}) or \code{object$classes} (\code{model="RF"}) output).
#' @return Vector of scores for each observation. The score is a value
#' between 0 and 1.
#' @author Katarzyna Sobiczewska
#' wmct <- WMCT(methPCBC, ENDOhealthy, G='SC', njob=3)
#' sign <- signatureWMCT(wmct, n=10)
#' train <- prepareTraining(methPCBC,ENDOhealthy,G='SC',signature = sign)
#' model <- buildScorer(train$X, train$Y, model="LR", cv.measure='dev')
#' probs <- scorer(train$X, model)
#' plot(sort(probs))
#' @export

scorer <- function(xtest, object, resp=2){
  stopifnot( class(object) %in% c('cv.glmnet','randomForest') )
  stopifnot( resp %in% 1:2 )
  if( class(object) == 'cv.glmnet' )
    out <- scorer_LRlasso(xtest,object,resp=resp)
  if( class(object) == 'randomForest' )
    out <- scorer_RF(xtest,object,resp=resp)
  return(out)
}
