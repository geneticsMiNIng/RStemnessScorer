#' Build the Scorer
#'
#' Build the scorer based on Logistic Regression (with LASSO penalty)
#' or Random Forest model.
#'
#' @param xdata dataset to train. \code{data.frame} or \code{matrix} object with numeric columns.
#' @param ydata labels for the train. Two-level factor.
#' @param model type of model to use. One from "LR" or "RF".
#' @param njob number of nodes in parallel computing.
#' @param ntree number of trees in the forest (only for RF)
#' @param cv.measure one from c("deviance","class","auc") (only for LR).
#' @param ... further arguments to pass to \code{cv.glmnet\{glmnet\}} (only for LR).
#' @return The model, object of \code{cv.glmnet} or \code{randomForest} class
#' respectively to the passed \code{model} argument.
#' @author Katarzyna Sobiczewska
#' @examples
#' wmct <- WMCT(methPCBC, ENDOhealthy, G='SC', njob=3)
#' sign <- signatureWMCT(wmct, n=10)
#' train <- prepareTraining(methPCBC,ENDOhealthy,G='SC',signature = sign)
#' scorer <- buildScorer(train$X, train$Y, model="LR",
#'                       cv.measure='class', njob = 3,
#'                       intercept=TRUE, standardize=FALSE)
#' class(scorer)
#' plot(scorer)
#' @export

buildScorer <- function(xdata, ydata, model, njob=1, ...){
  stopifnot( model %in% c("LR","RF") )
  stopifnot( njob %% 1 == 0 )
  stopifnot( is.factor(ydata) )
  stopifnot( length(levels(ydata)) == 2 )
  if( model == "LR" ) scorer <- buildScorer_LRlasso(xdata, ydata, njob=njob, ...)
  if( model == "RF" ) scorer <- buildScorer_RF(xdata, ydata, njob=njob, ...)
  return(scorer)
}
