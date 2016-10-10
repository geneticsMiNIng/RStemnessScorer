#' Extract Non-zero Model Coefficients
#'
#' This function returns only non-zero coefficient values from model
#' created by \code{\link{buildScorer}} function with parameter
#' \code{model="LR"}.
#'
#' @param model object created by \code{buildScorer} with argument \code{model="LR"}.
#' @details LASSO penalty shrinks many coefficients to exactly \code{0}.
#' @return Named vector of numerics. Names are predictors and values are
#' coefficients.
#' @author Katarzyna Sobiczewska
#' @examples
#' wmct <- WMCT(methPCBC, ENDOhealthy, G='SC', njob=3)
#' sign <- signatureWMCT(wmct, n=10)
#' train <- prepareTraining(methPCBC,ENDOhealthy,G='SC',signature = sign)
#' scorer <- buildScorer(train$X, train$Y, model="LR",
#'                       cv.measure='class', njob = 3,
#'                       intercept=TRUE, standardize=FALSE)
#' getCoeffs(scorer)
#' @export

getCoeffs <- function(model){
  stopifnot( class(model) == 'cv.glmnet' )
  c <- coef(model, s='lambda.min')
  ind <- which(c[,1] != 0)
  out <- c[ind,]
  names(out) <- rownames(c)[ind]
  return(out)
}
