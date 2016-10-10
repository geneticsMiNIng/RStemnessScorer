#' Prepare the Dataset to Train
#'
#' Combining PCBC and TCGA data as one training data set.
#'
#' @param pcbc data set containing in the first column labels
#' given as factor. At least one level of the factor should be G. The
#' next columns are for predictors.
#' @param tcga tcga data set with healthy tissues.
#' The columns are predictors.
#' @param G label name for selected group of samples in \code{pcbc} given
#' as character.
#' @param signature vector of characters with names of predictors that
#' might be used by the training data.
#' @return Two-element list.
#' @return  \code{X} training dataset as an object of class \code{data.frame}.
#' @return  \code{Y} vector of responses as an object of class \code{factor}.
#' @author Katarzyna Sobiczewska
#' @examples
#' wmct <- WMCT(methPCBC, ENDOhealthy, G='SC', njob=3)
#' sign <- signatureWMCT(wmct, n=10)
#' train <- prepareTraining(methPCBC,ENDOhealthy,G='SC',signature = sign)
#' m <- glmnet::glmnet(as.matrix(train$X), train$Y, family='binomial')
#' plot(m)
#' @export

prepareTraining <- function(pcbc, tcga, signature, G="SC"){
  stopifnot(G%in%levels(pcbc[,1]))
  g <- pcbc[pcbc[,1]==G,signature]
  healthy <- tcga[,signature]
  xdata <- rbind(g, healthy)
  ydata <- as.factor( rep( c(G,'healthy'), c(nrow(g),nrow(healthy)) ) )
  return(list(X = xdata, Y=ydata))
}
