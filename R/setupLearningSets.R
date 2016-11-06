#' Setup Train & Test
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
#' @return List of data sets as follows:
#' list(train = list(X=..., Y=...), test = list(X=..., Y=...))
#' @author Katarzyna Sobiczewska
#' @examples
#' wmct <- WMCT(methPCBC, ENDOhealthy, G='SC', njob=3)
#' sign <- signatureWMCT(wmct, n=10)
#' train <- prepareTraining(methPCBC,ENDOhealthy,G='SC',signature = sign)
#' m <- glmnet::glmnet(as.matrix(train$X), train$Y, family='binomial')
#' plot(m)
#' @export

setupLearningSets <- function(pcbc, tcga, signature, G="SC", cutoff=.7){
  stopifnot(G%in%pcbc[[key(pcbc)]])
  g <- pcbc[G,signature, with=FALSE]
  healthy <- tcga[,signature, with=FALSE]
  xdata <- rbind(g, healthy)
  ydata <- as.factor( rep( c(G,'healthy'), c(nrow(g),nrow(healthy)) ) )
  
  groups <- split(1:length(ydata), ydata)
  sapply( groups, function(ind){
    n <- length(ind)
    sample(ind, floor(n*cutoff))
  }) -> inTrain
  inTrain <- unlist(inTrain)
  testX <- xdata[!inTrain,]
  testY <- ydata[-inTrain]
  trainX <- xdata[inTrain,]
  trainY <- ydata[inTrain]
  
  return(list( test = list(X=testX, Y=testY),
               train = list(X=trainX, Y=trainY)) )
}
