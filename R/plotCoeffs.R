#' Plot the Coefficients
#'
#' Compare finded coefficients extracted from Logistict Regression + LASSO
#' model(s) (see: \code{\link{buildScorer}(model=RF)}).
#'
#' @param coeffs list of coefficients estimated from different models: each element
#' should be formatted as named vector. If there is no need to put list as
#' one model was created you can pass just a named vector. Names are predictor
#' names (signature) and values are coefficients values estimated by the model.
#' @return \code{ggplot} object that can be easily expanded and modified.
#' @author Katarzyna Sobiczewska
#' @examples
#' wmct <- WMCT(methPCBC, ENDOhealthy, G='SC', njob=3)
#' sign <- signatureWMCT(wmct, n=10)
#' train <- prepareTraining(methPCBC,ENDOhealthy,G='SC',signature = sign)
#' measures <- c('class','auc','dev')
#' models <- lapply(measures, function(m)
#'                    buildScorer(train$X, train$Y, model="LR",
#'                                cv.measure=m, njob = 3))
#' names(models) <- measures
#' signatures <- lapply(models, getCoeffs)
#' (g <- plotCoeffs(signatures))
#' g + ggtitle("The title")
#'
#' plotCoeffs(signatures[[3]])
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 theme
#' @export

plotCoeffs <- function(coeffs){
  one.model <- FALSE
  if( class(coeffs) != 'list' ){
    coeffs <- list(coeffs)
    names(coeffs) <- "1"
    one.model <- TRUE
  }
  signature <- Reduce(union, sapply(coeffs, names))
  lapply(names(coeffs), function(s){
    temp <- numeric(length(signature))
    names(temp) <- signature
    sign <- coeffs[[s]]
    temp[names(sign)] <- sign
    df <- data.frame(model.type = s, signature = names(temp),
                     coef=temp, row.names = NULL)
    return(df)
  }) -> df
  df <- do.call('rbind', df)
  ggplot(df, aes(x=signature, y=coef))  +
    geom_bar( aes(fill=model.type), stat='identity', position='dodge') +
    theme(axis.text.x = element_text(angle=90)) -> gg
  if( one.model ) gg <- gg + theme(legend.position = "none")
  return(gg)
}
