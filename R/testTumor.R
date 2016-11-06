#' Scores for Patients in TCGA Datasets.
#'
#' Predict probabilities for all patients from TCGA dataset and get
#' a nice visualization.
#' Each patients should be assign to some group of observations, i.e. tumor
#' type. This function was made to compare results between each group of
#' patients.
#'
#' @param tcga dataset containing in the first column labels
#' given as factor.
#' @param objects list of one or more models to compare.
#' @param resp which class is the interesting response (respectively to
#' \code{object$glmnet.fit$classnames} (\code{model="LR"}) or
#' \code{object$classes} (\code{model="RF"}) output).
#' @details If you prefer raw data rather than ggplot result use
#' \code{out$data} element. See example below to get more informations.
#' @return \code{ggplot} object that can be easily expanded and modifed.
#' @author Katarzyna Sobiczewska
#' @examples
#' wmct_ENDO <- WMCT(methPCBC, ENDOhealthy, G='SC', njob=3)
#' signature_ENDO <- signatureWMCT(wmct_ENDO,alpha=0.05)
#' train <- prepareTraining(methPCBC, ENDOhealthy,G='SC',signature_ENDO)
#' measures <- c("class","auc","dev")
#' models <- lapply(measures, function(measure)
#'                  buildScorer(train$X,train$Y,model='LR',
#'                              measure, njob=3))
#' gg <- TCGAscores(ENDOtumor, models)
#' gg <- TCGAscores(ENDOtumor, models)
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#' @export

testTumor <- function(tcga, objects, resp=2, verbose=TRUE){
  objtype <- class(objects)
  if( objtype %in% c('randomForest','cv.glmnet') ){
    objects <- list(objects)
    names(objects) <- objtype
  }
  objtypes <- unique(sapply(objects,class))
  # stopifnot( length(unique(objtypes))==1 ) #?
  stopifnot( all(unique(objtypes) %in% c("cv.glmnet","randomForest")) )
  if( is.null(names(objects)) ) stop('Objects are not named. Names are required.')
  coltypes <- names(table(sapply(tcga[,-c(1,2)],class)))
  if( any(coltypes!='numeric') ) stop('Predictors are not class of numeric.')
  
  target_col <- colnames(tcga)[!(colnames(tcga)%in%c('bcr_patient_barcode', 'cancer'))]
  # tcga <- tcga[, target_col ]
  # 
  setkey(tcga, cancer)
  lvls <- unique(tcga[[key(tcga)]])
  lapply(names(objects), function(objnm){
    sapply(lvls, function(l){
      if( verbose )
        message(sprintf("\t\t Processing group: %s for the %s model.", l, objnm))
      test <- tcga[l, !c('bcr_patient_barcode', 'cancer'), with=FALSE]
      out <- scorer(test, objects[[objnm]], resp=resp)
      return(out)
    }) -> scores
    scores <- melt(scores)
    scores <- data.frame(scores, model=objnm)
  }) -> df
  df <- do.call('rbind', df)
  colnames(df) <- c('values','Cancer group','model')
  ggplot( df, aes(y=values, x=model, fill=`Cancer group`) ) +
    geom_boxplot() + ylab('predictions')
}
