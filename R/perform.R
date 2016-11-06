#' Perforn Classification
#' 
#' Training and validation on stemness data and test on tumor data.
#' @import ggfortify
#' @import digest
#' @importFrom pROC roc
#' @export

perform <- function(data = 'methylation',
                    group = 'meso',
                    G = 'SC',
                    FUN = function(x) rank(x, na.last='keep')/length(x),
                    pcbc.dir = '~/RProjects/RStemnessScorer_backup/',
                    meta_class = 'Diffname_short',
                    meta_id = 'UID',
                    nthreads = 3,
                    ntrees = 2000){
  return <- list()
  main_path <- '~/RStemnessScorer_results'
  go_back_to <- getwd()
  if(!dir.exists(main_path)) dir.create(main_path); setwd(main_path)
  if(!dir.exists('.cache')) dir.create('.cache'); setwd('.cache')
  
  args <-as.list(match.call())
  args$pcbc.dir <- normalizePath(args$pcbc.dir)

  hs <- hash(args, c('FUN','pcbc.dir','data','meta_class','group'), 'data')
  if(isnot(hs)){
    PCBC.data <- loadPCBC(pcbc.dir, data, meta_class = meta_class)
    TCGA <- RTCGATumorNormal(data, group)
    ####  0. take the overlap
    PCBC.data <- nonNAdata(PCBC.data)
    TCGA$data <- nonNAdata(TCGA$data)
    
    message('Unify PCBC and TCGA normalization...')
    features <- intersect(colnames(TCGA$data), colnames(PCBC.data))
    PCBC.data[, (features):=lapply(.SD, FUN), .SDcols=features ]
    TCGA$data[, (features):=lapply(.SD, FUN), .SDcols=features ]
    
    pcbc <- PCBC.data[, c(meta_class, features), with=FALSE]
    tcga <- TCGA$data[, c(key(TCGA$data), 'cancer', features), with=FALSE]
    
    save(pcbc,tcga,PCBC.data,TCGA, file=hs)
  }else{
    message('Loading cached data sets.')
    load(hs)
  }
  message(sprintf('\tPCBC data: %s x %s.', dim(PCBC.data)[1], dim(PCBC.data)[2]))
  message(sprintf('\tTCGA data: %d x %d.', dim(TCGA$data)[1], dim(TCGA$data)[2]))
  message(sprintf('\t\t normal: %d x %d.', length(TCGA$normal), dim(TCGA$data)[2]))
  message(sprintf('\t\t tumor: %d x %d.', length(TCGA$tumor), dim(TCGA$data)[2]))
  
  #### 1. Wilcoxon test: multiple comparisions part.
  hs <- hash(args, c('FUN','pcbc.dir','data','meta_class','group','G'), 'wmct')
  if(isnot(hs)){
    wmct <- WMCT(pcbc, tcga[TCGA$normal,], G=G, njob=nthreads) ## 1000 pierwszych jeszcze dosc dobrze rozdzielalo.
    save(wmct, file=hs)
  }else{
    message('Loading cached Wilcoxon Multiple Comparisions p-values.')
    load(hs)
  }
  signature <- signatureWMCT(wmct,n=7000)
  return[['wmct_signature']] <- signature
  toshow = c(head(signature),tail(signature))
  return[['wmct_plot']] <- plotSignature(
      x=rbind(pcbc[,toshow, with=FALSE],
              tcga[TCGA$normal,toshow, with=FALSE]),
      y=c(ifelse(pcbc[[key(pcbc)]]=="SC","SC","nonSC"),rep("healthy", length(TCGA$normal))),
      horiz=FALSE
    ) 
  
  ### DESIGN
  hs <- hash(args, c('FUN','pcbc.dir','data','meta_class','group','G', 'cutoff'), 'design')
  if(isnot(hs)){
    learn <- setupLearningSets(pcbc,tcga[TCGA$normal,], G=G, 
                               signature = signature, cutoff=0.7)
    save(learn, file=hs)
  }else{
    message('Loading cached train:test data design.')
    load(hs)
  }
  tr <- table(learn$train$Y)
  train_balance <- paste(paste0(names(tr), collapse = ':'), paste0(tr, collapse=':'))
  ts <- table(learn$test$Y)
  test_balance <- paste(paste0(names(ts), collapse = ':'), paste0(ts, collapse=':'))
  message(sprintf('\tTrain class balance: %s.', train_balance))
  message(sprintf('\tTest class balance: %s.', test_balance))
  message(sprintf('\tNumber of features: %s.', dim(learn$train$X)[2]))
  
  balance <-  c(train_balance, test_balance)
  names(balance) <- c('train','test')
  return[['balance']] <- balance

  ### LR
  objectiveFun <- c("Class","AUC","Deviance")
  hs <- hash(args, c('FUN','pcbc.dir','data','meta_class','group','G','cutoff'), 'lr')
  if(isnot(hs)){
    models <- lapply(objectiveFun, function(f)
      buildScorer(learn$train$X, learn$train$Y, model="LR",
                  cv.measure=tolower(f), intercept=TRUE, standardize=FALSE,
                  njob = nthreads))
    names(models) <- objectiveFun
    save(models, file=hs)
  }else{
    message('Loading cached linear models.')
    load(hs)
  }
  
  for( m in objectiveFun ){
    g <- autoplot(models[[m]], label.vjust = -.1, color='blue', label.size = 4 ) + ggtitle(m)
    return[['glmnet_gg']][[m]] <- g
  }
  coefs <- lapply(models[objectiveFun], getCoeffs)
  return[['glmnet_features']] <- plotSignature(learn$train$X, 
                                               learn$train$Y, 
                                               signatures = sapply(coefs, names))
  return[['glmnet_coefs']] <- plotCoeffs(coefs)
  
  ### RF
  hs <- hash(args, names(args), 'rf')
  if(isnot(hs)){
    rf <- buildScorer(learn$train$X,
                      learn$train$Y,model="RF",ntree=ntrees, 
                      njob = nthreads)
    save(rf, file=hs)
  }else{
    message('Loading cached random forest model.')
    load(hs)
  }
  models[['Random Forest']] <- rf
  return[['models']] <- models
  
  ### VALID
  hs <- hash(args, names(args), 'valid')
  if(isnot(hs)){
    df <- data.frame()
    for( of in names(models) ){
      scores <- scorer(learn$test$X, models[[of]])
      s <- split(scores, learn$test$Y)
      df <- rbind(df, data.frame(melt(s), f=of))
    }
    colnames(df)[2] <- 'class'
    save(df, file=hs)
  }else{
    message('Loading cached valid scores.')
    load(hs)
  }
  setwd(go_back_to)
  
  return[['validation']][['hist']] <- ggplot(df, aes(value, group=class, fill=class)) +
    geom_histogram() +
    facet_grid( ~f )
  
  D <- split(df, df$f)
  lapply(names(D), function(x){
    d <- D[[x]]
    ROC = roc(d$class,d$value)
    FPR <- 1 - ROC$specificities
    TPR <- ROC$sensitivities
    n <- length(FPR)
    return(data.frame(fpr=FPR, tpr=TPR, auc=rep(ROC$auc, n), 
                      model=as.factor(rep(x,n))))
  }) -> ll
  dd = do.call(rbind, ll)
  dd$auc = format(dd$auc, digits=3)
  
  ggplot(dd,aes(fpr,tpr,color=model))+geom_path(size = 2, alpha = 0.7)+
    labs(x = "FPR", y = "TPR") -> g
  return[['validation']][['roc']] <- g
  
  ### TEST
  hash <- digest(c('test', learn, models, args[['G']]))
  if(isnot(hash)){
    message('Model testing...')
    test <- testTumor(tcga[TCGA$tumor,], models, verbose=FALSE)
    save(test, file=hash)
  }else{
    message('Loading cached test scores.')
    load(hash)
  }
  setwd(go_back_to)
  return[['test']] <- test

  # gg + geom_violin()
  # gg$layers[[1]] <- NULL
  # gg + geom_violin(aes(y=values, x=model, fill=`Cancer group`))
  
  return(return)
}

hash <- function(args, params_names, extra='')
  digest(paste0(paste0(args[params_names],collapse=''), extra))
isnot <- function(nm) return(!(nm %in% list.files('.')))
