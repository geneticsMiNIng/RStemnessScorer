#' Plot the Signature
#'
#' Visualize and compare distributions of selected signature between
#' different models.
#'
#' @param signatures list of signatures. One element of the list is a
#' signature from one model given as vector of characters. It is also
#' possible to pass one set signatures from a model as a single element. If
#' \code{signatures=NULL} than column names of \code{x} are used as a signature.
#' @param x dataset from which distributions should be visualised.
#' This object should contain all passed signatures.
#' @param y labels for each observations as a vector of characters or factors.
#' @param horiz logical; indicates if the boxplote should be horizontal.
#' @return \code{\link{gridExtra}} object. Red color exposes signature
#' that is common for all passed models.
#' @author Katarzyna Sobiczewska
#' @examples
#' wmct <- WMCT(methPCBC, ENDOhealthy, G='SC', njob=3)
#' sign <- signatureWMCT(wmct, n=10)
#' train <- prepareTraining(methPCBC,ENDOhealthy,G='SC',signature = sign)
#' plotSignature(sign, train$X, train$Y)
#'
#' ## much more interesting
#' measures <- c('class','auc','dev')
#' models <- lapply(measures, function(m)
#'                    buildScorer(train$X, train$Y, model="LR",
#'                                cv.measure=m, njob = 3))
#' names(models) <- measures
#' signatures <- lapply(models, function(m) names(getCoeffs(m)))
#' plotSignature(signatures, train$X, train$Y)
#' plotSignature(signatures[-2], train$X, train$Y)
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#' @importFrom gridExtra arrangeGrob
#' @export

plotSignature <- function(x, y, signatures=NULL, horiz=TRUE){
  # signatures - list of finded signatures.
  # x - data set
  # y - vector of responses
  if( is.null(signatures) ) signatures <- colnames(x)
  if( class(signatures) != 'list' ) signatures <- list(signatures)
  signatures <- signatures[!sapply(signatures,is.null)]
  common <- Reduce(intersect, signatures)
  all <- unique(unlist(signatures))
  if( !all(all[all!='(Intercept)'] %in% colnames(x)) ) 
    message('x dataset does not contain all passed signature names.')
  temp <- as.matrix(x)
  rownames(temp) <- y
  temp <- reshape2::melt(temp)
  colnames(temp) <- c("class", "Var2", "methylation")
  ###
  lapply(seq_along(signatures), function(i)
    g_signature(signatures[[i]], temp, names(signatures)[i],
                common, all, horiz)) -> ggplots

  if( length(signatures)>1 | is.null(signatures) ){
    mylegend <- g_legend(ggplots[[1]]+theme(legend.position='bottom'))
    ggplots <- lapply(ggplots, function(gg) gg + theme(legend.position='none'))
    names(ggplots) <- names(signatures)
    grobs <- paste0("ggplots[[", seq_along(signatures), "]]", collapse = ', ')
    txt <- sprintf('grid.arrange(arrangeGrob(%s, ncol=%d), mylegend, nrow=2, heights=c(10,1))', grobs, length(signatures))
    eval(parse(text=txt))
  } else {
    ggplots[[1]]
  }
}


### Funkcje pomocnicze ###
g_signature <- function(nm, temp, main, common, all, horiz){
  df <- temp[temp$Var2 %in% nm, ]
  df$class <- as.factor(df$class)
  nm <- df$Var2[!duplicated(df$Var2)]

  colIND <- (sort(nm)%in%common) & !tryCatch(all(common == all), warning = function(cond) FALSE)
  ggplot(df) +
    geom_boxplot( aes(x=Var2, y=methylation, fill=class) ) +
    ggtitle(main) +
    theme(axis.text.x = element_text(angle=90,hjust=1,vjust=0.5)) +
    theme(axis.text.y = element_text(color=ifelse(colIND, 'brown3', 'gray40'), face='bold')) +
    # "chartreuse4"
    xlab("") -> gg
  if (horiz) gg +
    # theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
    coord_flip() else gg
}

g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
