#' Wilcoxon Multiple Comparisions Test
#'
#' Find siginificant differences between samples from
#' G class (PCBC) and healthy tissues (TCGA) + non G (PCBC)
#' for each predictor. To get interesting signature use
#' \code{\link{signatureWMCT}} function.
#'
#' @param pcbc data set containing in the first column labels
#' given as factor. At least one level of the factor should be G. The
#' next columns are for predictors.
#' @param tcga tcga data set with healthy tissues.
#' The columns are predictors.
#' @param G label name for selected group of samples in \code{pcbc} dataset.
#' @param adjust method of correction. "none" if not interesting.
#' Another possible arguments are:
#' c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY","fdr")
#' @param njob number of clusters for parallel computing.
#' @details Two-sample Wilcoxon test is used for each of predictors
#' (for more details see: \code{\link{wilcox.test}}). As an alternative "two.sided"
#' was choosen. Note, that there are as many comparisions as many predictors
#' is on the overlap of `pcbc` and `tcga` sets. This approach requires
#' to use some of correction methods. Read more on \href{https://en.wikipedia.org/wiki/Multiple_comparisons_problem}{wikipedia}.
#' @return Two-element list. The first element is a vector of adjusted
#' p.values obtained from multple comparisons test. The second one is a vector
#' of original p-values without any correction. You can use this element
#' to apply some other corretion method. See more: \code{\link{p.adjust}}.
#' @author Katarzyna Sobiczewska
#' @examples
#' wmct <- WMCT(methPCBC, ENDOhealthy, G='SC', njob=3)
#' wmct2 <- p.adjust(wmct[[2]],'bonferroni')
#' signatureWMCT(wmct, n=10)
#' @import stats
#' @import parallel
#' @export

WMCT <- function(pcbc, tcga, G="SC", adjust = 'fdr', njob = 1){
  message(sprintf("non %s + healthy vs %s Wilcoxon Multiple Comparision test", G,G))

  # Selecting only overlaping features
  stopifnot(G%in%pcbc[[key(pcbc)]])
  stopifnot(adjust%in%c("holm", "hochberg", "hommel", "bonferroni",
                        "BH", "BY", "fdr", "none"))
  comm <- intersect(colnames(pcbc), colnames(tcga))
  
  wt <- function(feature){
    x <- list(healthy = tcga[, feature, with=FALSE],
              nonG = pcbc[!G , feature, with=FALSE ])
    x <- as.numeric(unlist(x))
    y <- unlist(pcbc[G , feature, with=FALSE])
    wt <- wilcox.test(x, y, alternative = 'two.sided', paired = FALSE)
    return(wt$p.value)
  }
  
  if( njob>1 ){
    cl <- makeCluster(njob)
    clusterExport(cl = cl, c("comm","pcbc","tcga","G","wt"), envir = environment())
    parSapply(cl, comm, wt) -> WT
    stopCluster(cl)
  } else {
    sapply(comm, wt) -> WT
  }
  WT_results <- p.adjust(WT, method = adjust)
  return(list(WT_p.adjusted = WT_results, WT_p.original = WT))
}
