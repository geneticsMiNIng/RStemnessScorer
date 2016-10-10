#' Get the signature from Wilcoxon Multiple Comparisions Test
#'
#' Set the cutoff on the adjusted p-values to obtain the interesting
#'  signature.
#'
#' @param wmct the result of \code{\link{WMCT}} function.
#' @param n to get a signature for n smallest adjusted p-values.
#' @param alpha to get a signature for all adjusted p-values smaller than alpha.
#' @details If both \code{n} and \code{alpha} values are passed,
#' than the \code{n} is ignored.
#' @return Vector of characters.
#' @author Katarzyna Sobiczewska
#' @examples
#' wmctENDO <- WMCT(methPCBC, ENDOhealthy, G='SC')
#' signatureWMCT(wmctENDO, n=10)
#' @export

signatureWMCT <- function(wmct, alpha=NULL, n=NULL){
  # non G + healthy vs G
  if(is.null(n)&is.null(alpha)) stop("Neither n nor alpha was passed to the function.")
  if(!"WT_p.adjusted"%in%names(wmct)) stop("Bad wmct object")
  if( !is.null(alpha) )
    if( alpha>1 | alpha<0 ) stop("Bad alpha value.")

  if(!is.null(n)){
    out <- head(sort(wmct$WT_p.adjusted),n)
    out <- names(out)
  }
  if(!is.null(alpha)){
    out <- wmct$WT_p.adjusted[wmct$WT_p.adjusted<alpha]
    out <- names(out)
  }
  return(out)
}
