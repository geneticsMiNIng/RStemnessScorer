#' Change the Format of Genes Names
#'
#' Converting Ensembl Gene Ids to HGNC gene names.
#'
#' @param ens a vector of characters containing ensembl gene set.
#' @return a vector of characters with genes as HGNC symbols.
#' @author k
#'
#' @examples
#' Ensembl2HGNC(rownames(CountsCPM[1:1,]))
#'
#' @export
#' @import biomaRt

Ensembl2HGNC <- function(ens){
  mart<- useDataset("hsapiens_gene_ensembl", useMart("ensembl"))
  ensembl_genes<- substr(ens, 1, 15)
  nnames <- getBM(
    filters= "ensembl_gene_id",
    attributes= c("ensembl_gene_id", "hgnc_symbol", "description"),
    values= ensembl_genes,
    mart= mart)
  return(nnames)
}
