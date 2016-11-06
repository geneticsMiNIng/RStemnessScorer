#' RTCGA Normal and Tumor
#'
#' The function collects group of cancers (given in 'cancerTypes/\code{group}.txt' files)
#' Splitting RTCGA.xxx data sets into tumor and normal patients respectively to 
#' barcode information. 
#'
#' @param data indicator of a RTCGA data set (as a string).
#' @param group name of one from ecto/endo/meso vectors.
#' @return list of length 3. At the fist position there is vector of tumor samples, at the 
#' second: vector of normal samples. The last element of the list contains full data set 
#' where all samples across cancers in given \code{group} are concatenated.
#' @author Katarzyna Sobiczewska
#' @examples
#' 
#' @import RTCGA
#' @export

 
RTCGATumorNormal <- function(data, group){
  require(sprintf('RTCGA.%s', data), character.only = TRUE)
  cohort <- cohorts[[tolower(group)]]
  data.sets <- ls(sprintf("package:RTCGA.%s", data))
  data.sets <- data.sets[unlist(sapply(cohort, function(x) 
    grep( paste0(x,'\\.'), data.sets )))]

  prefix <- sapply(strsplit(data.sets, '\\.'), function(x) x[1])
  sufix <- sapply(strsplit(data.sets, '\\.'), function(x) x[2])
  ## usuwanie zlepków
  temp <- sapply( prefix, function(i) sapply(prefix, function(j) paste0(i,j) %in% prefix))
  if(any(temp)){
    ind <- !(prefix %in% prefix[which(temp, arr.ind = TRUE)])
    prefix <- prefix[ind]
    sufix <- sufix[ind]
  }
  data.sets <- paste0(prefix, '.', sufix)
  message(sprintf('Loading RTCGA.%s: %s...',
                  data,
                  paste(prefix, collapse = ', ')))
  cancer.data <- do.call(rbind, lapply(data.sets,get))
  tryCatch(expr = rm(list=ls(sprintf("package:RTCGA.%s", data))), 
           warning=function(w) invisible() )
  gc()
  message('\tSplitting into normal and tumor respectively to the barcode...')
  id_name <- colnames(cancer.data)[1]
  id_col <- as.character(cancer.data[[id_name]])
  normal <- grepl('[1-9][0-9]A', sapply( strsplit(id_col, "-"), 
                                         function(x) x[4]))
  tumor <- grepl('0[1-9]A', sapply( strsplit(id_col, "-"),
                                    function(x) x[4]))
  normal <- id_col[normal]
  tumor <- id_col[tumor]
  
  cancer.data[['cancer']] <- rep(sapply(strsplit(data.sets,"\\."), function(x) x[1]),
                         sapply(data.sets, function(x) dim(get(x))[1]))
  cancer.data <- as.data.table(cancer.data)
  setkeyv(cancer.data,id_name)
  out <- list(tumor=tumor, normal=normal, data=cancer.data)
  return(out)
}